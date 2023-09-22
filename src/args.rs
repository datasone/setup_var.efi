use alloc::{
    borrow::ToOwned,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::fmt::{Display, Formatter};

use uefi::{
    data_types::EqStrUntilNul,
    prelude::BootServices,
    proto::loaded_image::{LoadOptionsError, LoadedImage},
    CStr16, CString16, Char16,
};
use uefi_services::println;

use crate::utils::CStr16Ext;

#[derive(Debug)]
pub enum ParseError {
    LoadedImageProtocolError,
    LoadOptionsError(LoadOptionsError),
    InvalidValue(String),
    NumberTooLarge(String),
    InvalidArgs(ArgsError),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::LoadedImageProtocolError => {
                write!(f, "Error while initializing UEFI LoadedImage protocol")
            }
            Self::LoadOptionsError(e) => {
                write!(f, "Error loading options: {e:?}")
            }
            Self::InvalidValue(s) => {
                write!(f, "Unexpected value: {s}")
            }
            Self::NumberTooLarge(s) => {
                write!(f, "Specified number {s} is too large (larger than 64-bit)")
            }
            Self::InvalidArgs(e) => {
                write!(f, "{e}")
            }
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ValueAddr {
    pub var_name: CString16,
    pub var_id:   Option<usize>,
    pub offset:   usize,
}

#[derive(Debug, Default, Clone)]
pub struct ValueOperation {
    pub val_size: usize,
    pub op_type:  ValueOperationType,
}

#[derive(Debug, Default, Clone)]
pub enum ValueOperationType {
    #[default]
    Read,
    Write(usize),
}

impl ValueOperation {
    pub fn validate(&self) -> Result<(), ArgsError> {
        if let ValueOperationType::Write(val) = self.op_type {
            if val > (1 << (self.val_size * 8)) {
                Err(ArgsError::ValLargerThanSize(val, self.val_size))
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ValueArg {
    pub addr:      ValueAddr,
    pub operation: ValueOperation,
}

#[derive(Debug)]
enum NamedArg {
    Help,
    Reboot,
    WriteOnDemand,
}

#[derive(Debug)]
enum Arg {
    Named(NamedArg),
    Value(ValueArg),
}

#[derive(Debug, Default)]
pub struct Args {
    pub value_args:      Vec<ValueArg>,
    pub help_msg:        bool,
    pub write_on_demand: bool,
    pub reboot:          bool,
}

impl Args {
    fn validate(&self) -> Result<(), ArgsError> {
        if self.help_msg {
            Ok(())
        } else {
            self.value_args
                .iter()
                .try_for_each(|va| va.operation.validate())
        }
    }
}

#[derive(Debug)]
pub enum ArgsError {
    ValLargerThanSize(usize, usize),
}

impl Display for ArgsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::ValLargerThanSize(value, size) => {
                write!(
                    f,
                    "Specified value to write (0x{:0width$X}) is larger than specified size \
                     {size} bytes",
                    value,
                    width = size * 2,
                )
            }
        }
    }
}

impl From<ArgsError> for ParseError {
    fn from(value: ArgsError) -> Self {
        Self::InvalidArgs(value)
    }
}

pub const HELP_MSG: &str = r#"Usage:
setup_var.efi <OFFSET> [<VALUE>] [-s <VALUE_SIZE>] [-n <VAR_NAME>] [-i <VAR_ID>] [-r/--reboot] [--write_on_demand]

OFFSET: The offset of value to be altered in the UEFI variable.
VALUE: The new value to write, capped at 64-bit. If not specified, the value at OFFSET will be read and shown.
VALUE_SIZE: Bytes of value to write, must be equal or larger than the size of <VALUE>, defaults to 0x1.
VAR_NAME: The name of UEFI variable to be altered, defaults to "Setup".
VAR_ID: Unique id for distinguishing variables with same name, which will be provided by setup_var.efi (when required).
-r or --reboot: Reboot (warm reset) the computer after the program successfully finishes.
--write_on_demand: If the value desired to be written is the same with storage, skip the unnecessary write.

OFFSET, VALUE, VALUE_SIZE and VAR_ID are numbers, and must be specified in hexadecimal with prefix "0x".

The program defaults to little endian for values ONLY while operating UEFI variables,
though it's recommended to only operate on one byte if you are not sure what this is or means.

Example: .\setup_var.efi 0x10E 0x1a -n CpuSetup"#;

pub fn parse_args(boot_services: &BootServices) -> Result<Args, ParseError> {
    let loaded_image = boot_services
        .open_protocol_exclusive::<LoadedImage>(boot_services.image_handle())
        .map_err(|_| ParseError::LoadedImageProtocolError)?;

    let options = loaded_image
        .load_options_as_cstr16()
        .map_err(ParseError::LoadOptionsError)?;

    parse_args_from_str(options)
}

fn drop_first_arg(arg: &CStr16) -> bool {
    let rev_efi_ext: [Char16; 5] = [
        '.'.try_into().unwrap(),
        'e'.try_into().unwrap(),
        'f'.try_into().unwrap(),
        'i'.try_into().unwrap(),
        '\0'.try_into().unwrap(),
    ];
    let arg_chars = arg.as_slice_with_nul();
    let has_efi_ext = if arg_chars.len() < 5 {
        false
    } else {
        let arg_last_chars = &arg_chars[arg_chars.len() - 5..arg_chars.len()];
        arg_last_chars == rev_efi_ext
    };

    #[allow(clippy::needless_bool)]
    if has_efi_ext {
        true // efi extension, treated as the name of the executable file
    } else if starts_with(arg, '-') {
        false // Highly probably an option
    } else {
        // Now that we can't decide what is the first arg as they starts with the var
        // name, defaults to keep it
        false
    }
}

macro_rules! try_parsers {
    ($input:expr, $parser:ident) => {{
        $parser($input)
    }};

    ($input:expr, $parser:ident, $( $parsers:ident ),*) => {{
        if let Ok(val) = $parser($input) {
            Ok(val)
        } else {
            try_parsers!($input, $($parsers),*)
        }
    }};
}

fn parse_args_from_str(options: &CStr16) -> Result<Args, ParseError> {
    let options = options.split_to_cstring(' '.try_into().unwrap());
    let opt_len = options.len();

    if opt_len == 0 {
        return Ok(Args {
            help_msg: true,
            ..Default::default()
        });
    }

    let first_arg = options[0].clone();
    let mut option_iter = options.into_iter();
    if drop_first_arg(&first_arg) {
        if opt_len == 1 {
            return Ok(Args {
                help_msg: true,
                ..Default::default()
            });
        }
        option_iter.next(); // Skips executable argv
    }

    let args = option_iter
        .filter(|s| s.as_slice_with_nul().len() != 1)
        .map(|s| {
            try_parsers!(&s, parse_named_arg, parse_value_arg)
                .map_err(|e| ParseError::InvalidValue(format!("{s}: {e}")))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let named_args = args
        .iter()
        .filter(|arg| matches!(arg, Arg::Named(_)))
        .map(|arg| {
            if let Arg::Named(arg) = arg {
                arg
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<_>>();
    let value_args = args
        .iter()
        .filter(|arg| matches!(arg, Arg::Value(_)))
        .map(|arg| {
            if let Arg::Value(arg) = arg {
                arg.clone()
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<_>>();

    let mut args = Args::default();
    for named_arg in named_args {
        match named_arg {
            NamedArg::Help => args.help_msg = true,
            NamedArg::Reboot => args.reboot = true,
            NamedArg::WriteOnDemand => args.write_on_demand = true,
        }
    }

    args.value_args = value_args;

    args.validate()?;
    Ok(args)
}

fn starts_with(s: &CStr16, c: char) -> bool {
    match s.as_slice_with_nul().first() {
        None => false,
        Some(&c_h) => c_h == c.try_into().unwrap(),
    }
}

fn try_next_char(iter: &mut impl Iterator<Item = char>, str: &CStr16) -> Result<char, ParseError> {
    iter.next()
        .ok_or_else(|| ParseError::InvalidValue(str.to_string()))
}

fn parse_hex_number(num_str: &CStr16) -> Result<usize, ParseError> {
    let mut str_iter = num_str.iter().map(|&c| char::from(c));

    // Prefix 0x(0X)
    let c_h = try_next_char(&mut str_iter, num_str)?;
    let c2_h = try_next_char(&mut str_iter, num_str)?;

    match (c_h, c2_h) {
        ('0', 'x') => {}
        ('0', 'X') => {}
        _ => return Err(ParseError::InvalidValue(num_str.to_string())),
    }

    if num_str.num_bytes() > 2 * (18 + 1) {
        return Err(ParseError::NumberTooLarge(num_str.to_string()));
    }

    let value = str_iter
        .map(|c| c.to_digit(16).map(|n| n as u8))
        .collect::<Option<Vec<u8>>>()
        .ok_or_else(|| ParseError::InvalidValue(num_str.to_string()))?;
    let value_len = value.len();
    let value = value.iter().enumerate().fold(0usize, |acc, (i, &n)| {
        acc + ((n as usize) << (4 * (value_len - i - 1)))
    });

    Ok(value)
}

fn parse_number(num_str: &CStr16) -> Result<usize, ParseError> {
    let chars = num_str.iter().map(|&c| char::from(c)).collect::<Vec<_>>();
    if chars.iter().any(|c| !c.is_ascii_digit()) {
        Err(ParseError::InvalidValue(num_str.to_string()))?
    }

    let value = chars
        .into_iter()
        .fold(0usize, |acc, n| acc * 10 + (n as u8 - b'0') as usize);

    Ok(value)
}

fn parse_named_arg(key: &CStr16) -> Result<Arg, ParseError> {
    if key.eq_str_until_nul(&"-h") || key.eq_str_until_nul(&"--help") {
        Ok(Arg::Named(NamedArg::Help))
    } else if key.eq_str_until_nul(&"-r") || key.eq_str_until_nul(&"--reboot") {
        Ok(Arg::Named(NamedArg::Reboot))
    } else if key.eq_str_until_nul(&"--write_on_demand") {
        Ok(Arg::Named(NamedArg::WriteOnDemand))
    } else {
        Err(ParseError::InvalidValue(key.to_string()))
    }
}

fn parse_value_arg(arg: &CStr16) -> Result<Arg, ParseError> {
    let mut arg_split = arg.split_to_cstring(':'.try_into().unwrap());
    if arg_split.len() != 2 {
        Err(ParseError::InvalidValue(arg.to_string()))?
    }

    let mut var_name = arg_split.swap_remove(0);
    let mut var_id = None;

    if var_name.contains('('.try_into().unwrap()) {
        let mut arg_split = var_name.split_to_cstring('('.try_into().unwrap());
        if arg_split.len() != 2 {
            Err(ParseError::InvalidValue(var_name.to_string()))?
        }
        let var_id_str = arg_split[1]
            .strip_suffix(')'.try_into().unwrap())
            .ok_or(ParseError::InvalidValue(var_name.to_string()))?;

        var_id = Some(try_parsers!(&var_id_str, parse_number, parse_hex_number)?);
        var_name = arg_split.swap_remove(0);
    }

    let mut op_type = ValueOperationType::Read;
    let mut var_offset = arg_split.swap_remove(0);
    let mut val_size = 1;

    if var_offset.contains('='.try_into().unwrap()) {
        let mut arg_split = var_offset.split_to_cstring('='.try_into().unwrap());
        if arg_split.len() != 2 {
            Err(ParseError::InvalidValue(var_offset.to_string()))?
        }
        let value = try_parsers!(&arg_split[1], parse_hex_number, parse_number)?;

        op_type = ValueOperationType::Write(value);
        var_offset = arg_split.swap_remove(0);
    }

    if var_offset.contains('('.try_into().unwrap()) {
        let mut arg_split = var_offset.split_to_cstring('('.try_into().unwrap());
        if arg_split.len() != 2 {
            Err(ParseError::InvalidValue(var_offset.to_string()))?
        }
        let val_size_str = arg_split[1]
            .strip_suffix(')'.try_into().unwrap())
            .ok_or(ParseError::InvalidValue(var_offset.to_string()))?;

        val_size = try_parsers!(&val_size_str, parse_number, parse_hex_number)?;
        var_offset = arg_split.swap_remove(0);
    }

    let offset = try_parsers!(&var_offset, parse_hex_number, parse_number)?;

    Ok(Arg::Value(ValueArg {
        addr:      ValueAddr {
            var_name,
            var_id,
            offset,
        },
        operation: ValueOperation { val_size, op_type },
    }))
}

#[allow(dead_code)]
pub(crate) fn test_functions() {
    println!("Testing parse_number");
    println!(
        r#"parse_number("0x1"), should be Ok({}), result is {:?}"#,
        0x1,
        parse_number(&CString16::try_from("0x1").unwrap())
    );
    println!(
        r#"parse_number("0x01"), should be Ok({}), result is {:?}"#,
        0x1,
        parse_number(&CString16::try_from("0x01").unwrap())
    );
    println!(
        r#"parse_number("0X12e"), should be Ok({}), result is {:?}"#,
        0x12E,
        parse_number(&CString16::try_from("0X12e").unwrap())
    );
    println!(
        r#"parse_number("0x12eFAb"), should be Ok({}), result is {:?}"#,
        0x12EFAB,
        parse_number(&CString16::try_from("0x12eFAb").unwrap())
    );
    println!(
        r#"parse_number("0xDEADBEEFDEADBEEF01"), should be Err({}), result is {:?}"#,
        ParseError::NumberTooLarge("0xDEADBEEFDEADBEEF01".to_owned()),
        parse_number(&CString16::try_from("0xDEADBEEFDEADBEEF01").unwrap())
    );
    println!(
        r#"parse_number("0x12eFAs"), should be Err({:?}), result is {:?}"#,
        ParseError::InvalidValue("0x12eFAs".to_owned()),
        parse_number(&CString16::try_from("0x12eFAs").unwrap())
    );
    println!(
        r#"parse_number("abc"), should be Err({:?}), result is {:?}"#,
        ParseError::InvalidValue("abc".to_owned()),
        parse_number(&CString16::try_from("abc").unwrap())
    );

    println!("Testing parse_named_arg");
    println!(
        r#"parse_named_arg("-h", None), should be Ok({:?}), result is {:?}"#,
        NamedArg::Help,
        parse_named_arg(
            &CString16::try_from("-h").unwrap(),
            &mut core::iter::empty::<CString16>()
        )
    );
    println!(
        r#"parse_named_arg("-r", None), should be Ok({:?}), result is {:?}"#,
        NamedArg::Reboot,
        parse_named_arg(
            &CString16::try_from("-r").unwrap(),
            &mut core::iter::empty::<CString16>()
        )
    );
    println!(
        r#"parse_named_arg("--reboot", None), should be Ok({:?}), result is {:?}"#,
        NamedArg::Reboot,
        parse_named_arg(
            &CString16::try_from("--reboot").unwrap(),
            &mut core::iter::empty::<CString16>()
        )
    );
    println!(
        r#"parse_named_arg("--write_on_demand", None), should be Ok({:?}), result is {:?}"#,
        NamedArg::WriteOnDemand,
        parse_named_arg(
            &CString16::try_from("--write_on_demand").unwrap(),
            &mut core::iter::empty::<CString16>()
        )
    );
    println!(
        r#"parse_named_arg("--help", Some("abc")), should be Ok({:?}), result is {:?}"#,
        NamedArg::Help,
        parse_named_arg(
            &CString16::try_from("--help").unwrap(),
            &mut [CString16::try_from("abc").unwrap()].into_iter()
        )
    );
    println!(
        r#"parse_named_arg("-s", Some("0x12e")), should be Ok({:?}), result is {:?}"#,
        NamedArg::ValueSize(0x12e),
        parse_named_arg(
            &CString16::try_from("-s").unwrap(),
            &mut [CString16::try_from("0x12e").unwrap()].into_iter()
        )
    );
    println!(
        r#"parse_named_arg("-s", Some("CpuSetup")), should be Err({:?}), result is {:?}"#,
        ParseError::InvalidValue("CpuSetup".to_owned()),
        parse_named_arg(
            &CString16::try_from("-s").unwrap(),
            &mut [CString16::try_from("CpuSetup").unwrap()].into_iter()
        )
    );
    println!(
        r#"parse_named_arg("-i", Some("0x1")), should be Ok({:?}), result is {:?}"#,
        NamedArg::VariableId(0x1),
        parse_named_arg(
            &CString16::try_from("-i").unwrap(),
            &mut [CString16::try_from("0x1").unwrap()].into_iter()
        )
    );
    println!(
        r#"parse_named_arg("-i", Some("CpuSetup")), should be Err({:?}), result is {:?}"#,
        ParseError::InvalidValue("CpuSetup".to_owned()),
        parse_named_arg(
            &CString16::try_from("-i").unwrap(),
            &mut [CString16::try_from("CpuSetup").unwrap()].into_iter()
        )
    );
    println!(
        r#"parse_named_arg("-n", Some("CpuSetup")), should be Ok({:?}), result is {:?}"#,
        NamedArg::CustomName(CString16::try_from("CpuSetup").unwrap()),
        parse_named_arg(
            &CString16::try_from("-n").unwrap(),
            &mut [CString16::try_from("CpuSetup").unwrap()].into_iter()
        )
    );
    println!(
        r#"parse_named_arg("0x13", Some("CpuSetup")), should be Err({:?}), result is {:?}"#,
        ParseError::InvalidValue("0x13".to_owned()),
        parse_named_arg(
            &CString16::try_from("0x13").unwrap(),
            &mut [CString16::try_from("CpuSetup").unwrap()].into_iter()
        )
    );
    println!(
        r#"parse_named_arg("-n", None), should be Err({:?}), result is {:?}"#,
        ParseError::OptionNoValue("-n".to_owned()),
        parse_named_arg(
            &CString16::try_from("-n").unwrap(),
            &mut core::iter::empty::<CString16>()
        )
    );

    println!("Testing parse_args_from_str");
    println!(
        r#"parse_args_from_str(""), should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            ..Default::default()
        },
        parse_args_from_str(&CString16::try_from(".\\setup_var.efi").unwrap())
    );
    println!(
        r#"parse_args_from_str("-h"), should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            ..Default::default()
        },
        parse_args_from_str(&CString16::try_from(".\\setup_var.efi -h").unwrap())
    );
    println!(
        r#"parse_args_from_str("0x12 -n VAR -i 0x1 -h"), should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            offset: Some(0x12),
            var_id: Some(0x1),
            ..Default::default()
        },
        parse_args_from_str(
            &CString16::try_from(".\\setup_var.efi 0x12 -n VAR -i 0x1 -h").unwrap()
        )
    );
    println!(
        r#"parse_args_from_str("0x12 -n -h"), should be Ok({:?}), result is {:?}"#,
        Args {
            var_name: Some(CString16::try_from("-h").unwrap()),
            offset: Some(0x12),
            ..Default::default()
        },
        parse_args_from_str(&CString16::try_from(".\\setup_var.efi 0x12 -n -h").unwrap())
    );
    println!(
        r#"parse_args_from_str("0x12 0x12 --write_on_demand -n VAR -h"), should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            offset: Some(0x12),
            value: Some(0x12),
            write_on_demand: true,
            ..Default::default()
        },
        parse_args_from_str(
            &CString16::try_from(".\\setup_var.efi 0x12 0x12 --write_on_demand -n VAR -h").unwrap()
        )
    );
    println!(
        r#"parse_args_from_str("0x12 0x12e -n VAR"), should be Err({:?}), result is {:?}"#,
        ParseError::InvalidArgs(ArgsError::ValLargerThanSize(0x12e, 1)),
        parse_args_from_str(&CString16::try_from(".\\setup_var.efi 0x12 0x12e -n VAR").unwrap())
    );
    println!(
        r#"parse_args_from_str("0x12 0x12e -n"), should be Err({:?}), result is {:?}"#,
        ParseError::OptionNoValue("-n".to_owned()),
        parse_args_from_str(&CString16::try_from(".\\setup_var.efi 0x12 0x12e -n").unwrap())
    );
    println!(
        r#"parse_args_from_str("-s 0x01 -n VAR"), should be Err({:?}), result is {:?}"#,
        ParseError::InvalidArgs(ArgsError::MissingOffset),
        parse_args_from_str(&CString16::try_from(".\\setup_var.efi -s 0x01 -n VAR").unwrap())
    );
    println!(
        r#"parse_args_from_str("-s 0x01 -n VAR -h --write_on_demand"), should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            val_size: Some(0x1),
            write_on_demand: true,
            ..Default::default()
        },
        parse_args_from_str(
            &CString16::try_from(".\\setup_var.efi -s 0x01 -n VAR -h --write_on_demand").unwrap()
        )
    );
    println!(
        r#"parse_args_from_str("0x12 0x12e -n VAR -n VAR1"), should be Err({:?}), result is {:?}"#,
        ParseError::AppliedMultipleTimes("-n".to_owned()),
        parse_args_from_str(
            &CString16::try_from(".\\setup_var.efi 0x12 0x12e -n VAR -n VAR1").unwrap()
        )
    );
    println!(
        r#"parse_args_from_str("0x01 -n VAR -h --write_on_demand"), w/o exe name, should be Ok({:?}), result is {:?}"#,
        Args {
            offset: Some(0x1),
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            write_on_demand: true,
            ..Default::default()
        },
        parse_args_from_str(&CString16::try_from("0x01 -n VAR -h --write_on_demand").unwrap())
    );
    println!(
        r#"parse_args_from_str("-s 0x01 -n VAR -h --write_on_demand"), w/o exe name, should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            val_size: Some(0x1),
            write_on_demand: true,
            ..Default::default()
        },
        parse_args_from_str(&CString16::try_from("-s 0x01 -n VAR -h --write_on_demand").unwrap())
    );
    println!(
        r#"parse_args_from_str("-s 0x01 -n VAR -h --write_on_demand"), w/ ambiguous exe name, should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            val_size: Some(0x1),
            write_on_demand: true,
            ..Default::default()
        },
        parse_args_from_str(
            &CString16::try_from("0x14.efi -s 0x01 -n VAR -h --write_on_demand").unwrap()
        )
    );
    println!(
        r#"parse_args_from_str("-s 0x01 -n VAR -h --write_on_demand"), w/ambiguous exe name, should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            val_size: Some(0x1),
            write_on_demand: true,
            ..Default::default()
        },
        parse_args_from_str(
            &CString16::try_from("-s.efi -s 0x01 -n VAR -h --write_on_demand").unwrap()
        )
    );
}
