use alloc::{
    borrow::ToOwned,
    string::{String, ToString},
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
use NamedArg::*;
use ParseError::*;

use crate::utils::cstr16_to_cstring16;

#[derive(Debug)]
pub(crate) enum ParseError {
    LoadedImageProtocolError,
    LoadOptionsError(LoadOptionsError),
    InvalidValue(String),
    NumberTooLarge(String),
    AppliedMultipleTimes(String),
    OptionNoValue(String),
    InvalidArgs(ArgsError),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            LoadedImageProtocolError => {
                write!(f, "Error while initializing UEFI LoadedImage protocol")
            }
            LoadOptionsError(e) => {
                write!(f, "Error loading options: {e:?}")
            }
            InvalidValue(s) => {
                write!(f, "Unexpected value: {s}")
            }
            NumberTooLarge(s) => {
                write!(f, "Specified number {s} is too large (larger than 64-bit)")
            }
            AppliedMultipleTimes(s) => {
                write!(f, "Argument {s} is applied multiple times, once expected")
            }
            OptionNoValue(s) => {
                write!(f, "Argument {s} has no value specified")
            }
            InvalidArgs(e) => {
                write!(f, "{e}")
            }
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct Args {
    pub(crate) help_msg: bool,
    pub(crate) offset:   Option<usize>,
    pub(crate) value:    Option<usize>,
    pub(crate) val_size: Option<usize>,
    pub(crate) var_name: Option<CString16>,
    pub(crate) var_id:   Option<usize>,
}

impl Args {
    fn validate(&self) -> Result<(), ArgsError> {
        if self.help_msg {
            Ok(())
        } else if self.offset.is_none() {
            Err(ArgsError::MissingOffset)
        } else if let Some(&value) = self.value.as_ref() {
            let val_size = if let Some(&vs) = self.val_size.as_ref() {
                vs
            } else {
                1
            };
            if value > (1 << (val_size * 8)) {
                Err(ArgsError::ValLargerThanSize(value, val_size))
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub(crate) enum ArgsError {
    MissingOffset,
    ValLargerThanSize(usize, usize),
}

impl Display for ArgsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::MissingOffset => {
                write!(f, "Offset is not provided")
            }
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

#[derive(Debug)]
enum NamedArg {
    ValueSize(usize),
    CustomName(CString16),
    VariableId(usize),
    Help,
}

pub(crate) const HELP_MSG: &str = r#"Usage:
setup_var.efi <OFFSET> [<VALUE>] [-s <VALUE_SIZE>] [-n <VAR_NAME>] [-i <VAR_ID>]

OFFSET: The offset of value to be altered in the UEFI variable.
VALUE: The new value to write, capped at 64-bit. If not specified, the value at OFFSET will be read and shown.
VALUE_SIZE: Bytes of value to write, must be equal or larger than the size of <VALUE>, defaults to 0x1.
VAR_NAME: The name of UEFI variable to be altered, defaults to "Setup".
VAR_ID: Unique id for distinguishing variables with same name, which will be provided by setup_var.efi (when required).

OFFSET, VALUE, VALUE_SIZE and VAR_ID are numbers, and must be specified in hexadecimal with prefix "0x".

The program defaults to little endian for values ONLY while operating UEFI variables,
though it's recommended to only operate on one byte if you are not sure what this is or means.

Example: .\setup_var.efi 0x10E 0x1a -n CpuSetup"#;

pub(crate) fn parse_args(boot_services: &BootServices) -> Result<Args, ParseError> {
    let loaded_image = boot_services
        .open_protocol_exclusive::<LoadedImage>(boot_services.image_handle())
        .map_err(|_| ParseError::LoadedImageProtocolError)?;

    let options = loaded_image
        .load_options_as_cstr16()
        .map_err(ParseError::LoadOptionsError)?;

    parse_args_from_str(options)
}

fn parse_args_from_str(options: &CStr16) -> Result<Args, ParseError> {
    let options = split_cstr16(options, ' '.try_into().unwrap());

    if options.len() == 1 {
        return Ok(Args {
            help_msg: true,
            ..Default::default()
        });
    }

    let mut args = Args::default();

    let mut option_iter = options.into_iter();
    option_iter.next(); // Skips executable argv
    while let Some(option) = option_iter.next() {
        if option.as_slice_with_nul().len() == 1 {
            continue; // empty string
        }
        if starts_with(&option, '-') {
            match parse_named_arg(&option, &mut option_iter)? {
                Help => {
                    args.help_msg = true;
                }
                CustomName(name) => {
                    if args.var_name.is_none() {
                        args.var_name = Some(name)
                    } else {
                        return Err(AppliedMultipleTimes(option.to_string()));
                    }
                }
                ValueSize(size) => {
                    if args.val_size.is_none() {
                        args.val_size = Some(size)
                    } else {
                        return Err(AppliedMultipleTimes(option.to_string()));
                    }
                }
                VariableId(id) => {
                    if args.var_id.is_none() {
                        args.var_id = Some(id)
                    } else {
                        return Err(AppliedMultipleTimes(option.to_string()));
                    }
                }
            }
        } else if let Ok(num) = parse_number(&option) {
            if args.offset.is_none() {
                args.offset = Some(num)
            } else if args.value.is_none() {
                args.value = Some(num)
            } else {
                return Err(InvalidValue(option.to_string()));
            }
        } else {
            return Err(InvalidValue(option.to_string()));
        }
    }

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
    iter.next().ok_or_else(|| InvalidValue(str.to_string()))
}

fn parse_number(num_str: &CStr16) -> Result<usize, ParseError> {
    let mut str_iter = num_str.iter().map(|&c| char::from(c));

    // Prefix 0x(0X)
    let c_h = try_next_char(&mut str_iter, num_str)?;
    let c2_h = try_next_char(&mut str_iter, num_str)?;

    match (c_h, c2_h) {
        ('0', 'x') => {}
        ('0', 'X') => {}
        _ => return Err(InvalidValue(num_str.to_string())),
    }

    if num_str.num_bytes() > 2 * (18 + 1) {
        return Err(NumberTooLarge(num_str.to_string()));
    }

    let value = str_iter
        .map(|c| c.to_digit(16).map(|n| n as u8))
        .collect::<Option<Vec<u8>>>()
        .ok_or_else(|| InvalidValue(num_str.to_string()))?;
    let value_len = value.len();
    let value = value.iter().enumerate().fold(0usize, |acc, (i, &n)| {
        acc + ((n as usize) << (4 * (value_len - i - 1)))
    });

    Ok(value)
}

fn parse_named_arg(
    key: &CStr16,
    opts: &mut impl Iterator<Item = CString16>,
) -> Result<NamedArg, ParseError> {
    if key.eq_str_until_nul(&"-h") || key.eq_str_until_nul(&"--help") {
        return Ok(Help);
    } else if key.eq_str_until_nul(&"--write_on_demand") {
        return Ok(WriteOnDemand);
    }

    let value = opts.next();
    let value = value.as_ref().ok_or_else(|| OptionNoValue(key.to_string()));

    if key.eq_str_until_nul(&"-s") {
        Ok(ValueSize(parse_number(value?)?))
    } else if key.eq_str_until_nul(&"-n") {
        Ok(CustomName(cstr16_to_cstring16(value?)))
    } else if key.eq_str_until_nul(&"-i") {
        Ok(VariableId(parse_number(value?)?))
    } else {
        Err(InvalidValue(key.to_string()))
    }
}

fn split_cstr16(s: &CStr16, split_char: Char16) -> Vec<CString16> {
    let mut split_strings = Vec::new();
    let mut current_string = Vec::new();

    for c in s.iter() {
        if *c != split_char {
            current_string.push(u16::from(*c))
        } else {
            current_string.push(0u16);
            split_strings.push(
                current_string
                    .try_into()
                    .expect("Invalid bytes in arguments"),
            );
            current_string = Vec::new()
        }
    }

    if !current_string.is_empty() {
        current_string.push(0u16);
        split_strings.push(
            current_string
                .try_into()
                .expect("Invalid bytes in arguments"),
        )
    }

    split_strings
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
        r#"parse_args_from_str("0x12 0x12 -n VAR -h"), should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            offset: Some(0x12),
            value: Some(0x12),
            ..Default::default()
        },
        parse_args_from_str(&CString16::try_from(".\\setup_var.efi 0x12 0x12 -n VAR -h").unwrap())
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
        r#"parse_args_from_str("-s 0x01 -n VAR -h"), should be Ok({:?}), result is {:?}"#,
        Args {
            help_msg: true,
            var_name: Some(CString16::try_from("VAR").unwrap()),
            val_size: Some(0x1),
            ..Default::default()
        },
        parse_args_from_str(&CString16::try_from(".\\setup_var.efi -s 0x01 -n VAR -h").unwrap())
    );
    println!(
        r#"parse_args_from_str("0x12 0x12e -n VAR -n VAR1"), should be Err({:?}), result is {:?}"#,
        ParseError::AppliedMultipleTimes("-n".to_owned()),
        parse_args_from_str(
            &CString16::try_from(".\\setup_var.efi 0x12 0x12e -n VAR -n VAR1").unwrap()
        )
    );
}
