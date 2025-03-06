use alloc::{
    borrow::{Cow, ToOwned},
    string::ToString,
    vec::Vec,
};

use uefi::{
    CStr16, CString16,
    data_types::EqStrUntilNul,
    prelude::*,
    proto::console::text::{Input, Key},
};

use crate::{
    args::{Args, NamedArg, ParseError, RebootMode, ValueAddr, ValueArg, ValueOperation},
    utils::CStr16Ext,
};

fn read_to_string(stdin: &mut Input) -> uefi::Result<CString16> {
    let mut str = CString16::new();

    while let Some(key) = stdin.read_key()? {
        if let Key::Printable(char) = key {
            str.push(char)
        }
    }

    Ok(str)
}

pub fn parse_input(system_table: &mut SystemTable<Boot>) -> Result<Args, ParseError> {
    let content = read_to_string(system_table.stdin()).map_err(|_| ParseError::LoadStdInError)?;
    if content.is_empty() {
        return Err(ParseError::NoInput);
    }
    let content = content
        .strip_prefix('\u{feff}'.try_into().unwrap())
        .unwrap_or(&content);

    let lines = content.split_to_cstring('\n'.try_into().unwrap());
    let lines = lines.into_iter().filter_map(|s| {
        let s = s.trim();
        if s.starts_with('#'.try_into().unwrap()) {
            return None;
        }

        let res = s.strip_suffix('\r'.try_into().unwrap()).unwrap_or(s);
        if res.is_empty() { None } else { Some(res) }
    });

    let entries = lines
        .map(|s| {
            crate::args::try_parsers!(
                &s,
                parser_with_comment(parse_named_arg),
                parser_with_comment(parse_address_def),
                parser_with_comment(parse_value_arg_wrapped),
                parser_with_comment(parse_address_ref),
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    let address_defs = entries
        .iter()
        .filter(|e| matches!(e, FileInputEntry::AddressDef { .. }))
        .map(|e| e.as_address_def())
        .collect::<Vec<_>>();
    let mut val_args = entries
        .iter()
        .filter(|e| matches!(e, FileInputEntry::ValueArg(_)))
        .map(|e| e.as_value_arg())
        .cloned()
        .collect::<Vec<_>>();

    let address_refs = entries
        .iter()
        .filter(|e| matches!(e, FileInputEntry::AddressRef { .. }));
    let mut address_ref_args = address_refs
        .map(|e| match e {
            FileInputEntry::AddressRef { name, operation } => {
                let (_, addr) = address_defs
                    .iter()
                    .find(|(def_name, _)| name == *def_name)
                    .ok_or_else(|| ParseError::InvalidValue(name.to_string()))?;
                Ok(ValueArg {
                    addr:      (*addr).clone(),
                    operation: *operation,
                })
            }
            _ => unreachable!(),
        })
        .collect::<Result<Vec<_>, ParseError>>()?;

    val_args.append(&mut address_ref_args);

    // Here we choose to let the last occurred setting override previous ones
    let reboot_entry = entries
        .iter()
        .filter(|e| matches!(e, FileInputEntry::NamedArg(NamedArg::Reboot(_))))
        .last();
    let reboot = match reboot_entry {
        Some(FileInputEntry::NamedArg(NamedArg::Reboot(mode))) => *mode,
        _ => RebootMode::Never,
    };

    let write_on_demand = entries
        .iter()
        .any(|e| matches!(e, FileInputEntry::NamedArg(NamedArg::WriteOnDemand)));

    Ok(Args {
        value_args: val_args,
        help_msg: false,
        write_on_demand,
        reboot,
    })
}

enum FileInputEntry {
    AddressDef {
        name: CString16,
        addr: ValueAddr,
    },
    ValueArg(ValueArg),
    AddressRef {
        name:      CString16,
        operation: ValueOperation,
    },
    NamedArg(NamedArg),
}

impl FileInputEntry {
    fn as_address_def(&self) -> (&CString16, &ValueAddr) {
        match self {
            Self::AddressDef { name, addr } => (name, addr),
            _ => panic!("Invalid variant used on into_address_def"),
        }
    }

    fn as_value_arg(&self) -> &ValueArg {
        match self {
            Self::ValueArg(val_arg) => val_arg,
            _ => panic!("Invalid variant used on into_value_arg"),
        }
    }
}

fn parser_with_comment<T>(parser: T) -> impl Fn(&CStr16) -> Result<FileInputEntry, ParseError>
where
    T: Fn(&CStr16) -> Result<FileInputEntry, ParseError>,
{
    move |arg| {
        let arg = if let Some((arg, _comment)) = arg.split_only_first('#'.try_into().unwrap()) {
            Cow::Owned(arg.trim())
        } else {
            Cow::Borrowed(arg)
        };

        parser(&arg)
    }
}

fn parse_named_arg(arg: &CStr16) -> Result<FileInputEntry, ParseError> {
    let named_arg = arg
        .strip_prefix('@'.try_into().unwrap())
        .ok_or_else(|| ParseError::InvalidValue(arg.to_string()))?;

    if named_arg.eq_str_until_nul("reboot") {
        Ok(FileInputEntry::NamedArg(NamedArg::Reboot(
            RebootMode::Always,
        )))
    } else if named_arg.eq_str_until_nul("reboot=auto") {
        Ok(FileInputEntry::NamedArg(NamedArg::Reboot(RebootMode::Auto)))
    } else if named_arg.eq_str_until_nul("write_on_demand") {
        Ok(FileInputEntry::NamedArg(NamedArg::WriteOnDemand))
    } else {
        Err(ParseError::InvalidValue(arg.to_string()))
    }
}

fn parse_address_def(arg: &CStr16) -> Result<FileInputEntry, ParseError> {
    let (addr_name, addr_def) = arg
        .split_only_first('='.try_into().unwrap())
        .ok_or_else(|| ParseError::InvalidValue(arg.to_string()))?;

    let addr_name = addr_name
        .strip_suffix(':'.try_into().unwrap())
        .ok_or_else(|| ParseError::InvalidValue(arg.to_string()))?;
    let val_arg = crate::args::parse_value_arg(&addr_def)?;

    if let ValueOperation::Write(_) = val_arg.operation {
        Err(ParseError::AddrDefWrite(addr_def.to_string()))
    } else {
        Ok(FileInputEntry::AddressDef {
            name: addr_name,
            addr: val_arg.addr,
        })
    }
}

fn parse_value_arg_wrapped(arg: &CStr16) -> Result<FileInputEntry, ParseError> {
    Ok(FileInputEntry::ValueArg(crate::args::parse_value_arg(arg)?))
}

fn parse_address_ref(arg: &CStr16) -> Result<FileInputEntry, ParseError> {
    let (addr_name, _value_str) = arg
        .split_only_first('='.try_into().unwrap())
        .ok_or_else(|| ParseError::InvalidValue(arg.to_string()))?;

    let addr_name = if addr_name.starts_with('$'.try_into().unwrap()) {
        addr_name
            .strip_prefix('$'.try_into().unwrap())
            .ok_or_else(|| ParseError::InvalidValue(arg.to_string()))?
    } else {
        Err(ParseError::InvalidValue(addr_name.to_string()))?
    };

    let value_op = crate::args::parse_value_wrapped(&arg)?;

    Ok(FileInputEntry::AddressRef {
        name:      addr_name.to_owned(),
        operation: value_op,
    })
}
