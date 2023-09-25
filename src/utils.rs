use alloc::{
    borrow::ToOwned,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::fmt::{Display, Formatter};

use uefi::{
    data_types::{chars::NUL_16, FromSliceWithNulError},
    prelude::RuntimeServices,
    table::runtime::{VariableAttributes, VariableKey, VariableVendor},
    CStr16, CString16, Char16, Status,
};
use uefi_services::{print, println};

pub enum UEFIVarError {
    EnumVars(Status),
    NoCorrespondingVar,
    MultipleVarNoId,
    InvalidVarName(FromSliceWithNulError),
    GetVariableSize(String, Status),
    GetVariable(String, Status),
    OffsetOverflow((usize, usize), usize), // ((offset, val_size), var_size)
    SetVariable(String, Status),
}

impl Display for UEFIVarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::EnumVars(st) => {
                write!(f, "Error while enumerating UEFI variables with code {st:?}")
            }
            Self::NoCorrespondingVar => {
                write!(f, "No variable with specified name found")
            }
            Self::MultipleVarNoId => {
                write!(
                    f,
                    "Please re-run with variable id as shown in the above table"
                )
            }
            Self::InvalidVarName(e) => {
                write!(f, "Unexpected invalid UEFI variable name obtained: {e:?}")
            }
            Self::GetVariableSize(s, st) => {
                write!(
                    f,
                    "Error while getting size of variable {s} with code {st:?}"
                )
            }
            Self::GetVariable(s, st) => {
                write!(
                    f,
                    "Error while getting content of variable {s} with code {st:?}",
                )
            }
            Self::OffsetOverflow((offset, val_size), size) => {
                write!(
                    f,
                    "Specified offset 0x{offset:X} and value size 0x{val_size:X} exceeds variable \
                     size 0x{size:X}"
                )
            }
            Self::SetVariable(s, st) => {
                write!(
                    f,
                    "Error while setting content of variable {s} with code {st:?}"
                )
            }
        }
    }
}

impl From<FromSliceWithNulError> for UEFIVarError {
    fn from(value: FromSliceWithNulError) -> Self {
        Self::InvalidVarName(value)
    }
}

struct UEFIVariable {
    name:       CString16,
    vendor:     VariableVendor,
    attributes: VariableAttributes,
    content:    Vec<u8>,
}

pub struct UEFIValue(pub Vec<u8>);

impl UEFIValue {
    pub fn from_usize(value: usize, val_size: usize) -> Self {
        let value = value & ((1 << (val_size * 8)) - 1);
        Self(value.to_le_bytes()[0..val_size].to_vec())
    }

    pub fn to_string_with_size(&self, val_size: usize) -> String {
        let mut bytes = [0; 8];
        bytes[0..self.0.len()].copy_from_slice(&self.0);

        format!(
            "0x{:0width$X}",
            usize::from_ne_bytes(bytes),
            width = val_size * 2
        )
    }
}

impl Display for UEFIValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut bytes = [0; 8];
        bytes[0..self.0.len()].copy_from_slice(&self.0);

        write!(f, "0x{:X}", usize::from_ne_bytes(bytes))
    }
}

pub fn read_val(
    runtime_services: &RuntimeServices,
    var_name: &CStr16,
    var_id: Option<usize>,
    offset: usize,
    val_size: usize,
) -> Result<UEFIValue, UEFIVarError> {
    let var = read_var(runtime_services, var_name, var_id)?;
    if offset + val_size > var.content.len() {
        return Err(UEFIVarError::OffsetOverflow(
            (offset, val_size),
            var.content.len(),
        ));
    }

    let slice = &var.content[offset..offset + val_size];
    Ok(UEFIValue(slice.to_vec()))
}

pub enum WriteStatus {
    Normal,
    Skipped,
}

pub fn write_val(
    runtime_services: &RuntimeServices,
    var_name: &CStr16,
    var_id: Option<usize>,
    offset: usize,
    value: &UEFIValue,
    val_size: usize,
    write_on_demand: bool,
) -> Result<WriteStatus, UEFIVarError> {
    let mut var = read_var(runtime_services, var_name, var_id)?;
    if offset + val_size > var.content.len() {
        return Err(UEFIVarError::OffsetOverflow(
            (offset, val_size),
            var.content.len(),
        ));
    }

    let slice = &mut var.content[offset..offset + val_size];

    if write_on_demand && slice == value.0 {
        Ok(WriteStatus::Skipped)
    } else {
        slice.copy_from_slice(&value.0);

        runtime_services
            .set_variable(&var.name, &var.vendor, var.attributes, &var.content)
            .map_err(|e| UEFIVarError::SetVariable(var.name.to_string(), e.status()))?;

        Ok(WriteStatus::Normal)
    }
}

fn read_var(
    runtime_services: &RuntimeServices,
    var_name: &CStr16,
    var_id: Option<usize>,
) -> Result<UEFIVariable, UEFIVarError> {
    let keys = runtime_services
        .variable_keys()
        .map_err(|e| UEFIVarError::EnumVars(e.status()))?;
    let mut keys = keys
        .into_iter()
        .filter(|k| {
            if let Ok(name) = k.name() {
                name == var_name
            } else {
                false
            }
        })
        .collect::<Vec<_>>();
    keys.sort_by_key(|k| k.vendor.0);

    if keys.is_empty() {
        return Err(UEFIVarError::NoCorrespondingVar);
    }

    if keys.len() > 1 && var_id.is_none() {
        print_keys(runtime_services, keys)?;
        return Err(UEFIVarError::MultipleVarNoId);
    }

    let var_key = if keys.len() == 1 {
        &keys[0]
    } else {
        &keys[var_id.unwrap()] // if keys.len() == 1, then branch above; if keys.len() > 1 && var_id is None, then above if
    };
    let var_name = var_key.name()?;

    let size = runtime_services
        .get_variable_size(var_name, &var_key.vendor)
        .map_err(|e| UEFIVarError::GetVariableSize(var_name.to_string(), e.status()))?;
    let mut buf = vec![0; size];
    let (_, var_attr) = runtime_services
        .get_variable(var_name, &var_key.vendor, &mut buf)
        .map_err(|e| UEFIVarError::GetVariable(var_name.to_string(), e.status()))?;

    Ok(UEFIVariable {
        name:       var_name.to_owned(),
        vendor:     var_key.vendor,
        attributes: var_attr,
        content:    buf,
    })
}

fn print_keys(
    runtime_services: &RuntimeServices,
    keys: Vec<VariableKey>,
) -> Result<(), UEFIVarError> {
    println!("Multiple variables with same name detected:");
    println!("ID\tVarName\t\t\t\tSize\t");
    println!();

    for (i, key) in keys.into_iter().enumerate() {
        let id = i;
        let name = key.name()?;
        let size = runtime_services
            .get_variable_size(name, &key.vendor)
            .map_err(|e| UEFIVarError::GetVariableSize(name.to_string(), e.status()))?;
        print!("0x{:X}\t", id);
        print!("{name}");
        match (name.num_bytes() / 2 - 1) / 8 {
            0 => {
                print!("\t\t\t\t")
            }
            1 => {
                print!("\t\t\t")
            }
            2 => {
                print!("\t\t")
            }
            _ => {
                print!("\t")
            }
        }
        println!("0x{:X}\t", size)
    }

    Ok(())
}

pub trait CStr16Ext {
    fn contains(&self, char: Char16) -> bool;
    fn strip_prefix(&self, prefix: Char16) -> Option<&CStr16>;
    fn strip_suffix(&self, suffix: Char16) -> Option<CString16>;
    /// The CStr16 requires a NUL terminator, so we can only collect them back
    /// into CString16
    fn split_to_cstring(&self, split: Char16) -> Vec<CString16>;
    fn split_only_first(&self, split: Char16) -> Option<(CString16, CString16)>;
    fn trim(&self) -> CString16;
    fn starts_with(&self, char: Char16) -> bool;
}

impl CStr16Ext for CStr16 {
    fn contains(&self, char: Char16) -> bool {
        self.iter().any(|&c| c == char)
    }

    fn strip_prefix(&self, prefix: Char16) -> Option<&CStr16> {
        if *self.iter().next()? == prefix {
            let str_ref = unsafe { CStr16::from_u16_with_nul_unchecked(&*(&self.as_slice_with_nul()[1..] as *const [Char16] as *const [u16])) };
            Some(str_ref)
        } else {
            None
        }
    }

    fn strip_suffix(&self, suffix: Char16) -> Option<CString16> {
        let str = self.to_u16_slice();
        if *str.last()? == u16::from(suffix) {
            let mut buf = self
                .as_slice_with_nul()
                .iter()
                .map(|&c| u16::from(c))
                .collect::<Vec<_>>();
            *buf.last_mut()? = 0;
            Some(CString16::try_from(buf).unwrap())
        } else {
            None
        }
    }

    fn split_to_cstring(&self, split: Char16) -> Vec<CString16> {
        let mut split_strings = Vec::new();
        let mut current_string = Vec::new();

        for c in self.iter() {
            if *c != split {
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

    fn split_only_first(&self, split: Char16) -> Option<(CString16, CString16)> {
        let index = self.iter().position(|&c| c == split)?;

        // We have to copy twice and check validity again???
        let mut first_str = self.as_slice()[..index].to_vec();
        first_str.push(NUL_16);

        let mut second_str = self.as_slice()[index + 1..].to_vec();
        second_str.push(NUL_16);

        let first_str = char16_vec_to_cstring16(first_str);
        let second_str = char16_vec_to_cstring16(second_str);

        Some((first_str, second_str))
    }

    fn trim(&self) -> CString16 {
        let is_space = |&c| c == ' '.try_into().unwrap() || c == '\t'.try_into().unwrap();

        let mut start = 0;
        for (i, c) in self.iter().enumerate() {
            if !is_space(c) {
                start = i;
            }
        }

        let mut end = self.num_chars() - 1;
        for (i, c) in self.as_slice().iter().rev().enumerate() {
            if !is_space(c) {
                end = i;
            }
        }

        let mut str = self.as_slice()[start..=end].to_vec();
        str.push(NUL_16);

        char16_vec_to_cstring16(str)
    }

    fn starts_with(&self, char: Char16) -> bool {
        *self.iter().next().unwrap_or(&NUL_16) == char
    }
}

/// We have no way to directly construct CString16, and as it does NOT use
/// repr(transparent). We even cannot construct it with unsafe operations. A
/// copy and char validity check is required in this function.
fn char16_vec_to_cstring16(s: Vec<Char16>) -> CString16 {
    CString16::try_from(s.into_iter().map(u16::from).collect::<Vec<_>>()).unwrap()
}
