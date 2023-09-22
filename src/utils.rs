use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::fmt::{Display, Formatter};

use uefi::{
    data_types::FromSliceWithNulError,
    prelude::RuntimeServices,
    table::runtime::{VariableAttributes, VariableKey, VariableVendor},
    CStr16, CString16, Status,
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
    fn from_usize(value: usize, val_size: usize) -> Self {
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
    value: usize,
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

    let value = UEFIValue::from_usize(value, val_size);
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
        name:       cstr16_to_cstring16(var_name),
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

pub(crate) fn cstr16_to_cstring16(s: &CStr16) -> CString16 {
    s.to_u16_slice_with_nul().to_vec().try_into().unwrap() // The try_into will success as it's directly obtained from CStr16
}
