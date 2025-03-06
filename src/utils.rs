use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::fmt::{Display, Formatter};

use num_enum::TryFromPrimitive;
use strum::EnumMessage;
use uefi::{
    CStr16, CString16, Status,
    data_types::FromSliceWithNulError,
    print, println,
    runtime::{VariableAttributes, VariableKey, VariableVendor},
};

#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(TryFromPrimitive, EnumMessage, strum::Display)]
#[repr(u8)]
pub enum UEFIStatusErrorCode {
    /// The image failed to load.
    LOAD_ERROR           = 1,
    /// A parameter was incorrect.
    INVALID_PARAMETER    = 2,
    /// The operation is not supported.
    UNSUPPORTED          = 3,
    /// The buffer was not the proper size for the request.
    BAD_BUFFER_SIZE      = 4,
    /// The buffer is not large enough to hold the requested data.
    /// The required buffer size is returned in the appropriate parameter.
    BUFFER_TOO_SMALL     = 5,
    /// There is no data pending upon return.
    NOT_READY            = 6,
    /// The physical device reported an error while attempting the operation.
    DEVICE_ERROR         = 7,
    /// The device cannot be written to.
    WRITE_PROTECTED      = 8,
    /// A resource has run out.
    OUT_OF_RESOURCES     = 9,
    /// An inconstency was detected on the file system.
    VOLUME_CORRUPTED     = 10,
    /// There is no more space on the file system.
    VOLUME_FULL          = 11,
    /// The device does not contain any medium to perform the operation.
    NO_MEDIA             = 12,
    /// The medium in the device has changed since the last access.
    MEDIA_CHANGED        = 13,
    /// The item was not found.
    NOT_FOUND            = 14,
    /// Access was denied.
    ACCESS_DENIED        = 15,
    /// The server was not found or did not respond to the request.
    NO_RESPONSE          = 16,
    /// A mapping to a device does not exist.
    NO_MAPPING           = 17,
    /// The timeout time expired.
    TIMEOUT              = 18,
    /// The protocol has not been started.
    NOT_STARTED          = 19,
    /// The protocol has already been started.
    ALREADY_STARTED      = 20,
    /// The operation was aborted.
    ABORTED              = 21,
    /// An ICMP error occurred during the network operation.
    ICMP_ERROR           = 22,
    /// A TFTP error occurred during the network operation.
    TFTP_ERROR           = 23,
    /// A protocol error occurred during the network operation.
    PROTOCOL_ERROR       = 24,
    /// The function encountered an internal version that was
    /// incompatible with a version requested by the caller.
    INCOMPATIBLE_VERSION = 25,
    /// The function was not performed due to a security violation.
    SECURITY_VIOLATION   = 26,
    /// A CRC error was detected.
    CRC_ERROR            = 27,
    /// Beginning or end of media was reached
    END_OF_MEDIA         = 28,
    /// The end of the file was reached.
    END_OF_FILE          = 31,
    /// The language specified was invalid.
    INVALID_LANGUAGE     = 32,
    /// The security status of the data is unknown or compromised and
    /// the data must be updated or replaced to restore a valid security status.
    COMPROMISED_DATA     = 33,
    /// There is an address conflict address allocation
    IP_ADDRESS_CONFLICT  = 34,
    /// A HTTP error occurred during the network operation.
    HTTP_ERROR           = 35,
}

pub enum UEFIStatusError {
    Success,
    Error(UEFIStatusErrorCode),
    UnknownError(usize),
}

impl From<Status> for UEFIStatusError {
    fn from(value: Status) -> Self {
        if !value.is_error() {
            Self::Success
        } else {
            match UEFIStatusErrorCode::try_from(value.0 as u8) {
                Ok(error_code) => UEFIStatusError::Error(error_code),
                Err(_) => UEFIStatusError::UnknownError(value.0),
            }
        }
    }
}

impl Display for UEFIStatusError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            UEFIStatusError::Success => write!(f, "Success"),
            UEFIStatusError::Error(error_code) => {
                write!(
                    f,
                    "{} ({})",
                    error_code,
                    error_code.get_documentation().unwrap_or_default()
                )
            }
            UEFIStatusError::UnknownError(code) => write!(f, "Unknown error with code {}", code),
        }
    }
}

pub enum UEFIVarError {
    NoCorrespondingVar,
    MultipleVarNoId,
    InvalidVarName(FromSliceWithNulError),
    GetVariableSize(String, UEFIStatusError),
    GetVariable(String, UEFIStatusError),
    OffsetOverflow((usize, usize), usize), // ((offset, val_size), var_size)
    SetVariable(String, UEFIStatusError),
}

impl Display for UEFIVarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
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
                write!(f, "Error while getting size of variable {s}: {st}")
            }
            Self::GetVariable(s, st) => {
                write!(f, "Error while getting content of variable {s}: {st}")
            }
            Self::OffsetOverflow((offset, val_size), size) => {
                write!(
                    f,
                    "Specified offset 0x{offset:X} and value size 0x{val_size:X} exceeds variable \
                     size 0x{size:X}"
                )
            }
            Self::SetVariable(s, st) => {
                write!(f, "Error while setting content of variable {s}: {st}")
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
    var_name: &CStr16,
    var_id: Option<usize>,
    offset: usize,
    val_size: usize,
) -> Result<UEFIValue, UEFIVarError> {
    let var = read_var(var_name, var_id)?;
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
    var_name: &CStr16,
    var_id: Option<usize>,
    offset: usize,
    value: usize,
    val_size: usize,
    write_on_demand: bool,
) -> Result<WriteStatus, UEFIVarError> {
    let mut var = read_var(var_name, var_id)?;
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

        uefi::runtime::set_variable(&var.name, &var.vendor, var.attributes, &var.content)
            .map_err(|e| UEFIVarError::SetVariable(var.name.to_string(), e.status().into()))?;

        Ok(WriteStatus::Normal)
    }
}

fn read_var(var_name: &CStr16, var_id: Option<usize>) -> Result<UEFIVariable, UEFIVarError> {
    let keys = uefi::runtime::variable_keys();
    let mut keys = keys
        .into_iter()
        .filter_map(|k| k.ok())
        .filter(|k| k.name == var_name)
        .collect::<Vec<_>>();
    keys.sort_by_key(|k| k.vendor.0);

    if keys.is_empty() {
        return Err(UEFIVarError::NoCorrespondingVar);
    }

    if keys.len() > 1 && var_id.is_none() {
        print_keys(keys)?;
        return Err(UEFIVarError::MultipleVarNoId);
    }

    let var_key = if keys.len() == 1 {
        &keys[0]
    } else {
        &keys[var_id.unwrap()] // if keys.len() == 1, then branch above; if keys.len() > 1 && var_id is None, then above if
    };
    let var_name = &var_key.name;

    let size = get_variable_size(var_name, &var_key.vendor)
        .map_err(|e| UEFIVarError::GetVariableSize(var_name.to_string(), e.status().into()))?;
    let mut buf = vec![0; size];
    let (_, var_attr) = uefi::runtime::get_variable(var_name, &var_key.vendor, &mut buf)
        .map_err(|e| UEFIVarError::GetVariable(var_name.to_string(), e.status().into()))?;

    Ok(UEFIVariable {
        name:       cstr16_to_cstring16(var_name),
        vendor:     var_key.vendor,
        attributes: var_attr,
        content:    buf,
    })
}

fn print_keys(keys: Vec<VariableKey>) -> Result<(), UEFIVarError> {
    println!("Multiple variables with same name detected:");
    println!("ID\tVarName\t\t\t\tSize\t");
    println!();

    for (i, key) in keys.into_iter().enumerate() {
        let id = i;
        let name = &key.name;
        let size = get_variable_size(name, &key.vendor)
            .map_err(|e| UEFIVarError::GetVariableSize(name.to_string(), e.status().into()))?;
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

fn get_variable_size(name: &CStr16, vendor: &VariableVendor) -> uefi::Result<usize> {
    let res = uefi::runtime::get_variable(name, vendor, &mut []);
    if res.status() == Status::BUFFER_TOO_SMALL {
        let err = res.unwrap_err();
        Ok(err.data().unwrap_or_default())
    } else {
        Err(res.discard_errdata().unwrap_err())
    }
}

pub(crate) fn cstr16_to_cstring16(s: &CStr16) -> CString16 {
    s.to_u16_slice_with_nul().to_vec().try_into().unwrap() // The try_into will success as it's directly obtained from CStr16
}
