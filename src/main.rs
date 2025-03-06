#![no_main]
#![no_std]

extern crate alloc;

mod args;
mod input;
mod utils;

use args::{HELP_MSG, ParseError, RebootMode, ValueArg, ValueOperation};
use uefi::{prelude::*, println, runtime::ResetType};
use utils::{UEFIValue, WriteStatus};

use crate::input::parse_input;

#[entry]
fn main() -> Status {
    uefi::helpers::init().expect("Unexpected error while initializing UEFI services");

    let args = match args::parse_args() {
        Ok(args) => {
            if args.help_msg {
                println!("{}", HELP_MSG);
                return Status::INVALID_PARAMETER;
            }

            args
        }
        Err(ParseError::NoArgs) => match parse_input() {
            Ok(args) => args,
            Err(ParseError::NoInput) => {
                println!("{}", HELP_MSG);
                return Status::INVALID_PARAMETER;
            }
            Err(e) => {
                println!("Error parsing input file:\n{e}");
                return Status::INVALID_PARAMETER;
            }
        },
        Err(e) => {
            println!("Error parsing arguments:\n{e}");
            return Status::INVALID_PARAMETER;
        }
    };

    let mut arg_process_state = ArgProcessState::default();
    for val_arg in args.value_args {
        let (status, state) = process_arg(args.write_on_demand, &val_arg);

        if status != Status::SUCCESS {
            return status;
        }
        arg_process_state.bind(&state);
    }

    let reboot_required = matches!(arg_process_state.write_status, WriteStatus::Normal);
    if args.reboot == RebootMode::Always || (args.reboot == RebootMode::Auto && reboot_required) {
        runtime::reset(ResetType::WARM, Status::SUCCESS, None);
    }

    Status::SUCCESS
}

struct ArgProcessState {
    write_status: WriteStatus,
}

impl ArgProcessState {
    fn bind(&mut self, other: &Self) {
        if let WriteStatus::Skipped = self.write_status {
            if let WriteStatus::Normal = other.write_status {
                self.write_status = WriteStatus::Normal;
            }
        }
    }

    fn nothing_written() -> Self {
        Self {
            write_status: WriteStatus::Skipped,
        }
    }
}

impl Default for ArgProcessState {
    fn default() -> Self {
        Self::nothing_written()
    }
}

fn process_arg(write_on_demand: bool, val_arg: &ValueArg) -> (Status, ArgProcessState) {
    let var_name = &val_arg.addr.var_name;
    let val_size = val_arg.addr.val_size;
    let offset = val_arg.addr.offset;

    match val_arg.operation {
        ValueOperation::Read => {
            match utils::read_val(var_name, val_arg.addr.var_id, offset, val_size) {
                Ok(value) => {
                    println!("{}", val_arg.to_string_with_val(&value));
                    (Status::SUCCESS, ArgProcessState::nothing_written())
                }
                Err(e) => {
                    println!("Error reading variable:\n{e}");
                    (Status::ABORTED, ArgProcessState::nothing_written())
                }
            }
        }
        ValueOperation::Write(value) => {
            let value = UEFIValue::from_usize(value, val_size);

            match utils::write_val(
                var_name,
                val_arg.addr.var_id,
                offset,
                &value,
                val_size,
                write_on_demand,
            ) {
                Err(e) => {
                    println!("Error writing variable:\n{e}");
                    (Status::ABORTED, ArgProcessState::nothing_written())
                }
                Ok(write_status) => {
                    let skipped_msg = match write_status {
                        WriteStatus::Skipped => " # Writing skipped",
                        WriteStatus::Normal => "",
                    };

                    println!("{}{}", val_arg.to_string_with_val(&value), skipped_msg);
                    (Status::SUCCESS, ArgProcessState { write_status })
                }
            }
        }
    }
}
