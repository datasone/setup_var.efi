#![no_main]
#![no_std]

extern crate alloc;

mod args;
mod input;
mod utils;

use args::{HELP_MSG, ParseError, ValueArg, ValueOperation};
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

    for val_arg in args.value_args {
        let status = process_arg(args.write_on_demand, &val_arg);

        if status != Status::SUCCESS {
            return status;
        }
    }

    if args.reboot {
        runtime::reset(ResetType::WARM, Status::SUCCESS, None)
    }

    Status::SUCCESS
}

fn process_arg(write_on_demand: bool, val_arg: &ValueArg) -> Status {
    let var_name = &val_arg.addr.var_name;
    let val_size = val_arg.addr.val_size;
    let offset = val_arg.addr.offset;

    match val_arg.operation {
        ValueOperation::Read => {
            match utils::read_val(var_name, val_arg.addr.var_id, offset, val_size) {
                Ok(value) => println!("{}", val_arg.to_string_with_val(&value)),
                Err(e) => {
                    println!("Error reading variable:\n{e}");
                    return Status::ABORTED;
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
                    return Status::ABORTED;
                }
                Ok(status) => {
                    let skipped_msg = if let WriteStatus::Skipped = status {
                        " # Writing skipped"
                    } else {
                        ""
                    };

                    println!("{}{}", val_arg.to_string_with_val(&value), skipped_msg);
                }
            }
        }
    }

    Status::SUCCESS
}
