#![no_main]
#![no_std]

extern crate alloc;

mod args;
mod input;
mod utils;

use args::{HELP_MSG, ParseError, ValueArg, ValueOperation};
use uefi::{prelude::*, table::runtime::ResetType};
use uefi_services::println;
use utils::{UEFIValue, WriteStatus};

use crate::input::parse_input;

#[entry]
fn main(_image_handle: Handle, mut system_table: SystemTable<Boot>) -> Status {
    uefi_services::init(&mut system_table)
        .expect("Unexpected error while initializing UEFI services");

    let args = match args::parse_args(system_table.boot_services()) {
        Ok(args) => {
            if args.help_msg {
                println!("{}", HELP_MSG);
                return Status::INVALID_PARAMETER;
            }

            args
        }
        Err(ParseError::NoArgs) => {
            match parse_input(&mut system_table) {
                Ok(args) => args,
                Err(e) => {
                    println!("Error parsing input file:\n{e}");
                    return Status::INVALID_PARAMETER;
                }
            }
        }
        Err(e) => {
            println!("Error parsing arguments:\n{e}");
            return Status::INVALID_PARAMETER;
        }
    };

    for val_arg in args.value_args {
        let status = process_arg(&system_table, args.write_on_demand, &val_arg);

        if status != Status::SUCCESS {
            return status;
        }
    }

    if args.reboot {
        system_table
            .runtime_services()
            .reset(ResetType::WARM, Status::SUCCESS, None)
    }

    Status::SUCCESS
}

fn process_arg(
    system_table: &SystemTable<Boot>,
    write_on_demand: bool,
    val_arg: &ValueArg,
) -> Status {
    let var_name = &val_arg.addr.var_name;
    let val_size = val_arg.addr.val_size;
    let offset = val_arg.addr.offset;

    match val_arg.operation {
        ValueOperation::Read => {
            match utils::read_val(
                system_table.runtime_services(),
                var_name,
                val_arg.addr.var_id,
                offset,
                val_size,
            ) {
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
                system_table.runtime_services(),
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
