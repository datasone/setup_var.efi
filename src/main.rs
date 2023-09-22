#![no_main]
#![no_std]

extern crate alloc;

mod args;
mod utils;

use uefi::{prelude::*, table::runtime::ResetType};
use uefi_services::println;
use utils::WriteStatus;

use crate::args::{ValueArg, ValueOperationType, HELP_MSG};

#[entry]
fn main(_image_handle: Handle, mut system_table: SystemTable<Boot>) -> Status {
    uefi_services::init(&mut system_table)
        .expect("Unexpected error while initializing UEFI services");

    match args::parse_args(system_table.boot_services()) {
        Ok(args) => {
            if args.help_msg {
                println!("{}", HELP_MSG);
                return Status::INVALID_PARAMETER;
            }

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
        }
        Err(e) => {
            println!("Error parsing arguments:\n{e}");
            return Status::INVALID_PARAMETER;
        }
    }

    Status::SUCCESS
}

fn process_arg(
    system_table: &SystemTable<Boot>,
    write_on_demand: bool,
    val_arg: &ValueArg,
) -> Status {
    let var_name = &val_arg.addr.var_name;
    let val_size = val_arg.operation.val_size;
    let offset = val_arg.addr.offset;

    match val_arg.operation.op_type {
        ValueOperationType::Read => {
            match utils::read_val(
                system_table.runtime_services(),
                var_name,
                val_arg.addr.var_id,
                offset,
                val_size,
            ) {
                Ok(value) => println!(
                    "Read value in {var_name} at offset 0x{:X} with 0x{:X} bytes: {}",
                    offset,
                    val_size,
                    value.to_string_with_size(val_size),
                ),
                Err(e) => {
                    println!("Error reading variable:\n{e}");
                    return Status::ABORTED;
                }
            }
        }
        ValueOperationType::Write(value) => {
            match utils::write_val(
                system_table.runtime_services(),
                var_name,
                val_arg.addr.var_id,
                offset,
                value,
                val_size,
                write_on_demand,
            ) {
                Err(e) => {
                    println!("Error writing variable:\n{e}");
                    return Status::ABORTED;
                }
                Ok(WriteStatus::Normal) => {
                    println!(
                        "Written value in {var_name} at offset 0x{:X} with 0x{:X} bytes: \
                         0x{:0width$X}",
                        offset,
                        val_size,
                        value,
                        width = val_size * 2
                    )
                }
                Ok(WriteStatus::Skipped) => {
                    println!(
                        "Skipped written value in {var_name} at offset 0x{:X} with 0x{:X} bytes: \
                         The value is already 0x{:0width$X}",
                        offset,
                        val_size,
                        value,
                        width = val_size * 2
                    )
                }
            }
        }
    }

    Status::SUCCESS
}
