#![no_main]
#![no_std]
#![feature(abi_efiapi)]

extern crate alloc;

mod args;
mod utils;

use uefi::prelude::*;
use uefi_services::println;

use crate::args::HELP_MSG;

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
            let var_name = args.var_name.unwrap_or_else(|| "Setup".try_into().unwrap());
            let val_size = args.val_size.unwrap_or(1);
            let offset = args.offset.unwrap(); // It's checked in args::parse_args

            if let Some(value) = args.value {
                if let Err(e) = utils::write_val(
                    system_table.runtime_services(),
                    &var_name,
                    args.var_id,
                    offset,
                    value,
                    val_size,
                ) {
                    println!("Error writing variable:\n{e}");
                    return Status::ABORTED;
                } else {
                    println!(
                        "Written value in {var_name} at offset 0x{:X} with 0x{:X} bytes: 0x{:0width$X}",
                        offset,
                        val_size,
                        value,
                        width = val_size * 2
                    )
                }
            } else {
                match utils::read_val(
                    system_table.runtime_services(),
                    &var_name,
                    args.var_id,
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
        }
        Err(e) => {
            println!("Error parsing arguments:\n{e}");
            return Status::INVALID_PARAMETER;
        }
    }

    Status::SUCCESS
}
