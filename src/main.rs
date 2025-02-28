#![no_main]
#![no_std]

extern crate alloc;

mod args;
mod input;
mod utils;

use uefi::{prelude::*, println, runtime::ResetType};
use utils::WriteStatus;

use crate::args::HELP_MSG;

#[entry]
fn main() -> Status {
    uefi::helpers::init().expect("Unexpected error while initializing UEFI services");

    match args::parse_args() {
        Ok(args) => {
            if args.help_msg {
                println!("{}", HELP_MSG);
                return Status::INVALID_PARAMETER;
            }
            let var_name = args.var_name.unwrap_or_else(|| "Setup".try_into().unwrap());
            let val_size = args.val_size.unwrap_or(1);
            let offset = args.offset.unwrap(); // It's checked in args::parse_args

            if let Some(value) = args.value {
                match utils::write_val(
                    &var_name,
                    args.var_id,
                    offset,
                    value,
                    val_size,
                    args.write_on_demand,
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
                            "Skipped written value in {var_name} at offset 0x{:X} with 0x{:X} \
                             bytes: The value is already 0x{:0width$X}",
                            offset,
                            val_size,
                            value,
                            width = val_size * 2
                        )
                    }
                }
            } else {
                match utils::read_val(&var_name, args.var_id, offset, val_size) {
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

            if args.reboot {
                runtime::reset(ResetType::WARM, Status::SUCCESS, None)
            }
        }
        Err(e) => {
            println!("Error parsing arguments:\n{e}");
            return Status::INVALID_PARAMETER;
        }
    }
}
