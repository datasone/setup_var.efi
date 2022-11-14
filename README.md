# UEFI Command Line Tool for Reading/Writing UEFI Variables
This tool is a rewritten version of the [modded grub with setup_var commands](https://github.com/datasone/grub-mod-setup_var), enhanced with much cleaner code, aarch64 support and ability to be automated. The tool is able to read/write UEFI variables, and is usually used for changing BIOS settings which are hidden from UI.

⚠ Use this tool with exetreme caution as accessing wrong varstore or variable may completely brick your computer!

Note: If you have been using the modded grub tool, you may want to refer to [the cheatsheet](#grub-cheatsheet).

## Term definitions
The legacy names used in grub version of the tool and some other related tools (e.g. IFR extractor) used *varstore* and *variable*. The former word is not used in UEFI specification, and is actually the **variable** in UEFI runtime services (sometimes referred as "variable store", which is possibly where "varstore" came from). So to clarify this, this project use "variable" and "value":
- **Variable** is the former *varstore*, which means a named storage with varying size from several to several thousands of bytes. Maybe used for storing many values of many BIOS setting items.
- **Value** is the former *variable*, which represents a value in the variable with few (often one to two) bytes, referred with variable and offset. While changing BIOS settings, this means the value of *one* BIOS setting item.

## Usage
Run `setup_var.efi` in UEFI shell without arguments to get this help:
```
setup_var.efi <OFFSET> [<VALUE>] [-s <VALUE_SIZE>] [-n <VAR_NAME>] [-i <VAR_ID>]

OFFSET: The offset of value to be altered in the UEFI variable.
VALUE: The new value to write, capped at 64-bit. If not specified, the value at OFFSET will be read and shown.
VALUE_SIZE: Bytes of value to write, must be equal or larger than the size of <VALUE>, defaults to 0x1.
VAR_NAME: The name of UEFI variable to be altered, defaults to "Setup".
VAR_ID: Unique id for distinguishing variables with same name, which will be provided by setup_var.efi (when required).

OFFSET, VALUE, VALUE_SIZE and VAR_ID are numbers, and must be specified in hexadecimal with prefix "0x".

The program defaults to little endian for values ONLY while operating UEFI variables,
though it's recommended to only operate on one byte if you are not sure what this is or means.

Example: .\setup_var.efi 0x10E 0x1a -n CpuSetup
```

For changing BIOS settings, you may use those following steps to obtain the variable name and value offset:
1. Obtain (raw) BIOS image, some motherboard OEMs provides BIOS image that can be directly opened but it's not commonly the case for branded PCs. For Intel CPU-based systems, you may use **Flash Programming Tool** from CSME System Tools to directly extract raw BIOS image.
2. Use [UEFITool](https://github.com/LongSoft/UEFITool) to open the BIOS image, and find `Setup` as string, there would be image sections named "Setup/*", navigate to that and extract body of the `Setup` folder to a file.
3. Use [IFR-Extractor](https://github.com/LongSoft/IFRExtractor-RS) to export setup info to a text file.
4. Find your desired setting in the text file, note the offset after `VarStoreInfo (VarOffset/VarName):` and the id after `VarStore:`.
5. Search for `VarStoreId: {id}`, where `{id}` is the id found earlier. And note the `Name` after it.
6. Change the value using noted variable(varstore) name, offset, and size.

## GRUB Cheatsheet
The legacy grub commands can be mapped to these usages of this tool:
- `setup_var offset [value]`: The default arguments of `setup_var.efi`, just `setup_var.efi offset [value]`.
- `setup_var2 offset [value]`: This command searches for named `Custom`, so `setup_var.efi offset [value] -n Custom`.
- `setup_var_3 offset [value]`: This command discards variables that are too small, `setup_var.efi` has the `-i VAR_ID` argument to handle this. Though it's not tested as the problemed firmware used to test this command has been updated and the issue was solved.
- `setup_var_vs offset [value_size] [value]`: This command processes variable-sized values, and is mapped to `setup_var.efi offset [value] [-s value_size]`.
- `setup_var_cv name offset [value_size] [value]`: This was the full-featured command, to use full features, use `setup_var.efi offset [value] [-s value_size] [-n name]`, in which the `-s value_size` can be omitted for 1-byte value.

## Build
Install nightly rust as it's required by `uefi-rs`, and
```
cargo build --target x86_64-unknown-uefi [--release]
```

Use `aarch64-unknoen-uefi` for aarch64 target.
