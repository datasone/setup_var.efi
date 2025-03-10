# UEFI Command Line Tool for Reading/Writing UEFI Variables
This tool is a rewritten version of the [modded grub with setup_var commands](https://github.com/datasone/grub-mod-setup_var), enhanced with much cleaner code, aarch64 support and ability to be automated. The tool is able to read/write UEFI variables, and is usually used for changing BIOS settings which are hidden from UI.

⚠ Use this tool with extreme caution as accessing wrong varstore or variable may completely brick your computer!

---
**The new usage document is for 0.3.0 version and later with the new syntax for specifying variables. For 0.2.x releases with the old syntax (0.2.x will still be maintained so you can continue to use that), refer to [document of that version](https://github.com/datasone/setup_var.efi/blob/0.2.x/README.md).**

---
Note: If you have been using the modded grub tool, you may want to refer to [the cheatsheet](#grub-cheatsheet).

## Term definitions
The legacy names used in grub version of the tool and some other related tools (e.g. IFR extractor) used *varstore* and *variable*. The former word is not used in UEFI specification, and is actually the **variable** in UEFI runtime services (sometimes referred to as "variable store", which is possibly where "varstore" came from). So to clarify this, this project uses "variable" and "value":
- **Variable** is the former *varstore*, which means a named storage with varying size from several to several thousands of bytes. Maybe used for storing many values of many BIOS setting items.
- **Value** is the former *variable*, which represents a value in the variable with few (often one to two) bytes, referred with variable and offset. While changing BIOS settings, this means the value of *one* BIOS setting item.

## Usage
Run `setup_var.efi -h` in UEFI shell to get this help:
```
setup_var.efi [-h/--help] [-r/--reboot(=auto)] [--write_on_demand] VALUE_ARG...

-r or --reboot: Reboot (warm reset) the computer after the program successfully finishes.
--reboot=auto: Reboot only if any value was actually written. This option automatically enables --write_on_demand.
--write_on_demand: If the value desired to be written is the same with storage, skip the unnecessary write.

VALUE_ARG represents the value needs to be read/written, and can be specified multiple times.
The format of VALUE_ARG is: <VAR_NAME>[(VAR_ID)]:<OFFSET>[(VALUE_SIZE)][=VALUE]

VAR_NAME: The name of UEFI variable to be altered, defaults to "Setup".
VAR_ID: Unique id for distinguishing variables with same name, which will be provided by setup_var.efi (when required).
OFFSET: The offset of value to be altered in the UEFI variable.
VALUE: The new value to write, capped at 64-bit. If not specified, the value at OFFSET will be read and shown.
VALUE_SIZE: Bytes of value to write, must be equal or larger than the size of <VALUE>, defaults to 1.

OFFSET, VALUE, VALUE_SIZE and VAR_ID are numbers, and can be specified in hexadecimal with prefix "0x", or decimal with no prefixes.

The program defaults to little endian for values ONLY while operating UEFI variables,
though it's recommended to only operate on one byte if you are not sure what this is or means.

Example: .\setup_var.efi -r CpuSetup:0x10E=0x1A
```

---

### Examples for `VALUE_ARG`
- `CpuSetup:0x10E=0x1A`: write one byte `0x1A` to offset `0x10E` in `CpuSetup`.
- `CpuSetup:0x10E(2)`: read one word (2 bytes) from offset `0x10E` in `CpuSetup`.
- `Setup(1):0x100(2)=0x1000`: write one word `0x1000` to offset `0x100` in `Setup` with variable id `1`.

---

For changing BIOS settings, you may use those following steps to obtain the variable name and value offset:
1. Obtain (raw) BIOS image, some motherboard OEMs provides BIOS image that can be directly opened but it's not commonly the case for branded PCs. For Intel CPU-based systems, you may use **Flash Programming Tool** from CSME System Tools to directly extract raw BIOS image.
2. Use [UEFITool](https://github.com/LongSoft/UEFITool) to open the BIOS image, and find `Setup` as string, there would be image sections named "Setup/*", navigate to that and extract body of the `Setup` folder to a file.
3. Use [IFR-Extractor](https://github.com/LongSoft/Universal-IFR-Extractor) to export setup info to a text file.
4. Find your desired setting in the text file, note the offset after `VarStoreInfo (VarOffset/VarName):` and the id after `VarStore:`.
5. Search for `VarStoreId: {id}`, where `{id}` is the id found earlier. And note the `Name` after it.
6. Change the value using noted variable(varstore) name, offset, and size.

## Input File
`setup_var.efi` now supports specifying with a input file (by UEFI shell redirection). All options that can be specified with arguments are available in input file.

Note: The text should be in UTF-16LE encoding, see [pinned issue #25](https://github.com/datasone/setup_var.efi/issues/25) for details.

The input file consists of three parts:
- Named arguments which are specified with a leading `@`.
- Defining a value address reference (to make the input file more readable) with `:=`.
- Specifying value arguments as how they are specified in progrm arguments.

Comments starting with `#` can be specified in the input file.

### Input File Example
```
# Named arguments
# @reboot # Reboot after executing setup_var.efi
@write_on_demand # Only write values when necessary

# Address Definition
ADLImonSlope:=CpuSetup:0x016E
ADLImonPrefix:=CpuSetup:0x0182

# Value Args
# Address can be referenced with leading '$'
$ADLImonSlope=0x14
$ADLImonPrefix=0x01

# Normal value argument also works
CpuSetup:0x0178(2)=0x4E20
```

The output of `setup.var.efi` has also be altered to match the format of the input file. So you can save your configurations with
```shell
setup_var.efi CpuSetup:0x10E=0x1A > vars.txt
```
and load them afterwards with
```shell
setup_var.efi < vars.txt
```

## GRUB Cheatsheet
The legacy grub commands can be mapped to these usages of this tool:
- `setup_var offset [value]`: `setup_var.efi Setup:offset[=value]`.
- `setup_var2 offset [value]`: This command searches for named `Custom`, so `setup_var.efi Custom:offset[=value]`.
- `setup_var_3 offset [value]`: This command discards variables that are too small, `setup_var.efi` has the `VAR_ID` argument to handle this. Though it's not tested as the problemed firmware used to test this command has been updated and the issue was solved.
- `setup_var_vs offset [value_size] [value]`: This command processes variable-sized values, and is mapped to `setup_var.efi Setup:offset(value_size)[=value]`.
- `setup_var_cv name offset [value_size] [value]`: This was the full-featured command, to use full features, use `setup_var.efi name:offset(value_size)[=value]`, in which the `(value_size)` can be omitted for 1-byte value.

## Build
```shell
cargo build [--release]
```

The default and tested target for this project is `x86_64-unknown-uefi`, it can also target `aarch64-unknown-uefi` for AArch64 UEFI and `i686-unknown-uefi` for 32-bit x86 EFI, though they are not tested and may contain glitches.
