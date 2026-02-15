# PRINT TO

| | |
|---|---|
| **Category** | Command |

## Description

This command will set the default output device for any other output command.

## Syntax

```text
PON which_device PFN print_to_disk_file_name
```

## Parameters

- **`which_device`** · `ASPD` · *Required*

  This sets the default output device. The options are A ask, S - screen, P - printer or D - disk. If you specify option A, the program will ask the user at runtime where to print.

- **`print_to_disk_file_name`** · `f/c/e` · *Optional*

  If you set the which_device value to D the name of the file to use may be specified with this option. You need to indicate the entire file name, including any path and extension. If the file name is not specified the program will ask for one at runtime. NOTE: When checked at runtime, if the file exists on disk already, the user will be asked if he/she wants to overwrite. If the answer is N the user will be able to enter a different file name. This applies whether the file name is supplied here or at runtime.

## Comments

This command sets the default output device; however, individual commands may override this value. In the case of all other print commands, this command must be used to set the device to DISK if it is to be used for output. If a print to disk file is already open, it will be closed when this command is executed.

## Program Editor

`Reports -> On`

## See Also

- [PSET](PSET.md)
