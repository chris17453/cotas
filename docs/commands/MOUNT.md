# MOUNT

| | |
|---|---|
| **Category** | Command |

## Description

This command will ‘mount’, or set active, a screen or report format compiled in the program. There can be only one screen or report format active at the same time.

## Syntax

```text
MOUNT format_name TYPE format_type PTO print_to PFN print_to_file EXTERN
```

## Parameters

- **`format_name`** · `sac` · *Required*

  The name of the format to mount. This name follows the standard file name requirements. The compiler tries to find a file with the format name in the same path as the original source file. The extension defaults to .FMT, although you may specify a different one if desired.

- **`format_type`** · `SR` · *Required*

  There are two possible format types, S - screen and R - report. The SCREEN format is used to define location and type of field and constant data. When mounted, as much as possible of the entire body of the format will be displayed in the current window. The fields are put in a buffer to be used in conjunction with the ENTER command. Then if a field is referenced in the active screen format, you need not use the column/row specifiers in the ENTER command; the program will know where it is. This also applies when a record is found. If fields in the mounted

- **`PTO`** · `` · *Optional*

  

- **`PFN`** · `` · *Optional*

  

- **`EXTERN`** · `` · *Optional*

  
