# WRITE

| | |
|---|---|
| **Category** | Command |
| **Platform** | 3.0 |

## Description

This command will cause the program to write a specific number of characters to a non-TAS file at a certain location.

## Syntax

```text
WRITE file_number START start NCHR number_of_characters FROM from MEM_AREA
mem_area# OFST offset_within_mem_area
```

## Parameters

- **`file_number`** · `f/c/e` · *Required*

  The file number value. This is the same value as that received in the OPENV (Open Variable) command. The file must have been previously opened.

- **`start`** · `f/c/e` · *Optional*

  Optional - The first byte position to write to. The first character position in the file, for this command, is 1. If no start value is given or the value is 0 the program will use the current internal file position value. (When the file is opened the position value is automatically set to 1.) This must be an R type field.

- **`NCHR`** · `f/c/e` · *Optional*

  The number of characters to write. If this is not supplied the program will use the size value stated in the OPENV (Open Variable) command.

- **`FROM`** · `fn/v` · *Optional*

  Normally the characters read would be written from the buffer specified in the OPENV (Open Variable) command. However, you may use this option to specify a different buffer field to be used during the execution of this command.

- **`mem_area#`** · `f/c/e` · *Optional*

  If you want to write the characters from a memory area you can specify the number here. If you use this it must resolve to a value of 1 through 4.
  NOTE: If you don't specify an OFST value the characters will start at position 1.

- **`OFST`** · `f/c/e` · *Optional*

  offset_within_mem_area - Optional - If you specify a memory area (MEM_AREA) you can also set an offset within that area. The first character in a memory area is at position 1.

## Comments

After execution, the internal file position value will be set to the first character immediately following
the characters written. For example, if the start value is set at 10 and the number of characters to be
written is 10, the position after the WRITE command will be 20.

## Program Editor

`fiLe -> Write`

## See Also

- [READ](READ.md)
