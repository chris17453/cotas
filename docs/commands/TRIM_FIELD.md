# TRIM FIELD

| | |
|---|---|
| **Category** | Command |

## Description

This will change the leading or trailing spaces in an A type field to binary 0s.

## Syntax

```text
TRIM fieldname TRAIL/LEAD
```

## Parameters

- **`fieldname`** · `fn/v` · *Required*

  The name of the field to be trimmed.

- **`TRAIL/LEAD`** · `e` · *Optional*

  Optional - Trim the TRAIL - trailing, or LEAD - leading spaces in the field. Default is TRAIL.

## Comments

When you use the function TRIM() the program puts the ‘trimmed’ field in the temporary data space. If you are trying to TRIM a field that is larger than that space you will get the message that the program is out of room in the temporary data area. This command is provided to you so that you can TRIM a field larger than the temporary data area within its own space. Using this command you will not run out of memory.

## Program Editor

`Field -> alpha Fld cmds -> Trim`

## See Also

- [TRIM()](../functions/TRIM.md)
