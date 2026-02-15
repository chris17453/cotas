# UPCASE FIELD

| | |
|---|---|
| **Category** | Command |

## Description

This will change all or some subset of all the characters in an A type field to upper case.

## Syntax

```text
UP fieldname ALL
```

## Parameters

- **`fieldname`** · `fn/v` · *Required*

  The name of the field to be changed to upper case.

- **`ALL`** · `sac` · *Optional*

  If this option is included in the command line then all of the characters in the field will be changed to upper case. If it is not specified then only those characters not surrounded by quote marks (single or double) will be changed to upper case.

## Comments

When you use the function UP() the program puts the ‘upcased’ field in the temporary data space. If you are trying to UP a field that is larger than that space you will get the message that the program is out of room in the temporary data area. This command is provided to you so that you can UP a field larger than the temporary data area within its own space. Using this command you will not run out of memory.

## Program Editor

`Field -> alpha Fld cmds -> Upcase`

## See Also

- [UP()](../functions/UP.md)
