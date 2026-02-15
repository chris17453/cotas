# SET LINE

| | |
|---|---|
| **Category** | Command |
| **Platform** | PROGRAM EDITOR |

## Description

This command will set up the fields to be used in a LISTF/LISTM command as a First Line (FLINE).

## Syntax

```text
SETLINE recv_fld WITH title_list
```

## Parameters

- **`recv_fld`** · `fn/v` · *Required*

  The name of the field that will be used to hold the title list.

- **`title_list`** · `f/c/e,f/c/e,...f/c/e` · *Required*

  The list of fields, constants, and/or expressions that will make up this title line.

## Comments

This command will only work in conjunction with the LISTF command.

## See Also

- [LISTF](LISTF.md)
