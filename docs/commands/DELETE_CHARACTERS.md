# DELETE CHARACTERS

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to delete a group of characters from an alpha field.

## Syntax

```text
DELC fieldname START start NCHR number_of_characters MEM_AREA mem_area#
```

## Parameters

- **`fieldname`** · `fn/v` · *Required*

  The name of the alpha field.

- **`start`** · `f/c/e` · *Optional*

  The starting position within the field. The first character in the field is at position 1. If this value isn’t specified the program will set start to the first character.

- **`number_of_characters`** · `f/c/e` · *Required*

  The number of characters to delete from the field.

- **`mem_area#`** · `f/c/e` · *Optional*

  If you are actually deleting the characters from a memory area then this is the number (from 1-4). NOTE: Even if you want to use a memory area you must still specify a fieldname, although it will be ignored during execution of the command.

## Comments

This command has the same effect as though you cut out a piece of the field and put the remaining two pieces back together, filling the end of the field with spaces.

## Program Editor

`Field -> alpha Fld cmds -> Delete chrs`

## See Also

- [DEL](DEL.md)
