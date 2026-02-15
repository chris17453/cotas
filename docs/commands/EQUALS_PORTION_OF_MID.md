# EQUALS PORTION OF (MID)

| | |
|---|---|
| **Category** | Command |

## Description

This is the equivalent of the TAS Professional 3.0 command Equals Portion Of.

## Syntax

```text
EQUALS PORTION OF (MID) start_val num_chrs mem_area#
```

## Parameters

- **`start_val`** · `f/c/e` · *Required*

  The starting character in the parse_fld. The first character in the field is at position 1.

- **`num_chrs`** · `f/c/e` · *Required*

  The number of characters to get from the parse_fld.

- **`mem_area#`** · `f/c/e` · *Optional*

  If you are actually getting the characters from a memory area then this is the number (from 1-4). NOTE: Even if you want to use a memory area you must still specify a parse_fld, although it will be ignored during execution of the command.

## Comments

This is the equivalent of the TAS Professional 3.0 command Equals Portion Of.
