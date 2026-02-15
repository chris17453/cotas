# POKE

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS |

## Description

POKE (DOS only) Use this command to ‘poke’ or put a byte or string of bytes into a specific location in memory.

## Syntax

```text
POKE offset FLD giving_fld NCHR number_of_chrs
```

## Parameters

- **`offset`** · `f/c/e` · *Required*

  f/c/e - Required - The actual memory location.

- **`giving_fld`** · `fn/v` · *Required*

  fn/v - Required - The field that contains the value to be inserted into memory at the specified location.

- **`number_of_chrs`** · `f/c/e` · *Optional*

  f/c/e - Optional - The number of characters to poke. If you don’t specify a number here the program defaults to 1.

## Comments

This command, along with PEEK, gives you a quick and easy way to make certain settings on a PC when you know the appropriate area will always be in the same location in memory.

## Program Editor

`PROGRAM EDITOR
System -> Programming -> pOke`

## See Also

- [PEEK](PEEK.md)
- [TEST()](../functions/TEST.md)
