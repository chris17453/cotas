# PEEK

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS |

## Description

Use this command to ‘peek’ at a specific location in memory.

## Syntax

```text
PEEK offset FLD receiving_fld NCHR number_of_chrs
```

## Parameters

- **`offset`** · `f/c/e` · *Required*

  The actual memory location.

- **`receiving_fld`** · `fn/v` · *Required*

  The field that will receive the value at the specified memory location.

- **`NCHR`** · `n` · *Required*

  The number of characters to get.

- **`number_of_chrs`** · `f/c/e` · *Optional*

  The number of characters to get. If you don’t specify a number here the program defaults to 1.

## Comments

This command, along with POKE, gives you a quick and easy way to check certain settings on a PC when you know the value will always be in the same location in memory.

## Sample Program

`CAPSLOCK`

## Program Editor

`System -> Programming -> pEek`

## See Also

- [POKE](POKE.md)
- [TEST()](../functions/TEST.md)
