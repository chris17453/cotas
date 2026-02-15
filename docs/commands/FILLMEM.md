# FILLMEM

| | |
|---|---|
| **Category** | Command |

## Description

This is a TAS Professional 3.0 command here for compatibility. There is no preferred TAS Professional 5.1 command/function.

## Syntax

```text
FILLMEM mem_area# START start_val NCHR num_chrs CHR chr_to_use
```

## Parameters

- **`mem_area#`** · `f/c/e` · *Required*

  This is the memory area to fill. This must resolve to a value of 1 through 4.

- **`START`** · `f/c/e` · *Required*

  

- **`start_val`** · `f/c/e` · *Required*

  The starting character in the memory area. The first character is at position 1.

- **`NCHR`** · `f/c/e` · *Required*

  

- **`num_chrs`** · `f/c/e` · *Required*

  The number of characters to fill.

- **`CHR`** · `f/c/e` · *Required*

  

- **`chr_to_use`** · `f/c/e` · *Required*

  What to use as a fill character.

## Comments

This is the equivalent of the TAS Professional 3.0 command Fill Memory.

## Program Editor

`3.0 Commands -> Fillmem`
