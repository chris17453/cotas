# MOVE DATA IN MEMORY

| | |
|---|---|
| **Category** | Command |

## Description

This command will move a specified number of characters from one location to another without concern to type or individual field sizes.

## Syntax

```text
XFER FROM from_field TO to_field NCHR number_of_characters FMEM from_mem_area#
TMEM to_mem_area#
```

## Parameters

- **`from_field`** · `fn/v` · *Required*

  The from field.

- **`to_field`** · `fn/v` · *Required*

  The to field or recipient.

- **`number_of_characters`** · `f/c/e` · *Required*

  The number of characters to move.

- **`FMEM`** · `f/c/e` · *Optional*

  from_mem_area# - f/c/e - Optional - The memory area number to transfer from. This must resolve to a value from 1 through 4. If you specify this value then the from_field is evaluated as the offset within this area.

- **`TMEM`** · `f/c/e` · *Optional*

  to_mem_area# - f/c/e - Optional - The memory area number to transfer to.
