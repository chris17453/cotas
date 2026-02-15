# READ

| | |
|---|---|
| **Category** | Command |

## Description

This command will cause the program to read a specific number of characters from a non-TAS file at a certain location.

## Syntax

```text
READ file_number START start NCHR number_of_characters TO to MEM_AREA mem_area#
OFST offset_within_mem_area
```

## Parameters

- **`file_number`** · `n` · *Required*

  The file number from which to read.

- **`start`** · `n` · *Required*

  The starting location within the file.

- **`number_of_characters`** · `n` · *Required*

  The number of characters to read.

- **`to`** · `n` · *Required*

  The destination in memory where the read characters are stored.

- **`mem_area#`** · `n` · *Required*

  The memory area number.

- **`offset_within_mem_area`** · `n` · *Required*

  Offset within the memory area.
