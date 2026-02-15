# SORT ARRAY

| | |
|---|---|
| **Category** | Command |

## Description

ASC/DSC - Optional - Which way to sort. Use ASC for ascending (names and numbers get larger as the array element number increases) or DSC for descending (names and numbers get smaller as array element number increases).
This is the TAS Professional 3.0 command here for compatibility. The preferred TAS Professional 5.1 command is SORT ARRAY.

## Syntax

```text
SORT ARRAY
```

## Parameters

- **`mem_area#`** · `f/c/e` · *Required*

  This is the memory area to use. This must resolve to a value of 1 through 4.

- **`sort_expression`** · `f/c/e` · *Required*

  This resolves to the value to sort.

- **`size_of_sort_expr`** · `f/c/e` · *Required*

  The internal size of the value to sort.

- **`num_flds_to_sort`** · `f/c/e` · *Required*

  This is the maximum number of entries that will be made to the sort buffer. This is only required when initializing the sort buffer at the beginning of the process.

- **`chg_value`** · `f/c/e` · *Required*

  The amount to add to or subtract from each pointer. NOTE: The size and type of this field must be the same as the pointer.

- **`what_to_do`** · `f/c/e` · *Required*

  This must resolve to either 0 (initialize the sort buffer) or 1 (insert a new item into the buffer).

## Comments

This is the equivalent of the TAS Professional 3.0 command Sort.

## Sample Program

`SORTEST`

## Program Editor

`Field -> Array -> Sort`

## See Also

- [RDA](RDA.md)
- [WRTA](WRTA.md)
