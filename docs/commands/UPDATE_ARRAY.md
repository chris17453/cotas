# UPDATE ARRAY

| | |
|---|---|
| **Category** | Command |

## Description

This command is used to change all or several of the values in an array or group of arrays. It will quickly and efficiently add or subtract values, or insert or delete a specific element number in an array.

## Syntax

```text
UPDTA array_field_list INS/DEL/ADD/SUB/CLR TIMES times VAL value
```

## Parameters

- **`array_field_list`** · `fn/v1, fn/v2,..., fn/vx` · *Required*

  The names of the array fields to be changed. You may set the array element spec to the first element number to be updated, or in the case of INS/DEL, the location to start the insertion/deletion.

- **`INS/DEL/ADD/SUB/CLR`** · `sac` · *Required*

  Options include:
  ADD - Add a value to an array of values. The type of the value being added needs to be the same as the type of the array field(s). The value specified will added the number of times specified, starting with the array element indicated in array_field_list. For example, if the following were true:
  Element #
  ————
  1
  2
  3
  4
  5
  
  FLD1
  ——
  250
  300
  100
  125
  190
  
  and the following were executed:
  UPDTA FLD1[2] ADD VAL 10 TIMES 3

- **`TIMES`** · `n` · *Required*

  The number of times to apply the operation.

- **`VAL`** · `n` · *Required*

  The value to be added (or used by the operation).
