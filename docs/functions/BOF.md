# BOF(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `BOF` |
| **Returns** | `L` |

## Purpose

This function will determine whether or not the program has tried to read a record that is previous to the beginning of the file.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The file_number value of the file to be checked. |

## Return Type

L If the file is at BOF the function will return .T.; otherwise it will return .F.
