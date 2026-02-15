# EOF(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `EOF` |
| **Returns** | `L` |

## Purpose

This function will determine whether or not the program has tried to read a record that is past the end of the file.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The filenumber value of the file to be checked. |

## Return Type

L If the file is at EOF the function will return .T.; otherwise it will return .F.
