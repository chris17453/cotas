# LCKD(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `LCKD` |
| **Returns** | `L` |

## Purpose

LCKD()
LCKD(1,2)
PURPOSE
This function will allow you to check if a record is locked by another user for a specific file/key value
without having to actually read the record.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The file_number of the file to be checked. |
| 2 | `lexpr` | The key number to be used. |

## Return Type

L If the record is locked the function will return .T., otherwise .F.
