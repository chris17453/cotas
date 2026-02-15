# DELF(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `DELF` |
| **Returns** | `L` |

## Purpose

This function will attempt to delete a file. If the deletion is successful, a logical .T. will be returned.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The name of the file to be deleted. This must include the path and extension in standard notation. |

## Return Type

L If the file is found and deleted the function will return a value of .T.; otherwise the returned value is .F.
