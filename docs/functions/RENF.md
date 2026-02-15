# RENF(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `RENF` |
| **Returns** | `L` |

## Purpose

This function can be used to rename a file. It will return a value indicating whether or not the renaming was successful.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The current file name, must include full path and extension. |
| 2 | `f/c/e` | The new file name, must include full path and extension. |

## Return Type

L If the rename operation is successful the function will return .T., if not, .F..

## Comments

COMMENTS You may ‘move’ the file from one path to another by using the appropriate path values.
