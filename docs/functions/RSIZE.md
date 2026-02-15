# RSIZE(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `RSIZE` |
| **Returns** | `I` |

## Purpose

This function will return the record size for a specified file.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The file_number for the specified file. |

## Return Type

I This is the maximum record size.

## Example

```tas
define hndl type i
openv ‘errmsg’ fnum hndl
? rsize(hndl)
324
```
