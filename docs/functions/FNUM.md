# FNUM(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `FNUM` |
| **Returns** | `I` |

## Purpose

This function returns the file_number for a file opened with the OPENV (Open Variable) command.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The name of the file. This needs to be in standard alpha notation or as an A type field (e.g., ‘BKARCUST’). |

## Return Type

I Will return the file_number if found. If it is not found, the function will return 0.

## Comments

This function will allow you to use the old form OPEN command and still make use of certain functions that require the file_number instead of the file name.

## Example

```tas
This function can be embedded in any other function so that you don’t need to keep the file_number in another field. For example:
? flerr(fnum(‘BKARCUST’))
0
? eof(fnum(‘BKARCUST’))
.F.
```
