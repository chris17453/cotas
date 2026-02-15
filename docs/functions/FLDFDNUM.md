# FLDFDNUM(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `FLDFDNUM` |
| **Returns** | `I` |

## Purpose

This function returns the file handle, if any, for the field named in the function.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | An F type pointer field that is pointing at the appropriate field. |

## Return Type

I

## Comments

If the field isn’t in a file or you haven’t used an F type pointer the returned value will be 0.
