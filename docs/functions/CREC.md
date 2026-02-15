# CREC(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `CREC` |
| **Returns** | `R` |

## Purpose

This function will return the given field as an R type (record or 4 byte integer) field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The field to be converted to R type. |

## Return Type

R The original field is not changed.

## Comments

An F type field cannot be converted to R; however, all others can. If an L type field is converted, a .T. value will be converted to 1, a .F. to 0.
