# CBYT(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `CBYT` |
| **Returns** | `B` |

## Purpose

This function will return the given field as a B type (1 byte integer) field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The field to be converted to B type. |

## Return Type

B The original field is not changed.

## Comments

F, P, T, and D type fields cannot be converted to B; however, all others can. If an L type field is converted, a .T. value will be converted to 1, a .F. to 0. If the value of the field to be converted is larger than the maximum value of a B type field (255), the converted value will not be accurate.
