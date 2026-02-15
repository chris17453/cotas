# FFLD(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `FFLD` |
| **Returns** | `P,` |

## Purpose

This function will return the pointer (P or F) for a field. The function can also return a logical value to indicate that the field was not found.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The name or number of the field to search for. |
| 2 | `f/c/e` | What to return. ‘P’ returns a P type pointer, ‘F’ returns an F type pointer, and ‘L’ returns a logical value. |

## Return Type

P, F or L

## Comments

If the return type is P or F, and you use a number and it is out of range, or the field name is not found, the program will return an error. If the return type is L the function will return .F.
