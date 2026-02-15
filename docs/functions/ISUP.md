# ISUP(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ISUP` |
| **Returns** | `L` |

## Purpose

This function will determine whether the first character of a field is a uppercase alphanumeric character
(A-Z), or something else.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Field to check, must be of type A. |

## Return Type

L If the first character is from A-Z (upper case), the function will return .T., otherwise .F.

## Example

```tas
x = ‘SAMPLE’
? isup(x)
.T.
x = ‘sample’
? isup(x)
.F.
```
