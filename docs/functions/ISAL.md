# ISAL(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ISAL` |
| **Returns** | `L` |

## Purpose

This function will determine whether the first character of a field is an alphanumeric character (A-Z), or
something else.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Field to check, must be of type A. |

## Return Type

L If the first character is from A-Z (upper or lower case), the function will return .T., otherwise .F.

## Example

```tas
x = ‘SAMPLE’
? isal(x)
.T.
x = ‘!SAMPLE’
? isal(x)
.F.
```
