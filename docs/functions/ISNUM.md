# ISNUM(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ISNUM` |
| **Returns** | `L` |

## Purpose

This function will determine whether the first character of a field is a numeric character (0-9), or
something else.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Field to check, must be of type A. |

## Return Type

L If the first character is from 0-9, the function will return .T., otherwise .F.

## Example

```tas
x = ‘x10’
? isnum(x)
.F.
x = ’10x’
? isnum(x)
.T.
```
