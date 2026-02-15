# ISLO(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ISLO` |
| **Returns** | `L` |

## Purpose

This function will determine whether the first character of a field is a lowercase alphanumeric character
(a-z), or something else.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Field to check, must be of type A. |

## Return Type

L If the first character is from a-z (lower case), the function will return .T., otherwise .F.

## Example

```tas
x = ‘SAMPLE’
? islo(x)
.F.
x = ‘sample’
? islo(x)
.T.
```
