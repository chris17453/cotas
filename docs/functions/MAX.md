# MAX(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `MAX` |
| **Returns** | `?` |

## Purpose

This function returns the maximum value of the two specified.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The first value to check, may be of any type other than P or F. |
| 2 | `f/c/e` | The second value to check, may be of any type other than P or F. Part 2 must be the same type as part 1. |

## Return Type

? This depends on the type of fields specified. The value returned will be of the same type.

## Example

```tas
? max(100,200)
200
? max(‘ABC’,’DEF’)
DEF
```
