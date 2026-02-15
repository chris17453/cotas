# MIN(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `MIN` |
| **Returns** | `?` |

## Purpose

This function returns the minimum value of the two specified.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The first value to check, may be of any type other than P or F. |
| 2 | `f/c/e` | The second value to check, may be of any type other than P or F. Part 2 must be the same type as part 1. |

## Return Type

? This depends on the type of fields specified. The value returned will be of the same type.

## Example

```tas
? min(100,200)
100
? min(‘ABC’,’DEF’)
ABC
```
