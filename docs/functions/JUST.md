# JUST(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `JUST` |
| **Returns** | `A` |

## Purpose

This function will return the field with the non-space characters moved to the left, right, or center.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The field to be justified, must be A type. |
| 2 | `f/c/e` | Which direction: ‘L’ - Left, ‘R’ - Right, or ‘C’ - Center. |

## Return Type

A The original field remains unchanged.

## Example

```tas
x = ‘ABCD ’
? just(x,’r’)
‘ ABCD’
? just(x,’C’)
‘ ABCD ’
```
