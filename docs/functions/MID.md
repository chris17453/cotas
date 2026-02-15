# MID(1,2,3,4)

| | |
|---|---|
| **Category** | Function |
| **Name** | `MID` |
| **Returns** | `A` |

## Purpose

This function will return a portion of a specified A type field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The value to be parsed. |
| 2 | `f/c/e` | The starting character position. The first character of an A type field is position 1. |
| 3 | `f/c/e` | The number of characters to return. If the value given would go beyond the end of the original field, only those characters up to the end of the field will be passed. |
| 4 | `f/c/e` | The memory area number. This may be from 1 through 4. If this is set properly, you do not have to specify part 1, the field to be parsed. However, you do still need to put the comma in for place keeping, i.e., =mid(,1,10,3) |

## Return Type

A If the receiving field is too short, the remaining characters will be truncated.

## Example

```tas
x = ‘ABCDEFG’
? mid(x,2,2)
BC
? mid(x,6,10)
FG
```
