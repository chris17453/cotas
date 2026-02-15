# ELOC(1,2,3,4)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ELOC` |
| **Returns** | `I` |

## Purpose

This is an ‘instring’ function. It will return the location of a string of characters (A type field) within another A type field, starting at the end of the field and working towards the beginning.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Field to check for, must be A type. |
| 2 | `f/c/e` | Field to check within, must be A type. |
| 3 | `f/c/e` | The character position to start checking at. The first position (far left) is 1. If this isn’t provided the default value is the end of the field. |
| 4 | `f/c/e` | The number of characters to check. If this isn’t provided the function will check the entire field (part 2). |

## Return Type

I The first character position of the matched field. If no match is found the value returned is 0.

## Comments

x = ‘ABCDEFGHCDIJ’
? loc(‘CD’,x)
9
? loc(‘CD’,x,3)
0

## Example

```tas
x = ‘ABCDEFGHCDIJ’
? loc(‘CD’,x)
9
? loc(‘CD’,x,3)
0
```

## Sample Program

`LOCTEST.SRC`
