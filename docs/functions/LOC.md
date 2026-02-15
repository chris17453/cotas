# LOC(1,2,3,4,5,6)

| | |
|---|---|
| **Category** | Function |
| **Name** | `LOC` |
| **Returns** | `I` |

## Purpose

This is an ‘instring’ function. It will return the location of a string of characters (A type field) within
another A type field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Field to check for, must be A type. |
| 2 | `f/c/e` | Field to check within, must be A type. |
| 3 | `f/c/e` | The character position where the function will start checking. The first position (far left) is 1. If this value isn’t provided the default value is 1. |
| 4 | `f/c/e` | The number of characters to check. If this value isn’t provided the function will check the the entire field (part 2). |
| 5 | `f/c/e` | If this is set to Y, the function will ignore differences in case (upper or lower). Default is N. |
| 6 | `f/c/e` | The memory area number. This may be from 1 through 4. If this is set properly, you do not have to specify part 2, the field to check within. However, you do still need to put the comma in for place keeping, i.e., =loc('abc',,10,300,,4) |

## Return Type

I The first character position of the matched field. If no match is found the return is 0.

## Example

```tas
x = ‘ABCDEFGHIJ’
? loc(‘CD’,x)
3
? loc(‘CD’,x,4,5)
0
define srch_fld type a size 10
srch_fld=’CD’
? loc(trim(srch_fld,’t’),x)
3
```
