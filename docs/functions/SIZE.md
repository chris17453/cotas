# SIZE(1,2,3)

| | |
|---|---|
| **Category** | Function |
| **Name** | `SIZE` |
| **Returns** | `I` |

## Purpose

This function will return the size of a specified field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The field being checked. |
| 2 | `f/c/e` | ‘I’ - Return the internal size. ‘D’ - Return the display size. ‘A’ - Return the actual size only if the field being checked is type A. |
| 3 | `f/c/e` | The memory area number. This may be from 1 through 4. If this is set properly, you do not have to specify part 1, the field to check. However you do still need to put the comma in for place keeping, i.e., =size(,'a',3) |

## Return Type

I

## Example

```tas
EXAMPLE
define x type n size 12 dec 2
x = 100.10
? size(x,’i’)
8
? size(x,’d’)
12
? size(x,’a’)
0
define x type a size 20
x = ‘ABCD’
? size(x,’i’)
20
? size(x,’d’)
20
? size(x,’a’)
4
```
