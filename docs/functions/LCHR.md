# LCHR(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `LCHR` |
| **Returns** | `A` |

## Purpose

This function will return the last displayable (non-space) character in an A type field or the position
value of that character.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The field to be checked. Must be an A type field. |
| 2 | `f/c/e` | If ‘C’ the function will return the character, if ‘L’ the location of the last character. |

## Return Type

A If ‘C’ the function returns a single A type character.
I If ‘L’ the function returns an I type value.

## Example

```tas
x = ‘ABCD ‘
? last_chr(x,’c’)
D
? last_chr(‘ABCD ‘,’l’)
4
```
