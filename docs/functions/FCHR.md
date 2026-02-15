# FCHR(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `FCHR` |
| **Returns** | `A` |

## Purpose

This function will return the first displayable (non-space) character in an A type field or the position value of that character.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The field to be checked. Must be an A type field. |
| 2 | `f/c/e` | If ‘C’ the function will return the character; if ‘L’ the location of the first character. |

## Return Type

A If ‘C’ the function returns a single A type character. I If ‘L’ the function returns an I type value.

## Comments

EXAMPLE x = ‘ 100.21’
? fchr(x,’c’) 
1
? fchr(‘
5

100.21’,’l’)

## Example

```tas
x = ‘ 100.21’
? fchr(x,’c’)
1
? fchr(‘
5

100.21’,’l’)
```

## Sample Program

`ETST.SRC, LSTCTEST.SRC`
