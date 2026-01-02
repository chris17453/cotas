# FLDTEST.SRC

## Summary
FCHR(1,2)
PURPOSE
This function will return the first displayable (non-space) character in an A type field or the position
value of that character.

## Signature
```
PARTS
1
```

## Details
f/c/e
The field to be checked. Must be an A type field.
2
f/c/e
If ‘C’ the function will return the character; if ‘L’ the location of the first character.
RETURN TYPE
A If ‘C’ the function returns a single A type character.
I If ‘L’ the function returns an I type value.
EXAMPLE
x = ‘ 100.21’
? fchr(x,’c’)
1
? fchr(‘
5
100.21’,’l’)
SAMPLE PROGRAMS
ETST.SRC, LSTCTEST.SRC
Function Reference
