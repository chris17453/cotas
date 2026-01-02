# LBLNME(1)

## Summary
PURPOSE
This function will return the name of the label for a given value.

## Signature
```
PARTS
1
```

## Details
f/c/e
The number of the label name to be found.
RETURN TYPE
A The name of the label.
COMMENTS
This function will work properly only if the program has been compiled with the -D (debug) flag.
LBL_LNE()
Function Reference
LBL_LNE(1)
PURPOSE
This function will return the internal (program) location for a given label number.
PARTS
1
f/c/e
The number of the label location to be found.
RETURN TYPE
I The internal location of the label.
LCHR(1,2)
PURPOSE
This function will return the last displayable (non-space) character in an A type field or the position
value of that character.
PARTS
1
f/c/e
The field to be checked. Must be an A type field.
2
f/c/e
If ‘C’ the function will return the character, if ‘L’ the location of the last character.
RETURN TYPE
A If ‘C’ the function returns a single A type character.
I If ‘L’ the function returns an I type value.
EXAMPLE
x = ‘ABCD ‘
? last_chr(x,’c’)
