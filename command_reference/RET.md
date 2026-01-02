# RET

## Summary
LOC(1,2,3,4,5,6)
PURPOSE
This is an ‘instring’ function. It will return the location of a string of characters (A type field) within
another A type field.

## Signature
```
PARTS
1
```

## Details
f/c/e
Field to check for, must be A type.
2
f/c/e
Field to check within, must be A type.
3
f/c/e
The character position where the function will start checking. The first position (far left)
is 1. If this value isn’t provided the default value is 1.
4
f/c/e
The number of characters to check. If this value isn’t provided the function will check the
entire field (part 2).
5
f/c/e
If this is set to Y, the function will ignore differences in case (upper or lower). Default is
