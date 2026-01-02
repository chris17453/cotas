# EDIT()

## Summary
window. At the bottom of the window the function creates two buttons, one is F10 To
Save Memo, and the other is ESC Exit without Saving. If the user clicks on the F10
button or presses the F10 key the function will put the new note/memo in the field
provided and returns .T. If the user presses the ESC key (or button) the function will
not change the field provided and returns .F.

## Signature
```
ELOC(1,2,3,4)
PURPOSE
This is an ‘instring’ function. It will return the location of a string of characters (A type field) within
another A type field, starting at the end of the field and working towards the beginning.
```

## Details
PARTS
1
f/c/e
Field to check for, must be A type.
2
f/c/e
Field to check within, must be A type.
3
f/c/e
The character position to start checking at. The first position (far left) is 1. If this isn’t
provided the default value is the end of the field.
4
f/c/e
The number of characters to check. If this isn’t provided the function will check the entire
field (part 2).
RETURN TYPE
I The first character position of the matched field. If no match is found the value returned is 0.
EXAMPLE
x = ‘ABCDEFGHCDIJ’
? loc(‘CD’,x)
9
? loc(‘CD’,x,3)
0
