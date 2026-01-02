# CINT()

## Summary
PURPOSE
This function will return the given field as an I type (2 byte integer) field.

## Signature
```
PARTS
1
```

## Details
f/c/e
The field to be converted to I type.
RETURN TYPE
I The original field is not changed.
COMMENTS
F, P, T, and D type fields cannot be converted to I; however, all others can. If an L type field is
converted, a .T. value will be converted to 1, a .F. to 0. If the value of the field to be converted is larger
than the maximum value of an I type field (65535), the converted value will not be accurate.
CLICKED_ON() (Windows only)
PURPOSE
This function returns whether or not the user has moved to a specific field by clicking on it with the
mouse.
NO OTHER PARTS
RETURN TYPE
L If the use has clicked on the field this function will return .T., otherwise, if normal program process
brought the user to this field it will return .F.
CLICKED_ON()
Function Reference
COMMENTS
Line numbers in a program run consecutively, so the line after the current one is CLNUM()+1.
