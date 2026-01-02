# CBYT()

## Summary
Function Reference

## Signature
```
RETURN TYPE
B The original field is not changed.
```

## Details
COMMENTS
F, P, T, and D type fields cannot be converted to B; however, all others can. If an L type field is
converted, a .T. value will be converted to 1, a .F. to 0. If the value of the field to be converted is larger
than the maximum value of a B type field (255), the converted value will not be accurate.
CC(1) (DOS only)
PURPOSE
This function changes the color temporarily to the value specified. May be used in a PRINT MESSAGE command only.
PARTS
1
f/c/e
The new color value.
RETURN TYPE
There is no value returned in this function. The only result is that the color value is temporarily
changed.
CCE() (DOS only)
PURPOSE
This function changes the color temporarily to error. May be used in a PRINT MESSAGE command
only.
NO OTHER PARTS
RETURN TYPE
There is no value returned in this function. The only result is that the color value is temporarily
changed.
CCF() (DOS only)
PURPOSE
This function changes the color temporarily to foreground. May be used in a PRINT MESSAGE
command only.
NO OTHER PARTS
Function Reference
