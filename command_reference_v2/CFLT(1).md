# CFLT(1)

## Summary
PURPOSE
This function will return the given field as an N type (floating point) field.

## Signature
```
PARTS
1
```

## Details
f/c/e
The field to be converted to N type.
RETURN TYPE
N The original field is not changed.
COMMENTS
An F type field cannot be converted to N; however, all others can. If an L type field is converted, a .T.
value will be converted to 1.00, a .F. to 0.00.
Function Reference
