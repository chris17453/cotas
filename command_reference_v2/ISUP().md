# ISUP()

## Summary
PURPOSE
This function will determine whether the first character of a field is a uppercase alphanumeric character
(A-Z), or something else.

## Signature
```
PARTS
1
```

## Details
f/c/e
Field to check, must be of type A.
RETURN TYPE
L If the first character is from A-Z (upper case), the function will return .T., otherwise .F.
EXAMPLE
x = ‘SAMPLE’
? isup(x)
