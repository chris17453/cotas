# ISTEST.SRC

## Summary
JUST(1,2)
PURPOSE
This function will return the field with the non-space characters moved to the left, right, or center.

## Signature
```
PARTS
1
```

## Details
f/c/e
The field to be justified, must be A type.
2
f/c/e
Which direction: ‘L’ - Left, ‘R’ - Right, or ‘C’ - Center.
RETURN TYPE
A The original field remains unchanged.
EXAMPLE
x = ‘ABCD ‘
? just(x,’r’)
‘ ABCD’
? just(x,’C’)
‘ ABCD ‘
