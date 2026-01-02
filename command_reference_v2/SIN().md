# SIN()

## Summary
Function Reference
EXAMPLE
? sin(1.539)
0.9994954

## Signature
```
SIZE(1,2,3)
PURPOSE
This function will return the size of a specified field.
```

## Details
PARTS
1
f/c/e
The field being checked.
2
f/c/e
‘I’ - Return the internal size.
‘D’ - Return the display size.
‘A’ - Return the actual size only if the field being checked is type A.
3
f/c/e
The memory area number. This may be from 1 through 4. If this is set properly, you do
not have to specify part 1, the field to check. However, you do still need to put the
comma in for place keeping, i.e., =size(,'a',3)
RETURN TYPE
