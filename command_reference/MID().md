# MID()

## Summary
3

## Signature
```
f/c/e
```

## Details
The number of characters to return. If the value given would go beyond the end of the
original field, only those characters up to the end of the field will be passed.
4
f/c/e
The memory area number. This may be from 1 through 4. If this is set properly, you do
not have to specify part 1, the field to be parsed. However, you do still need to put the
comma in for place keeping, i.e., =mid(,1,10,3)
RETURN TYPE
A If the receiving field is too short, the remaining characters will be truncated.
EXAMPLE
x = ‘ABCDEFG’
? mid(x,2,2)
