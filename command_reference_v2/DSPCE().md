# DSPCE()

## Summary
PURPOSE
This function will return the amount of unused disk space on the current ‘default’ drive.

## Signature
```
NO OTHER PARTS
RETURN TYPE
R The value returned is in number of bytes. If there are 5 Mbytes remaining the returned value would
be: 5242880
```

## Details
DTOC(1,2)
PURPOSE
This function will convert a D type field into an A type field.
PARTS
1
f/c/e
The D type field to be converted. The original field value is not changed.
2
f/c/e
The size of the A type version of the date to return. The default is 8 (mm/dd/yy). You
can use any legal date size as the value.
RETURN TYPE
A If the receiving field is not long enough the returned value will be truncated.
