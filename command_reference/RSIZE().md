# RSIZE()

## Summary
PURPOSE
This function will return the record size for a specified file.

## Signature
```
PARTS
1
```

## Details
f/c/e
The file_number for the specified file.
RETURN TYPE
I This is the maximum record size.
EXAMPLE
define hndl type i
openv ‘errmsg’ fnum hndl
? rsize(hndl)
324
