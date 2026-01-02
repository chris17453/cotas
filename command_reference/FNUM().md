# FNUM()

## Summary
PURPOSE
This function returns the file_number for a file opened with the OPENV (Open Variable) command.

## Signature
```
PARTS
1
```

## Details
f/c/e
The name of the file. This needs to be in standard alpha notation or as an A type field
(e.g., ‘BKARCUST’).
RETURN TYPE
I Will return the file_number if found. If it is not found, the function will return 0.
COMMENTS
This function will allow you to use the old form OPEN command and still make use of certain functions that require the file_number instead of the file name.
EXAMPLE
This function can be embedded in any other function so that you don’t need to keep the file_number in
another field. For example:
? flerr(fnum(‘BKARCUST’))
0
? eof(fnum(‘BKARCUST’))
