# GFLD()

## Summary
Function Reference

## Signature
```
GFLD(1,2,3,4)
PURPOSE
This function returns a field from a delimited file buffer previously read with the READ or FINDV
(Find Variable) command.
```

## Details
PARTS
1
f/c/e
The field number in the delimited record. The first value in the record is field 1.
2
f/c/e
The file_number for the file that contains the delimited records. You must provide a
file_number or provide a field or constant in part 4.
3
f/c/e
The delimiter between fields in the buffer. If this value is not provided the program
assumes a comma ‘,’.
4
f/n/v
A field to be used instead of a file_number.
RETURN TYPE
