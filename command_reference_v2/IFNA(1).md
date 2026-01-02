# IFNA(1)

## Summary
PURPOSE
This function will check if there is an active record for a specific file.

## Signature
```
PARTS
1
```

## Details
The file_number of the file to be checked.
f/c/e
RETURN TYPE
L If a record IS NOT ACTIVE for the file, the function will return .T., otherwise .F.
IIF(1,2,3)
PURPOSE
This function can return many different values depending on whether the logical expression provided
returns .T. or .F.
PARTS
1
lexpr
The expression to evaluate to determine what should be returned.
2
f/c/e
If the lexpr in part 1 evaluates to .T. the function will return this value.
3
f/c/e
If the lexpr evaluates to .F. the function will return this value.
RETURN TYPE
? The return type depends entirely on you.
COMMENTS
If you are only concerned about one type of return (i.e., only .T. or only .F.), the other part need not be
included.
EXAMPLE
x=1
? iif(x=1,,’False’)
(the return above will be blank (“”) )
? iif(x=1,’True’)
True
SAMPLE PROGRAMS
IIFTEST.SRC, IIFTEST2.SRC
Function Reference
