# LCKD()

## Summary
LCKD(1,2)
PURPOSE
This function will allow you to check if a record is locked by another user for a specific file/key value
without having to actually read the record.

## Signature
```
PARTS
1
```

## Details
f/c/e
The file_number of the file to be checked.
2
key_expr
The key number to be used.
RETURN TYPE
L If the record is locked the function will return .T., otherwise .F.
LIKE(1,2)
PURPOSE
This function will return a logical value indicating whether the two fields are alike. You may use the
skeleton characters ? and * in the first field.
PARTS
1
f/c/e
Comparison field 1. If you are going to use the skeleton characters (variable character
specifiers) ? or *, they must be in this field. Must be an A type value.
2
f/c/e
Comparison field 2. Must be an A type field.
RETURN TYPE
L If the fields match the function will return .T., otherwise .F.
COMMENTS
The character ? will match any other single character in that same position in field 2. The character *
will match all following characters from that position on. If comparison field 1 is shorter than field 2
and the last character in field 1 is NOT * the fields will not match and .F. will be returned. Differences
in upper and lower case are treated as differences in characters.
EXAMPLE
? like(‘ABC’,’abc’)
