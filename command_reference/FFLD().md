# FFLD()

## Summary
Function Reference

## Signature
```
RETURN TYPE
P, F or L
```

## Details
COMMENTS
If the return type is P or F, and you use a number and it is out of range, or the field name is not found,
the program will return an error. If the return type is L the function will return .F.
FILL(1,2)
PURPOSE
This function will return an A type field of a specific size and made up entirely of a specific character.
PARTS
1
f/c/e
The size of the field to be returned.
2
f/c/e
The fill character to be used. This must be an A type field and only the first character of
that field will be used.
EXAMPLE
? FILL(10,’*’)
**********
