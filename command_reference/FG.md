# FG

## Summary
MID_REC(1,2,3)
PURPOSE
This function will return a portion of an active record in a specified file.

## Signature
```
PARTS
1
```

## Details
f/c/e
The file_number of the file to use.
2
f/c/e
The starting character position. The first character of a record is position 1.
3
f/c/e
The number of characters to return. If the value given would go beyond the end of the
record, only those characters up to the end of the record will be passed.
RETURN TYPE
A If the receiving field is too short, the remaining characters will be truncated.
MIN(1,2)
PURPOSE
This function returns the minimum value of the two specified.
PARTS
1
f/c/e
The first value to check, may be of any type other than P or F.
2
f/c/e
The second value to check, may be of any type other than P or F. Part 2 must be the same
type as part 1.
