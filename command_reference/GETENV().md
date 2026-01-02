# GETENV()

## Summary
PURPOSE
This function will check for a matching value in the DOS environment area and, if one is found, will
return the characters after the =.

## Signature
```
PARTS
1
```

## Details
f/c/e
The environment variable name to search for. Don’t include the equal sign (=).
RETURN TYPE
A If a match is not found a null string will be returned.
EXAMPLE
? getenv(‘tas’)
D:\PROF\
Function Reference
GET_REC()
GET_REC(1,2)
PURPOSE
This function will return an individual record from a buffer that has been used to store an entire file.
Before using this function you should have read a non-TAS type file (general text or X type) into a field
in your program that you are using as a temporary buffer.
PARTS
1
fn/v
The name of the field you are using as a buffer.
2
f/c/e
The record number you want to retrieve. The first record in the buffer is 1.
RETURN TYPE
A The size of this field depends on the number of characters in the record. The termination
characters(s) at the end of the record is not returned as part of the record.
COMMENTS
Each record in the buffer must be terminated by a carriage return (CR) or a carriage return/line feed
pair (CR/LF) or a binary 0. If you attempt to get more records than are in the file, the function will
return a blank field with a length of 0. This also applies if there are no characters in the record to
return.
