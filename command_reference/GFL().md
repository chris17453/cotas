# GFL()

## Summary
PURPOSE
This function will take a format line from a mounted report format and convert it to an alpha field with
all appropriate values set.

## Signature
```
PARTS
1
```

## Details
f/c/e
The number of the format line to use.
RETURN TYPE
A If the format line number is not accurate or if a report format has not been mounted, a blank value
will be returned. If the receiving field is not large enough to contain the entire line the remaining
characters will be truncated.
COMMENTS
This is the same as printing the report format line except the line is being placed in an A type field.
