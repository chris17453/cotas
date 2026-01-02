# LOW(1)

## Summary
PURPOSE
This function returns the A type field specified in lower case characters.

## Signature
```
PARTS
1
```

## Details
f/c/e
The value to use. The original value is left unchanged.
RETURN TYPE
A If the receiving field is too short, the remaining characters will be truncated.
EXAMPLE
? low(‘ABcdEFG’)
abcdefg
