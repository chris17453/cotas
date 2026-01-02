# DIFF()

## Summary
DIFF(1,2)
PURPOSE
Using a process similar to the SNDX() function, this function will return a value that can be used to
determine how ‘close’ the two values ‘sound.’

## Signature
```
PARTS
1
```

## Details
f/c/e
Comparison value 1. Must be type A.
2
f/c/e
Comparison value 2. Must be type A.
RETURN TYPE
I The return value is from 0 thru 4. 0 is no match at all; 4 is a very close match.
EXAMPLE
? DIFF(‘translate’,’trnslt’)
4
? DIFF(‘John’,’Paul’)
0
DMY(1,2)
PURPOSE
This function returns a date value in the form of Day Month Year.
PARTS
1
f/c/e
Date value to use.
2
f/c/e
If set to ‘S’ only the first three characters of the month value are used (e.g., Jan = January)
and only the last two digits of the year. If set to ‘L’ (long) then the full month name and
the full year are used.
RETURN TYPE
