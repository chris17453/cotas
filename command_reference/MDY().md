# MDY()

## Summary
Function Reference

## Signature
```
MDY(1,2)
PURPOSE
This function returns a date value in the form of Month, Day, Year.
```

## Details
PARTS
1
f/c/e
Date value to use.
2
f/c/e
If set to ’S’, only the first three characters of the month value are used (e.g., Jan =
January).
RETURN TYPE
A The size of the string returned depends on the month.
EXAMPLE
? mdy(date(),’s’)
Jun 23, 94
