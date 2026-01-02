# BOF(1)

## Summary
PURPOSE
This function will determine whether or not the program has tried to read a record that is previous to the
beginning of the file.

## Signature
```
PARTS
1
```

## Details
f/c/e
The file_number value of the file to be checked.
RETURN TYPE
L If the file is at BOF the function will return .T.; otherwise it will return .F.
