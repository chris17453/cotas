# WRAP()

## Summary
Whether or not to trim the trailing spaces at the end of the block. This will allow the next
line to be output immediately after the end of the last non-space line.
‘Y’ - trim trailing spaces. This is the default value.
‘N’ - don’t trim spaces.

## Signature
```
RETURN TYPE
A If the receiving field is too short, the remaining characters will be truncated.
```

## Details
COMMENTS
You may have up to 10 WRAP() functions on the same print line. Each one will print the appropriate
number of lines.
In the default mode the program will trim any trailing blanks in the field so that blank lines are not
printed. However, a minimum number of characters will be printed within the space for the block even
if there are not enough characters to fill a single line. If you want all the lines to print, blank or not, set
option 3 (trim trailing spaces) to ‘N’.
Also, note that any subsequent lines will start at the same starting column number as the first line.
EXAMPLE
field1 = ‘abcd’
field2 = ‘now is the time for all good men to come to the aid of their party.’
field3 = 100.00
? field1,’ ‘,wrap(field2,20),’ ‘,field3
abcd now is the time for 100.00
all good men to come
to the aid of their
party.
SAMPLE PROGRAMS
WRAPTEST.SRC, WRPTEST2.SRC
