# V

## Summary
Logical
Pro 3.0 BCD
Overlay field

## Signature
```
1 byte
maximum 10 bytes
```

## Details
display_size - numeric constant - Optional - The display size of the field being added. Must
conform to standard field display size requirements. If this is not included, then the
default size value for the type of field is used.
NOTE: If the field is type A or O you must supply a value. V (overlay) type fields
are treated internally as A type fields so you must supply a size for these also. Internally, the overlay field will start with the next field in the list and continue for the
number of characters specified. See further information about the overlay field in
COMMENTS below.
decimal_chrs - numeric constant - Optional - If the field is of type N you can specify the
number of decimal characters. If this isn’t included the default value is 0. This could
affect the displayed value of the field or the ability to ENTER the correct value.
Therefore you should take great care to make sure the proper value is used. The
available range is 0 through 8.
num_array_elements - numeric constant - Optional - The number of array elements for this
field. Default value is 0.
picture - f/c/e - Optional - If this is a type ‘A’ field, you can use this option to enter a field into
a space that is shorter than the defined length of the field. This is called a slider field.
A slider is a field that shows fewer characters on the screen than actually exist in the
field. The user can see the other characters by pressing the LEFT and RIGHT
ARROW keys and the characters will appear to ‘slide’ back and forth across the
available space. This is very useful when you have several large fields and want them
all to fit on the same screen. A slider field is specified with an alpha constant of
"Sxx", where xx is the number of columns to display.
dup_field - fn/v - Optional - This option will force all of the fields in this particular DEFINE
command to be of the same type, size, decimal characters, and array elements as the
one specified from the File Field Data Dictionary. The benefit to this option is that if
you are using defined fields in a program and those fields are supposed to mimic fields
in a record, you no longer have to worry about making them match. Simply recompile
the programs if you make any changes, and the defined fields will always match their
DUP fields in the data dictionary.
UP - Optional - If this is a type A field you may specify whether all characters entered to it are
to be automatically forced into upper case. To accomplish this include this option.
RESET - Optional - If this option is part of this command the program will try to match the
fieldname up with the identical name in the previous program, if any. If it matches it
will be reset to the specifications for the field in the previous program. This means
that you can use that data in the new program just as though it was there normally.
When you’re done, and you return to the previous program any changes made to that
data are still there. This is an alternative to using the WITH option in the CHAIN
command and the PARAM command in the subsequent program. If the field cannot
be found in the previous program, or there is no previous program, the field is treated
like a normal field within that program. It is initialized and can be used, if desired.
