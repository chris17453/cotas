# ALLOCATE FIELD

## Summary
Use this command to change a field previously defined within a program. This command will work only
with DEFINE’d fields. The command will allow you to change any part of the field: size, type, etc.,
even the number of array elements. It is generally used in situations where you don’t know how large a
field you might need and don’t want to take up space before it is needed. Also, you can DEALLOC
(Array Deallocate) an allocated field during runtime. This capability is useful when you know you need
to chain to other programs and might not have enough room if the field is at a set size.
ALLOC field_name TYPE type SIZE display_size DEC decimal_chrs
ARRAY num_array_elements UP up_case PICT picture
field_name - fn/v - Required - The name of the field being allocated. This field must be
previously defined within the program.
type - f/c/e - Required - The type of the field being allocated. Must conform to standard field
type requirements.
display_size - f/c/e - Optional - The display size of the field being allocated. Must conform to
standard field display size requirements. If this is not included then the default values
for the type of field are used.
decimal_chrs - f/c/e - Optional - If the field is of type N you can specify the number of decimal
characters. If this isn’t included the default value is 0. This could affect the displayed
value of the field or the ability to ENTER the correct value. Because this is true, you
should take great care to make sure the proper value is used.
num_array_elements - f/c/e - Optional - The number of array elements for this field.
up_case - f/c/e - Optional - If this is a type ‘A’ field you may specify whether all characters
entered to it are to be automatically forced into upper case. To accomplish this, make
this value ‘Y’.

