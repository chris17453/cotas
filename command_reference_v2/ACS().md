# ACS()

## Summary
PURPOSE
This function will return the name of the current alternate collating sequence file, if any.

## Signature
```
NO OTHER PARTS
RETURN TYPE
A If there is no ACS file active the function will return NONE.
```

## Details
AEV(1,2)
PURPOSE
This function will return an element value from an array field.
PARTS
1
fn/v
The array field itself. You may use a P type field to point to the array field.
2
f/c/e
The element number.
RETURN TYPE
The function will return the same type of field as the array field (option 1).
ALC_FLD(1,2)
PURPOSE
You can use this function to allocate a single previously defined field.
PARTS
1
fn/v
The field to allocate. This must be a type A field.
2
f/c/e
The new desired size. Maximum size possible is 65,535. If the amount of available
memory is less than the size given the program will be allocated as much as possible.
ALC_FLD()
Function Reference
RETURN TYPE
I The actual size allocated.
COMMENTS
The field (part 1) used in this command cannot be a part of a standard TAS Professional 5.1 file. It
must have been created using the DEFINE command.
Once this function has been used to allocate the field the data space originally used by the field is lost to
use. Due to this you should define the field with a size of 1 initially. This keeps the amount of wasted
space to a minimum.
ALOC(1,2,3)
PURPOSE
You can use this function to find an array element number based on a value you specify.
PARTS
1
f/c/e
The field value to search for.
2
fn/v
The array field to search.
3
f/c/e
The array element value to start with. If none is provided the program will start with the
first.
RETURN TYPE
I The element number if found.
COMMENTS
If there is a match for the search field value the program will return the array element number. If no
match is found the return value will be 0.
NOTE: Do not specify an array element in the array field to search for (part 2). If you wish to start
with an element other than 1 specify that value in part 3.
ALOCARY(1,2)
PURPOSE
You can use this function to allocate an array for a previously defined field.
PARTS
1
fn/v
The field to allocate array elements for.
Function Reference
2
f/c/e
