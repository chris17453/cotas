# PROFESSIONAL

## Summary
VERSION 5.1

## Signature
```
Chapter 6
Function Reference
```

## Details
INTENTIONALLY BLANK
Function Reference
GENERAL REFERENCE INFORMATION
CONVENTIONS USED IN THE FUNCTION REFERENCE SECTION
The following format is used in documenting the TAS Professional 5.1 Functions.
•
The functions are listed in alphabetical order.
•
The PURPOSE of each function is a brief definition of what it does. This provides a quick
insight into the function, and why the programmer might use it.
•
The PARTS of each function are listed by number.
Each function part is described, along with its purpose, valid values, default value (if applicable), consequences, and whether or not the part is required.
The programmer entry portion can consist of the following:
f/c/e - Field/Constant/Expression. You can use any type of field or constant. This can consist
of alpha or numeric constants as well as any type of field or variable field. You can
also use an expression that results in the proper type of field.
fn/v - Field name/Variable field. In certain commands/options you must use the actual field
name or a variable field.
key_expr - You may define the key number to be used in a command in several ways:
number - The actual number of the key, as defined in the data dictionary, may be
used preceded by an ampersand (‘@’). @3 refers to the third key in the
appropriate file (the first key is 1).
key name - The name of the key as assigned in the data dictionary may be used.
dict_overlay is an example of a key name.
no key - If you do not want to use a key but want to access the file through the direct
method then use the no key symbol, the number 0 preceded by an ampersand (‘@’). @0 tells the program to use the direct access method when
searching this file. This is the default method when you have opened a nonTAS file.
lexpr - An expression that will resolve to a logical value of .T. or .F. For example:
fld1 = 100
another example:
(fld1 = 100 .a. fld2=’ABC’) .o. fld3=.F.
•
The RETURN TYPE and what is being returned is given.
•
Special COMMENTS or restrictions are given where applicable.
•
SAMPLE PROGRAM will list the name of a file containing a program that demonstrates the
function in action.
•
EXAMPLES will give the function followed immediately below by the result.
