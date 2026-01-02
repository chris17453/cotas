# LIST

## Summary
LIST (DOS only)
This command will list a file, or a group of array fields, to the screen, printer, or a disk file.
LIST field_list TTL title_list MEM NUM number CNTR counter_field
FILE filename/@file_number KEY keyname/@key_number
START start_value SCOPE scope scope_value FOR for_filter_expression
WHILE while_filter_expression PTO print_to PFN print_to_file NOFF
field_list - f/c/e1, f/c/e2,..., f/c/ex - Required - This is the list of fields to be used in this
command.
If the MEM option is set then the program assumes that each field is an array field.
Through the use of the counter_field you need not use just regular array fields.
However, if just standard array fields are to be used then you need not include any
array spec. For example, a normal array field would be used as:
FIELD[ARRAY_ELEMENT]. When using a standard array field in this option, you
would specify just the FIELD portion, and no array element. The program will
automatically increment the array element number. You can also use the
counter_field within the array expression. For example, if the counter_field is CNTR
then the current array element number can be accessed by using CNTR as the array
element field, i.e., FIELD[CNTR]. You can also specify the beginning array element
number by setting this value in the counter_field.
A maximum of 80 fields/expressions/constants or any combination thereof may be
included in a single field_list group.
title_list - f/c/e1, f/c/e2,..., f/c/ex - Optional - This list will be displayed after any automatic
internal top of form and as the first line printed by the command. The information
printed is not automatically lined up with the data so you need to be sure that it
displays properly.
MEM - Optional - If you are listing from an array in memory instead of a standard file include
this option in the command.
number - fn/v - If the MEM option is set, then this is Required - If MEM is specified then this
is the number of elements in the array to be listed.
counter_field - fn/v - Optional - This is an I type field that will be used for passing the number
of records read, and the current array number, to your program. You can also use this
to count the number of records listed.
filename/@file_number - file_expr - Optional - The name or number of the file to be used. If
you do not include this option, the program will look for a default search file set in the
SRCH (Search File) command. If a default search file hasn’t been previously set, the
program will report an error and the command will be skipped. This entry will
override any default value set in the SRCH command.
keyname/@key_number - key_expr - Optional - Set this to the appropriate value to list the
records in the file in the correct order. If you do not include this option, the program
will look for a default key set in the SRCH (Search File) command. If a default key
hasn’t been previously set the program will report an error and the command will be
skipped. This entry will override any default value set in the SRCH command.
start_value - f/c/e1,f/c/e2,...,f/c/ex - Optional - The value to use as the beginning record. This
is similar to doing a FIND before the command. If the index you’re searching on has
multiple segments you may list the proper values for each of the segments, separating

