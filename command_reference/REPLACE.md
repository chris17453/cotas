# REPLACE

## Summary
The programmer can use this command to change the values in fields in a file in some or all records.
REPL field_list WITH with_list MEM NUM number CNTR counter_field FILE filename/
@file_number KEY keyname/@key_number START start_value SCOPE scope
scope_value FOR for_filter_expression WHILE while_filter_expression DISP
field_list - fn/v1, fn/v2,..., fn/vx - Required - This is the list of fields whose values will be
replaced in this command.
If the MEM option is set then the program assumes that each field is an array field. In
this command you must use regular array fields. When used in this option you would
use just the FIELD portion, with no array element specified. The program will
automatically increment the array element number. You can specify the beginning
array element number by setting this value in the counter_field.
A maximum of 80 fields/expressions/constants or any combination thereof may be
included in a single field_list group.
with_list - f/c/e1, f/c/e2,..., f/c/ex - Required - These are the values that will be used as
replacements.
MEM - Optional - If you are replacing from an array in memory instead of a standard file
include this option in the command line.
number - fn/v - If the MEM option is included, this is Required - If MEM is specified then this
is the number of elements in the array to be replaced.
counter_field - fn/v - Optional - This is an I type field that will be used for passing the number
of records read, and the current array number, to your program. You can also use this
to count the number of records replaced.
filename/@file_number - file_expr - Optional - The name or number of the file to be used. If
you do not include this option, the program will look for a default search file set in the

