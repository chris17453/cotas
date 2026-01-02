# IMPORT

## Summary
This command will import records from a non-TAS file and save them to a TAS Pro 5.1 file. Within
this single command you can accomplish many tasks that would normally take many more lines of code
and more time to execute.
IMPORT to_field_list FROM from_file TYPE file_type
DLM delimiter_character MEM MAXA number CNTR counter_field
TO to_filename/@file_number SCOPE scope scope_value
FOR for_filter_expression DISP
to_field_list - fn/v1, fn/v2,..., fn/vx - Required - The fields in the record that are being imported to. These must be real fields and cannot be expressions.
from_file - f/c/e - Required - The name of the file you are importing from. Must include the
entire path, if any, and any extension. The program will use the exact name you enter.
file_type - f/c/e - Required - The type of file you are importing from. The options are:
D - dBASE III+
L - delimited
F - fixed length/SDF

