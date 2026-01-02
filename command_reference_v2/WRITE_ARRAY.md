# WRITE ARRAY

## Summary
This command is used for writing to a group of records from array fields in memory. Within this single
command you can accomplish many tasks that would normally take many more lines of code and more
time to execute.
WRTA from_field_list TO to_field_list RECA record_number_array MAXA number_of_elements
CNTR counter_field FILE filename/@file_number FOR for_filter_expression DISP
from_field_list - f/c/e1, f/c/e2,..., f/c/ex - Required - The fields in the array whose values are to
be written. You may also create expressions that can be transferred to the receiving
(to_field_list) fields. The first from_field_list field is paired to the first to_field_list
field, the second to the second, etc. There must be the same number of
from_field_list fields as to_field_list fields.
to_field_list - fn/v1, fn/v1,..., fn/vx - Required - These are the recipient fields in the record to
be written. They must be of the same type as the respective from_field_list fields.
The first from_field_list field is paired with the first to_field_list field, the second to
the second, etc. There must be the same number of to_field_list fields as
from_field_list fields.
record_number_array - fn/v - Optional - If you want to update current records then you must
provide an array of record numbers so this command can read the appropriate record
before updating the fields and saving it back. The record number can be obtained
through the use of the RCN() function.
NOTE: If you read these records using the RDA (Array Read) command you may
make one of the from_field_list fields the function RCN() and the corresponding
to_field_list field an R type array field to hold the record number.
number_of_elements - f/c/e - Required - The number of array elements (or records) to be
written.
counter_field - fn/v - Optional - This value is updated by the program during operation so that
you may use expressions in the from_field_list. Use this field as the array specifier
where appropriate. Must be of type I.
filename/@file_number - file_expr - Optional - The name or number of the file to be used. If
you do not include this option, the program will look for a default search file set in the
SRCH (Search File) command. If a default file hasn’t been previously set, the
program will report an error and the command will be skipped. This entry will
override any default value set in the SRCH command.
for_filter_expression - lexpr - Optional - You would use this option to restrict the records
written in this command. If the expression did not resolve to .T., the record would not
be written and the program would continue to the next array element. The process
continues until all array elements (number_of_elements) have been checked and
written if appropriate.
DISP - Optional - If you include this option, each time a record is ready to be written the
program will redisplay the screen fields as appropriate. This will slow down the
operation of this command, with the amount of slow-down depending on how many
fields are displayed on the screen. The net effect will probably be negligible unless
you are reading through a very large file.

