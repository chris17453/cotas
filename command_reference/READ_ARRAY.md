# READ ARRAY

## Summary
The command is used for reading a group of records into array fields in memory. Within this single
command you can accomplish many tasks that would normally take many more lines of code and time to
execute.
RDA from_field_list TO to_field_list FILE filename/@file_number
KEY keyname/@key_number START start_value SCOPE scope scope_value
FOR for_filter_expression WHILE while_filter_expression CNTR counter_field DISP
from_field_list - fn/v/e1, fn/v/e2,..., fn/v/ex - Required - The fields in the record that are being
read. You may also create expressions that can be transferred to the receiving
(to_field_list) fields. The first from_field_list field is paired to the first to_field_list
field, the second to the second, etc. There must be the same number of
from_field_list fields as to_field_list fields.
NOTE: You cannot use constants in the from_field_list but you can use expressions.
For example, you can read the record number for each record into an array of record
numbers by using the RCN() function as one of the from_field_list fields and the
appropriate R type array field as the corresponding to_field_list field.

