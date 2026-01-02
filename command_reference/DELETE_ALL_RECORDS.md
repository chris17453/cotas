# DELETE ALL RECORDS

## Summary
You can use this command to delete all or a group of records from a TAS Professional 5.1 file.
DALL filename/@file_number KEY keyname/@key_number START start_value SCOPE scope
scope_value FOR for_filter_expression
WHILE while_filter_expression CNTR counter_field DISP
filename/@file_number - file_expr - Optional - The name or number of the file to be used. If
you do not include this option, the program will look for a default search file set in the
SRCH (Search File) command. If that hasn’t been previously set, the program will
report an error and the command will be skipped. This entry will override any default
value set in the SRCH command.
keyname/@key_number - key_expr - Optional - Set this to the appropriate value to delete the
records in the file in the correct order. If you do not include this option, the program
will look for a default key set in the SRCH command. If a default key hasn’t been
previously set the program will report an error and the command will be skipped.
This entry will override any default value set in the SRCH command.
start_value - f/c/e1,f/c/e2,...,f/c/ex - Optional - The value to use as the beginning record. This
is similar to doing a FIND before the command. If the index you’re searching on has
multiple segments you may list the proper value for each segment, separating them
with commas. This will allow you to specify the exact type for each of the segments.
For example, suppose you’re searching on an index that has two segments: a 5
character alpha and an I type field. Each record you want has the same alpha but the I
field will change, and is in order. In the first record the I type field has a value of 0.
The following would find the first record for that group, if it exists:
start ‘ABCDE’,0!
Notice that we separate the values with a comma. The only requirement is that the
values be of the same type (and size) as the segments in the key. In this case we put
the I type constant specifier (!) after the 0 to make sure that it is passed as an I type
field.

