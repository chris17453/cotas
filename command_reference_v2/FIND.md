# FIND

## Summary
Use this command to find a record in any TAS file opened with the OPEN command.
FIND find_type SRCH key_or_file_name REL related_field ERR error_label
FOR for_filter_expression NLOCK KEYO NOCLR
find_type - mgflnpr - Required - This is the type of FIND you want to execute. The options
are:
M - matching - Find the exact record based on the search_value option. You can also
set the appropriate key fields with the EQUAL command and then the
search_value option need not be used.
G - generic - Same as match however the program will return the exact or next
greatest record.
F - first - Find the first record in the file.
L - last - Find the last record in the file.
N - next - Find the next record. This assumes that you have done a search previously
that successfully found a record in the file (exact, generic, first, etc.).
P - previous - Find the previous record. This assumes that you have done a search
previously that successfully found a record in the file (exact, generic, last,
etc.).
R - related - In this type of search, the program will use the value in the related_field
when searching for the record. NOTE: There is a process in TAS Pro 5.1
where you can set relations between files outside of the FIND command. It
is recommended that you use that method. Then TAS Pro 5.1 will automatically search for related records whenever a record is found using any method.
key_or_file_name - sac - Required - If this is a type N or P (next or previous) search, then this
value is the name of the file being searched in special alpha constant form (i.e., the
true name of the file without quotes). Otherwise this is the name of the key being used
for the search, also in special alpha constant form.
related_field - fn/v - The name of the field to use as the search value for this command.
NOTE: There is a process in TAS Pro 5.1 where you can set relations between files
outside of the FIND command. It is recommended that you use that method. Then
TAS Pro 5.1 will automatically search for related records whenever a record is found
using any method.
error_label - label - Optional - If you specify a label here, the program will transfer control to
the appropriate line if an error occurs during the command.

