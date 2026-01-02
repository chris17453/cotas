# DEFINE FIELD

## Summary
This command is used to ‘create’ a field that will be used only within the current program (or subsequent programs that are called from this program). This is not a field that is part of a record in a
standard TAS Professional 5.1 file.
DEFINE field_name_list TYPE type SIZE display_size DEC decimal_chrs
ARRAY num_array_elements PICT picture DUP dup_field UP RESET LOCAL INIT
init_val
field_name_list - sac1,sac2,...sacx - Required - The name or names of the field(s) being
defined. Must conform to standard field name requirements. You may define a
maximum of 10 fields in one DEFINE command. Each of the fields will have the
same specifications.
type - sac - Optional - The type of the field being added. Must conform to standard field type
requirements. If you don’t specify a field type, the default value is N. The field name
types are:
Name
—————————
A Alphanumeric
N Numeric
D Date
T Time
I Integer
B Byte
F F type pointer
P P type pointer
R Record number

## Details
Internal size
———————————————
maximum 4 gbytes
8 bytes
4 bytes
4 bytes
2 bytes
1 byte
5 bytes
14 bytes
4 bytes
