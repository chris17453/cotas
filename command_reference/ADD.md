# ADD

## Summary
This command will add fields to the internal field list. These fields may be a part of a file that has been
opened or just for standalone purposes. This file could have been specified in the original program or
could have been opened during the execution of the program. Once the field is added you can access it
almost as though it had been named in the original program. An ‘F’ type pointer is returned and by
using the field redirector (&) you can access that field for any purpose, including ENTER, =, etc.

## Signature
```
ADD field_name TYPE type SIZE display_size DEC decimal_chrs
ARRAY num_array_elements UP up_case PICT picture
FILE filename/@file_number KEY key_number OFST offset_in_file FPTR field_ptr
field_name - f/c/e - Required - The name of the field being added. Must conform to standard
field name requirements.
```

