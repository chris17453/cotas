# INSERT

## Summary
Use this command to replace temporary place-keeping symbols in an A type field with other A type
fields. The general use of this command will be in inserting names, addresses, amounts, etc. into stock
paragraphs, letters, etc. that can then be printed with this ‘personal’ information in it.
INSERT field_list INTO into_field
field_list - f/c/e1, f/c/e2,..., f/c/ex - Required - The fields/expressions that will be used when
replacing the place-keeper values. The first field will replace place-keeper 1, the
second field place-keeper 2, etc. Each INSERT value must be of A type. The placekeeper value is ‘\*’+number, i.e., \*1 or \*2, etc. Each INSERT field may be used
multiple times within the into_field.
into_field - fn/v - Required - The name of the field that contains the place-keeping values that
need to be replaced. Must be an A type field.

