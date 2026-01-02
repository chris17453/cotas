# SEARCH FILE

## Summary
This command will set the default search file and key.
SRCH filename/@file_number KEY keyname/@key_number
filename/@file_number - file_expr - Required - This is the name or number of the file to be
used.
keyname/@key_number - key_expr - Required - Set this to the appropriate value to read the
records in the file in a particular order.
If you do not want to specify any key but want to search sequentially through the file,
set the null key. For example:
KEY @0 ( 0 is the number zero and not the letter ‘O’)

## Signature
```
COMMENTS
By using this command the programmer can set the default values to be used by any other command that
accesses data in files. This also includes the user operated file keys (F9 - Find, F5- First, F6 - Last, etc.).
If default values have not been set and the cursor is not on a key field that has the same name as a key,
the user will not be able to use the file keys. However, if you set the defaults, the program will use the
values for file key searches no matter where the cursor is. Using this process you can make sure that
only the file and key you want is used for searching. For more information on the file keys please refer
to Chapter 1, Installation and General Information.
To reset the SRCH value execute the command without a file or key specified. For example:
```

