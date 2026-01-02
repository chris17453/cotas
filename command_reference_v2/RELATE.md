# RELATE

## Summary
You would use this command to set a file as ‘related’ to another file. This means that when a record
from the ‘master’ file is found, the appropriate record will be found in the ‘slave’ file without any further
action from you.
REL slave_filename/@file_number KEY slave_keyname/@key_number MSTR
master_filename/@file_number FLDLST master_field_list
slave_filename/@file_number - file_expr - Required - The file name or number for the ‘slave’
file. The program will search this file for the appropriate record when a new ‘master’
record is found.
slave_keyname/@key_number - key_expr - Required - The key name or number in the
‘slave’ file. The program will use this index in searching for the appropriate record
when a new ‘master’ record is found.
master_filename/@file_number - file_expr - Optional - If a record is found in this file the
program will attempt to find the slave records. If this value is not included the relation
will be turned off.
master_field_list - fn/v1,f/c/e2,...,f/c/ex - Optional - The fields that contain the values to be
used in searching for the ‘slave’ record.

## Signature
```
COMMENTS
A maximum of 32 relations may be set up for all the programs. A list of all relations is kept and
checked each time a record is found. A slave for one relation may be a master for another. However,
you must be careful about creating an endless loop. For example, suppose the files in a master/slave
relationship are also set with the original slave now the master, and the original master now the slave.
When the master record (using the original relation) is found and the program finds the slave record, the
slave will now act like a master and will find the slave (the original master), which will ... an endless
loop.
Once a REL command has been executed it will remain active until the current program has been
QUIT. If you want to ‘turn off’ the relationship then execute the command with the Slave file and key
set properly but with the MSTR option (master file) blank.
Relations only affect the program in which they are set, whether or not the files are reopened (ROPEN)
in a subsequent program.
If a file is cleared and it is a master, the slaves will be cleared also. However, slaves will not be automatically saved or deleted; you must do that explicitly within the program.
```

## Details
EXAMPLE
Assume you have a customer file and a invoice file. You want to find the first invoice record, if any, in
the invoice file for any customer code entered by the user. The command to set up this relation might
be:
