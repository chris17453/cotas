# DELETE RECORD

## Summary
Delete an active record for a specific TAS Pro 5.1 file.
DEL filename/@file_number NOCNF GOTO goto_if_no_del ERR error_label
filename/@file_number - f/c/e - Required - The name or number of the file that contains the
record to be deleted.
NOCNF - Optional - If this option is included in the command, the record is deleted without
asking for confirmation from the user. The default action is to ask before deleting the
record. This would be useful when you want to ask more than just "Delete? Y or N,"
or if you’re deleting records from multiple files and want to ask the Delete question
only once.
goto_if_no_del - label - Optional - If the user answers N to the Delete question (NOCNF is not
included), you can put a line label here signifying where the program should transfer
control. If this is not provided the program will continue with the next line.
error_label - label - Optional - If you specify a label here the program will transfer control to
the appropriate line if an error occurs during the command. If you don't put a label
here and an error occurs it will be displayed on the screen for the user. If you don't
want an error to be displayed then specify NO_ERR as the error_label.
NOTE: If an error occurs during the DEL command you can test for it with the
FLERR() function.

## Signature
```
COMMENTS
This command will only work with regular TAS Professional 5.1 files. Also, the record to be deleted
must be active. This means that it cannot have been cleared either through the CLR (Clear Record
Buffer) command or SAVE command.
```

## Details
PROGRAM EDITOR
fiLe -> Delete
