# SAVE RECORD

## Summary
This command is used to save records to any file, TAS or non-TAS.
SAVE filename/@file_number NOCNF NOCLR GOTO goto_label ERR error_lbl
filename/@file_number - file_expr - Required - This is the name or number of the file that
will receive the record.
NOCNF - Optional - The normal process is for the program to confirm that it is okay to save
the record. If you do not want the program to confirm, include this option in the

