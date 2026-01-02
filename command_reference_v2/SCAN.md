# SCAN

## Summary
This command combines the work of a WHILE/ENDW loop and the FIND command into one super
command. This is actually the starting point of a control structure.
SCAN filename/@file_number KEY keyname/@key_number START start_value SCOPE scope
scope_value FOR for_filter_expression WHILE while_filter_expression DISP NLOCK REV
filename/@file_number - file_expr - Optional - The name or number of the file to be used. If
you do not include this option, the program will look for a default search file set in the
SRCH (Search File) command. If a default file hasn’t been previously set the
program will report an error and the command will be skipped. This entry will
override any default value set in the SRCH command.

