# IF RECORD NOT ACTIVE

## Summary
This is a process control command, i.e., it will control whether a command, or group of commands, will
be executed. This is one part of a complete command structure.
IFNA filename/@file_number what_to_do goto_gosub_label
filename/@file_number - file_expr - Required - The name or number of the file to be
checked.
what_to_do - Required - The action to take if the record is not active. Possible actions
include:
DO - Do the following commands until the program reaches an ELSE, ELSE_IF or
ENDIF command. This is the default value and doesn’t have to be specified.
THEN - Execute the following command. The command to execute in this case must
follow on the same physical line.

