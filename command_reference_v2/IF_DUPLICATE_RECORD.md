# IF DUPLICATE RECORD

## Summary
This is a process control command, i.e., it will control whether a command, or group of commands, will
be executed. This is one part of a complete command structure.
IFDUP keyname what_to_do goto_gosub_label
keyname - sac - Required - The program checks the file to see if a record exists with the value
in the key field(s). If it does, the what_to_do action will be taken. If it does not and
the what_to_do action is not THEN, the program will look for the next ELSE_IF or
ELSE command. If one is not found, the program will transfer control to the line
following the appropriate ENDIF command.

