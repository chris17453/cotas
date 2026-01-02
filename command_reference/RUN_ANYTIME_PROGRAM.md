# RUN ANYTIME PROGRAM

## Summary
This command will put the name of a program into memory so that it can be executed later by a user
who presses a specific key.
RAP program_name NUM number WITH with
program_name - f/c/e - Required - The name of the program to run. This needs to include the
path also, if any.
number - f/c/e - Required - The RAP number to be assigned. May be from 1 to 10 and
corresponds to the keys ALT-1 through 10. For example, if number is 5, the user
would press the ALT key plus the 5 key at the same time to run the program.
with - fn/v1,fn/v2,...,fn/vx - Optional - A list of field values to be passed to the program. The
receiving program can test for these with the PARAM (Parameters) command.
NOTE: Do not use constants or expressions in this command since TAS Pro 5.1
clears that information out before chaining to the next program. You can, however,
pass pointers (P type only) to the receiving program so that you can access a value

