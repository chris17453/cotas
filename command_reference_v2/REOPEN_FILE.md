# REOPEN FILE

## Summary
Use this command to ‘attach’ a file opened in a previous program to the current program.
ROPEN file_number
file_number - f/c/e - Required - This is the number of the file in the previous program. This
value can be passed to a subsequent program through the use of the reset option in the
DEFINE command or as a PARAM (Parameter). Once the subsequent program has
the file_number value this command can be executed and the new program will be
able to access the file completely and in its current state.

