# CHAIN

## Summary
Use this command to execute another TAS Professional 5.1 program.
CHAIN program_name WITH with NOBASEWIND
program_name - f/c/e - Required - The name of the program to run. This needs to include the
path also, if any.
with - fn/v1,fn/v2,...,fn/vx - Optional - A list of field values to be passed to the program. The
receiving program can test for these with the PARAM (Parameters) command.
NOTE: Do not use constants or expressions in this command since TAS Pro 5.1
clears that information out before chaining to the next program. You can, however,
pass pointers (P type only) to the receiving program so that you can access a value

