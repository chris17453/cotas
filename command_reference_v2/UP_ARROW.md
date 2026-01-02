# UP ARROW

## Summary
This command allows the programmer to control how far the user can go when s/he presses the Up
Arrow key.
UPAR test_udf GOTO goto_label
test_udf - udf - Optional - This is the UDF that would be tested if this command was executed
as the result of the user pressing the Up Arrow key. The UDF, if provided, must
return a logical value (.T. or .F.). If a UDF is not provided or if the UDF returns a .T.
value, the program will not progress above this command and the command immediately following will be executed.

