# USER DEFINED COMMAND

## Summary
When you want to use a UDC defined elsewhere in your program (using the CMD command) or in a
separate library all you need to do is put the command name on the line followed by the values to be
passed to the routine.
cmd_name cmd_options
cmd_name - sac - Required - The name of the UDC.
cmd_options - f/c/e1,f/c/e2...f/c/ex - Optional - The values to be passed to the UDC. There
should be at least one space between the cmd_name and the cmd_options. Separate
the options with commas. Do not surround them with parentheses as you would a
UDF or function.

## Signature
```
COMMENTS
This is how you will actually execute the UDC. You set up the UDC using the CMD command. See the
details under COMMAND.
```

## Details
PROGRAM EDITOR
System -> Programming -> do udC
