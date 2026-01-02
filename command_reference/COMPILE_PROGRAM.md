# COMPILE PROGRAM

## Summary
COMPILE PROGRAM (DOS only)
Use this command to compile a TAS Pro 5.1 source code file from within another TAS Pro 5.1
program.
COMPRG source_code_file PTO messages_where ERR error_field DEBUG
DICT dictionary
source_code_file - f/c/e - Required - The name of the source file to be compiled. If the file
isn’t in the current path, the proper path must be part of the file name. For example:
COMPRG ‘D:\TAS50\CUSTCODE’.....
where the program being compiled is CUSTCODE.SRC
NOTE: If you are compiling an EDT file you must include the extension or the
program will think it is an SRC type source file. In the example above it would be:
COMPRG ‘D:\TAS50\CUSTCODE.EDT’
messages_where - pd - Optional - This will tell the compiler to print the results somewhere
other than the screen. If you choose D (disk) the program will create a file with the
same name as the program being compiled, using the same path, with an extension of
.ERR. If the INT option is set the file name will be set to INTERNAL. If you choose
P (printer) the information will be sent to the default printer as set in TAS50.OVL.
error_field - fn/v - Optional - The program will return the number of errors that occur during
the compile process into this field. Must be of I type.
DEBUG - Optional - If this option is part of the command, the compiler will include information so that finished program can display the program name and actual line number
during any error or help message.
dictionary - f/c/e - Optional - You may specify an alternate dictionary path here if desired. It
will override all others, including that in the TAS50.OVL file.

## Signature
```
COMMENTS
Each program must be compiled before it can be executed by the TAS Pro 5.1 runtime. By being able
to compile a ‘field’ and return it to another ‘field’ you can create ‘one time use’ programs on the fly.
Through the ability to read and write non-TAS files using the standard OPENV (Open Variable)
command, etc., you can create programs and still save them to disk, if desired.
```

## Details
PROGRAM EDITOR
System -> Programming -> compile Prog
SAMPLE PROGRAMS
COMPTST, COMPTST2
