# SELECT

## Summary
This is a process control command, i.e., it will control whether a command, or group of commands will
be executed. This is one part of a complete command structure. This command is the first part of a
CASE structure.
SELECT expression
expression - f/c/e - Required - This must resolve to an integer type value that will be used as
the comparison value by each of the subsequent CASE commands.

## Signature
```
COMMENTS
For more information on the different structured programming commands, and the SELECT command
in particular, please see Chapter 7, Structured Programming Commands.
If you are missing an ENDC (End Case) command in a SELECT/CASE structure you will get the If
without Endif error message during the compile process.
```

