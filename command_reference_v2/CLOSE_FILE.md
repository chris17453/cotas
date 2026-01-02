# CLOSE FILE

## Summary
This command is used to close both TAS and non-TAS files that have been previously opened using the
OPEN or OPENV (Open Variable) command.
CLOSE filename/@file_number DEL
filename/@file_number - file_expr - Required - The name or number of the file to be closed.
DEL - Optional - If the file being closed is non-TAS, you can include this option and the
program will delete the file from the disk.

## Signature
```
COMMENTS
If you don’t close the files before the program exits, it will be done automatically. Once a file is closed,
its ‘position’ can be used again; i.e., if one is closed another can be opened.
```

## Details
PROGRAM EDITOR
fiLe -> Open/close -> Close
