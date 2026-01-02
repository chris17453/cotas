# SET ACTIVE

## Summary
If you have two or more TAS Pro 5.1 files that all use the same buffer (i.e., the same file descriptor
name or schema), and you need to switch from one to the other when searching for a particular record
(FIND command), then you need to use this command to make the switch.
SETACT file_descriptor_name FILE file_name
file_descriptor_name - sac - Required - The FD name (or schema for 3.0 users) for the files to
be switched. The name can be a maximum of 8 characters. Must be the actual FD
name.
file_name - sac - Required - The name of the file that needs to be SETACT. The name can be
a maximum of 8 characters. Must be the actual file name.

## Signature
```
COMMENTS
We recommend that you use the OPENV (Open Variable) and FINDV (Find Variable) commands
which obviate the need for this command. However, the SETACT command is included here for
compatibility with TAS Pro 3.0.
```

## Details
PROGRAM EDITOR
fiLe -> Open/close -> Set active
