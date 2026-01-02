# PERR

## Summary
PG_BRK

## Signature
```
MOUSE_MOV
MOUSE_LBD
MOUSE_LBU
MOUSE_RBD
MOUSE_RBU
```

## Details
a file is attempted to be opened that is locked in full by
another program. For more information see below.
Executes trap if record is locked and user answers N (No)
to system's offer to retry. See below.
Executes trap if an error occurs while reading a file. This
may not have any effect in a command that accesses a
complete file, such as DELETE_ALL, since the command
will exit normally once an error occurs.
Executes trap if an error occurs during the execution of a
program. Normally if a program error occurs the error
message will be displayed and the program will continue
executing. If this trap is set the error message will not be
displayed.
Executes trap if during a printing routine the internal line
position counter should exceed the ‘printable lines’ for the
appropriate device. This is generally used so that the
programmer can output the correct header lines at the top
of the page or screen.
Executes trap if mouse is moved.
Executes trap if left mouse button is pressed.
Executes trap if left mouse button is released.
Executes trap if right mouse button is pressed.
Executes trap if right mouse button is released.
Default Key Operations
