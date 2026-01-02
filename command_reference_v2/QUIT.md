# QUIT

## Summary
Provide a process for leaving a program.
QUIT quit_level
quit_level - f/c/e - Optional - This is the program level to quit to. If this is not provided the
program will quit to the previous program. If there is no previous program you will be
returned to DOS.
Quit_level 0 will return you to DOS, 1 will return you to the first program, 2 to the
second, etc. Through the use of this option you can force the program to exit all the
way to DOS or to a specific program. All appropriate action at each level is taken so
that all files that need to be closed are closed, and all fields that need to be deallocated
are removed from memory.

## Signature
```
COMMENTS
executable line). (This doesn’t apply if the last executable line is a RET command in a subroutine or
UDF/UDC.) For example, the following program will exit automatically after the print command:
clrscr
? ‘this is a test’
However, if you want to control where the program exits then the QUIT command must be used. For
example, that same program with a subroutine would require the QUIT command:
clrscr
gosub prt_test
quit
prt_test:
? ‘this is a test’
ret
If there was a previous program (i.e., the program being quit was chained to), that previous program will
continue executing with the next line.
```

## Details
PROGRAM EDITOR
System -> Quit
