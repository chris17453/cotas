# REMARK

## Summary
This command allows the programmer to include a message or remark within the program that will have
no effect on the program.
REM statement
statement - sac - Optional - Anything you want. This command is ignored by the compiler and
is for your use only.

## Signature
```
COMMENTS
If the remark is going to extend over several lines then the REM command (or appropriate symbol)
must begin each line.
Several characters may be used instead of the REM command. The semicolon (;) or asterisk (*) may be
used instead of REM so long as it is the first non-space character on the line. For remarks following a
command on the same line, the programmer may use the double ampersand (&&). Some examples:
REM All of the following remarks are legal
; This is a legal remark
* This is also.
? ‘TEST’
&& This is a legal remark after a command.
The programmer cannot put a remark on a command line if the command is continued. For example:
? ‘THIS IS A TEST’+ \ && this is not a legal remark
‘ ALSO’
&& this is a legal remark
A remark cannot be put on the continued line since the continuation character (\) must be the last
character on the line. Please note that a continuation character is treated as such, even if it appears on a
line after a remark character.
```

