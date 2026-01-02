# GOTO LINE

## Summary
This command will ‘permanently’ transfer control to a different part of the program. In this case the
program transfers control directly to a line number.
GOTOL goto_line_number
goto_line_number - label - Required - Transfer program control to the line as specified by the
number value.

## Signature
```
COMMENTS
This command, as opposed to the GOSUB, has no automatic way to return. You must use another
GOTO (or GOTOL) to return to the next command, if desired. Generally, this command is used only
in situations where you do not want to execute the lines following this command.
This command is not part of the regular ‘structured’ commands that a programmer would use in creating
a program. Much effort has been made in TAS Professional 5.1 so that this command need not be used
at all. However, it remains here for those times you find it the expedient choice.
NOTE: It is up to you to be sure that you pass a legal line number. If you do not the results are
uncertain and may stop the program entirely.
```

## Details
PROGRAM EDITOR
Prg control -> Goto/gosub -> goto Line
