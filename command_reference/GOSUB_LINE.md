# GOSUB LINE

## Summary
This command will temporarily transfer control to a subroutine. In this case the program transfers
control directly to a line number.
GOSUBL gosub_line_number
gosub_line_number - label - Required - Transfer program control to the line as specified by
the number value.

## Signature
```
COMMENTS
A subroutine ‘called’ through this approach must use the RET (Return) command when finished.
Unlike a function, no data may be returned. The program will continue with the next line after the
original GOSUBL (once the RET command has been executed from the subroutine).
NOTE: It is up to you to be sure that you pass a legal line number. If you do not the results are
uncertain and may stop the program entirely.
```

## Details
PROGRAM EDITOR
Prg control -> Goto/gosub -> gosub linE
