# ENDIF.

## Summary
Each IF command must have a corresponding ENDIF to finish it off. For more
information please refer to Chapter 7, Structured Programming Commands.
This can also occur if you have forgotten an ENDC (End Case) command at the end
of a SELECT/CASE structure.

## Signature
```
564
```

## Details
There have been 1 or more WHILE commands without corresponding ENDWHILE.
Each WHILE command must have a corresponding ENDW (Endwhile) to finish it
off. For more information please refer to Chapter 7, Structured Programming
Commands.
565
There have been 1 or more SELECT commands without corresponding ENDCASE.
Each SELECT command must have a corresponding ENDC (Endcase) to finish it
off. For more information please refer to Chapter 7, Structured Programming
Commands.
566
There have been 1 or more FOR commands without corresponding NEXT.
Each FOR command must have a corresponding NEXT to finish it off. For more
information please refer to Chapter 7, Structured Programming Commands.
Compiler Errors
567
The expression is too complex. Either increase the number of internal fields, or split it into two operations.
569
The comparison type was not acceptable.
Please see Chapter 1, Installation and General Information for the comparison
operators.
570
The parentheses are unbalanced in the expression.
Each expression in which parentheses are used must have an equal number of ‘(‘ and
‘)’. Make sure NOT to surround a single value with parentheses, i.e.:
(4) or (counter)
571
There is an error in your expression. Probably a single
number/field with parentheses around it.
(1) or (‘abc’) or (test) are not legal expressions. These expressions can be stated
without parentheses.
572
The function in the expression is not a legal TAS Professional 5.1 function.
If this is a UDF the programmer must be sure that the #UDF compiler directive has
been set before the function is used in an expression.
574
You must have a receiving field in this command.
575
You must have a file name in this command.
580
You have specified too many #INC (include) files in the
program. The maximum allowed is 20.
581
FILEDICT.B was not found.
Make sure that the TAS50= value is set. For more information please see Chapter 1,
Installation and General Information.
582
FILELOC.B was not found.
Make sure that the TAS50= value is set. For more information please see Chapter 1,
Installation and General Information.
583
FILEKNUM.B was not found.
Make sure that the TAS50= value is set. For more information please see Chapter 1,
Installation and General Information.
585
No memory space is available to allocate the Source file
buffer.
586
No memory space is available to allocate the Constant
buffer.
587
No memory space is available to allocate the Field buffer
Compiler Errors
588
No memory space is available to allocate the Label buffer
589
No memory space is available to allocate the Compiled
program buffer
590
No memory space is available to allocate the Specification line buffer
591
No memory space is available to allocate the Screen/
Report source file buffer
592
No memory space is available to allocate the Compiled
Screen/Report buffer
593
The maximum size for field name and array is 60 characters.
The field name cannot be more than 15 characters; however, the array element
specifier can be an expression.
594
The option entered was not found.
595
The option entered is not allowed in this command.
596
*** Internal Error ***
Call support.
598
Error while opening error file.
599
The Constant segment has grown larger than the memory
allocated. For more information please refer to the
manual under Chapter 4, Compiler Information.
600
The Code segment has grown larger than the memory allocated. For more information please refer to the manual
under Chapter 4, Compiler Information.
601
The Spec Code segment has grown larger than the memory
allocated. For more information please refer to the
manual under Chapter 4, Compiler Information.
602
You have too many different named fields. For more information please refer to the manual under Chapter 4,
Compiler Information.
603
You have used too many different files.
A maximum of 32 files may be used in a program.
605
You have reached the maximum nesting level for the IF
command.
The IF/ENDIF structure can be nested 20 deep. For more information please refer to
Chapter 7, Structured Programming Commands.
Compiler Errors
606
You have reached the maximum nesting level for the WHILE
command.
The WHILE/ENDW structure can be nested 20 deep. For more information please
refer to Chapter 7, Structured Programming Commands.
607
You have reached the maximum nesting level for the SELECT/CASE command.
The SELECT/CASE/ENDC structure can be nested 20 deep. For more information
please refer to Chapter 7, Structured Programming Commands.
608
You have reached the maximum nesting level for the FOR
command.
The FOR/NEXT structure can be nested 20 deep. For more information please refer
to Chapter 7, Structured Programming Commands.
609
** WARNING ** The field you are trying to add has already been DEFINEd. You cannot define the same field
twice. This DEFINE command will be ignored.
In TAS Professional 5.1 only the first DEFINE in the program will be effective. This is just a warning
message and will not keep the program from running. The line number of the
subsequent DEFINE will be displayed. You should check the program and make
sure the field is being properly defined.
610
The maximum number of fields you may allocate is 2000.
611
You have tried to add more fields than you have allocated. For more information please refer to the manual
under Chapter 4, Compiler Information.
612
The field name specified was too long.
613
The field name is illegal.
chr from A thru Z.
614
You have specified an unknown field type. The only acceptable values are: A / N / D / T / I / B / P / R / L /
