# Compiler Errors

Total: 121

## Error 500

The source file above was not found. Make sure the file
has the correct extension and that you have entered the
drive and path correctly.
The default source extension is .SRC.

## Error 501

The Screen/Report format was not found.
If the EXTERN option is set in the MOUNT command or if the #EXT_FMT
compiler directive is set the compiler looks for the screen/report formats on the disk
in the same sub-directory as the source code.

## Error 504

An error has occured while reading the Screen/Report
format file.
A DOS file error occured.

## Error 512

The line is longer than 1024 characters, or the cr/lf
pair was not found. This could also be caused by a binary 0 within the source line.
The maximum length of any source code line is 1024 characters.

## Error 513

There has been an error discovered in the source file.
This is a DOS file error problem.

## Error 527

Number of fields in program:
The compiler will return the number of named fields used in the program.

## Error 528

Number of Line Labels:
The compiler will return the number of line labels named in the program. UDC and
UDF names are treated as line labels.

## Error 529

Size of Code segment (bytes):
This is the run code buffer as explained in Chapter 4, Compiler Information.

## Error 530

Size of Constant segment (bytes):
This is the constant buffer as explained in Chapter 4, Compiler Information.

## Error 531

Size of Spec Code segment (bytes):
This is the spec line buffer as explained in Chapter 4, Compiler Information.

## Error 532

Size of DEFINE Data (bytes):
This is the total internal size of all fields created with the DEFINE command. The
maximum size of defined data allowable is 262,140 bytes.

## Error 536

All file handles available from DOS were in use. Please
make sure that the line FILES=20 (or more) is in your
CONFIG.SYS file. If that isn’t the case then add it,
reboot your computer and try again.
These are files that are opened by the compiler for use during the compilation
process.

## Error 537

Access has been denied to you for the file requested.
This means either the file is marked read-only or that
there is no space/directory entries available.
Make sure that the .RUN program you are creating is not set read-only.

## Error 538

The path entered does not exist.

## Error 539

The program is unable to create the file above.
reason.
Unknown
The programmer should make sure that a legal file name was entered.

## Error 540

There are no lines of code in the specified source file.

## Error 541

There was not enough space on the disk to save the run
file. Please delete or move some files to other disks
and try again.

## Error 542

The compiler was successful and the .RUN file has been
created.
Everything went just fine and the .RUN program is ready to run.

## Error 545

Quote marks are unbalanced in the line.
You need to make sure that the same type of quote marks have been used at the
beginning and end of the string constant. An easy error can occur when you use an
apostrophe within the constant. For example:
‘Don’t do that again’
Will cause this error to appear. The correct way to use this constant is:
“Don’t do that again”

## Error 547

An unknown command modifier was found.
An option has been specified which doesn’t exist for this command.

## Error 548

An unknown compiler directive was found.

## Error 549

An error was made in specifying the date type. The only
choices are:
American/Ansi/British/Italian/French/
German.
The above options are the date formats allowed. For more information on what each
one looks like please see Chapter 5, Command Reference.

## Error 553

Too many decimal characters have been specified in a
constant. The maximum is 8.

## Error 554

An ELSE command has been found without a corresponding
IF.
An ELSE cannot be used unless an IF command has been used first and is still active.
For more information please refer to Chapter 7, Structured Programming Commands.

## Error 555

An ENDIF command has been found without a corresponding
IF.
An ENDIF cannot be used unless an IF command has been used first and is still
active. For more information please refer to Chapter 7, Structured Programming
Commands.

## Error 556

An ENDWHILE command has been found without a corresponding WHILE.
An ENDW (Endwhile) cannot be used unless a WHILE command has been used
first and is still active. For more information please refer to Chapter 7, Structured
Programming Commands.

## Error 557

An EXIT command has been found without a corresponding
WHILE.
An EXIT cannot be used unless a WHILE command has been used first and is still
active. For more information please refer to Chapter 7, Structured Programming
Commands.

## Error 558

A NEXT command has been found without a corresponding
FOR.
A NEXT cannot be used unless a FOR command has been used first and is still
active. For more information please refer to Chapter 7, Structured Programming
Commands.

## Error 559

A FLOOP command has been found without a corresponding
FOR.
A FLOOP cannot be used unless a FOR command has been used first and is still
active. For more information please refer to Chapter 7, Structured Programming
Commands.

## Error 560

A FEXIT command has been found without a corresponding
FOR.
A FEXIT cannot be used unless a FOR command has been used first and is still
active. For more information please refer to Chapter 7, Structured Programming
Commands.

## Error 561

A LOOP command has been found without a corresponding
WHILE.
A LOOP cannot be used unless a WHILE command has been used first and is still
active. For more information please refer to Chapter 7, Structured Programming
Commands.

## Error 562

An ENDCASE cmd has been found without a corresponding
SELECT.
An ENDC (Endcase) cannot be used unless a SELECT command has been used first
and is still active. For more information please refer to Chapter 7, Structured
Programming Commands.

## Error 563

There have been 1 or more IF cmds without corresponding
ENDIF.
Each IF command must have a corresponding ENDIF to finish it off. For more
information please refer to Chapter 7, Structured Programming Commands.
This can also occur if you have forgotten an ENDC (End Case) command at the end
of a SELECT/CASE structure.

## Error 564

There have been 1 or more WHILE commands without corresponding ENDWHILE.
Each WHILE command must have a corresponding ENDW (Endwhile) to finish it
off. For more information please refer to Chapter 7, Structured Programming
Commands.

## Error 565

There have been 1 or more SELECT commands without corresponding ENDCASE.
Each SELECT command must have a corresponding ENDC (Endcase) to finish it
off. For more information please refer to Chapter 7, Structured Programming
Commands.

## Error 566

There have been 1 or more FOR commands without corresponding NEXT.
Each FOR command must have a corresponding NEXT to finish it off. For more
information please refer to Chapter 7, Structured Programming Commands.

## Error 567

The expression is too complex. Either increase the number of internal fields, or split it into two operations.

## Error 569

The comparison type was not acceptable.
Please see Chapter 1, Installation and General Information for the comparison
operators.

## Error 570

The parentheses are unbalanced in the expression.
Each expression in which parentheses are used must have an equal number of ‘(‘ and
‘)’. Make sure NOT to surround a single value with parentheses, i.e.:
(4) or (counter)

## Error 571

There is an error in your expression. Probably a single
number/field with parentheses around it.
(1) or (‘abc’) or (test) are not legal expressions. These expressions can be stated
without parentheses.

## Error 572

The function in the expression is not a legal TAS Professional 5.1 function.
If this is a UDF the programmer must be sure that the #UDF compiler directive has
been set before the function is used in an expression.

## Error 574

You must have a receiving field in this command.

## Error 575

You must have a file name in this command.

## Error 580

You have specified too many #INC (include) files in the
program. The maximum allowed is 20.

## Error 581

FILEDICT.B was not found.
Make sure that the TAS50= value is set. For more information please see Chapter 1,
Installation and General Information.

## Error 582

FILELOC.B was not found.
Make sure that the TAS50= value is set. For more information please see Chapter 1,
Installation and General Information.

## Error 583

FILEKNUM.B was not found.
Make sure that the TAS50= value is set. For more information please see Chapter 1,
Installation and General Information.

## Error 585

No memory space is available to allocate the Source file
buffer.

## Error 586

No memory space is available to allocate the Constant
buffer.

## Error 587

No memory space is available to allocate the Field buffer

## Error 588

No memory space is available to allocate the Label buffer

## Error 589

No memory space is available to allocate the Compiled
program buffer

## Error 590

No memory space is available to allocate the Specification line buffer

## Error 591

No memory space is available to allocate the Screen/
Report source file buffer

## Error 592

No memory space is available to allocate the Compiled
Screen/Report buffer

## Error 593

The maximum size for field name and array is 60 characters.
The field name cannot be more than 15 characters; however, the array element
specifier can be an expression.

## Error 594

The option entered was not found.

## Error 595

The option entered is not allowed in this command.

## Error 596

*** Internal Error ***
Call support.

## Error 598

Error while opening error file.

## Error 599

The Constant segment has grown larger than the memory
allocated. For more information please refer to the
manual under Chapter 4, Compiler Information.

## Error 600

The Code segment has grown larger than the memory allocated. For more information please refer to the manual
under Chapter 4, Compiler Information.

## Error 601

The Spec Code segment has grown larger than the memory
allocated. For more information please refer to the
manual under Chapter 4, Compiler Information.

## Error 602

You have too many different named fields. For more information please refer to the manual under Chapter 4,
Compiler Information.

## Error 603

You have used too many different files.
A maximum of 32 files may be used in a program.

## Error 605

You have reached the maximum nesting level for the IF
command.
The IF/ENDIF structure can be nested 20 deep. For more information please refer to
Chapter 7, Structured Programming Commands.

## Error 606

You have reached the maximum nesting level for the WHILE
command.
The WHILE/ENDW structure can be nested 20 deep. For more information please
refer to Chapter 7, Structured Programming Commands.

## Error 607

You have reached the maximum nesting level for the SELECT/CASE command.
The SELECT/CASE/ENDC structure can be nested 20 deep. For more information
please refer to Chapter 7, Structured Programming Commands.

## Error 608

You have reached the maximum nesting level for the FOR
command.
The FOR/NEXT structure can be nested 20 deep. For more information please refer
to Chapter 7, Structured Programming Commands.

## Error 609

** WARNING ** The field you are trying to add has already been DEFINEd. You cannot define the same field
twice. This DEFINE command will be ignored.
In TAS Professional 5.1 only the first DEFINE in the program will be effective. This is just a warning
message and will not keep the program from running. The line number of the
subsequent DEFINE will be displayed. You should check the program and make
sure the field is being properly defined.

## Error 610

The maximum number of fields you may allocate is 2000.

## Error 611

You have tried to add more fields than you have allocated. For more information please refer to the manual
under Chapter 4, Compiler Information.

## Error 612

The field name specified was too long.

## Error 613

The field name is illegal.
chr from A thru Z.

## Error 614

You have specified an unknown field type. The only acceptable values are: A / N / D / T / I / B / P / R / L /
O

## Error 615

You must use a numeric constant for the field size.

## Error 616

You must use a numeric constant for the field decimal
chrs size.

## Error 617

You must use a numeric constant for the field array size.

## Error 618

Something is wrong with the array specifier.
check it.

## Error 619

The field display size specified is too long for the
field type.

## Error 620

You have not specified a field display size.
Maximum 15 chrs.
It must start with an alpha
Please

## Error 621

You cannot specify more than 8 decimal chrs in a numeric
field.

## Error 622

You have not specified a field name for this field.

## Error 624

This field was used in the program.
DEFINEd or in the data dictionary.

## Error 625

You have defined a field that is also in the data dictionary.
It must be either
This is just a warning error; however, the programmer must be sure that s/he isn’t
misusing the field.

## Error 626

You have tried to DEFINE too many fields at the same
time. The limit is 10 for a single DEFINE command.

## Error 627

The #XLATE compiler directive has been set in this program and the file cannot be found in the default directory.
The TRANSLTE.B file must be in the same directory as TASCOMP.EXE and have
the TAS50=path value set.

## Error 628

The key name was not found in FILEKNUM.B
Each key created for a file has a record in FILEKNUM.B. You can refer to this key
in any command requiring it using the key number (preceded by the at sign ‘@’) or
the key name. If the compiler cannot find the key name used, this message will be
displayed. If you add an index to a file make sure that you either initialize or reindex/
restructure that file. You won’t be able to use the key until you do.

## Error 629

The label name was used but was not set as a legal label.
This could also happen if you have used a UDF or UDC but the actual function or
command is not a part of the program or has not been included. If you can’t find the
error remove the compiler directives for the UDF and/or UDC (or UDX). This will
increase your errors but you will rapidly find the line giving you the problem.

## Error 630

The label name was already used.
UDF and UDC names are added to the label list. You must be careful not to duplicate
those names with actual labels or to duplicate label names themselves.

## Error 631

The value in this instance must be a constant.

## Error 632

You can’t specify a Pointer type constant.

## Error 633

You can’t specify a Date type constant.
=CTOD(‘date’) instead.
Use
If you are trying to set the value of a D type field you may also use the date as an
alpha constant. For example:
‘1/1/92’
‘10/10/85’

## Error 634

You can’t specify a Time type constant.
=CTOT(‘time’) instead.
Use
If you are trying to set the value of a T type field you may also use the time as an
alpha constant. For example:
‘10:10’
‘11:52’

## Error 635

An error has occured in Btrieve.

## Error 638

An error occurred while accessing the library file.

## Error 641

You may not use an expression or array field in a FIND
The name used in the FIND command as the key_or_file_name must be named
explicitly as a key or be the name of a file.

## Error 642

You must have the FD in this command.
This is for the SETACT (Set Active) command.

## Error 643

There is a limit of 20 screen/report formats in a single
program.

## Error 644

The Screen/Report Format above was not part of the source
file but was referred t in a MOUNT command
If you don't have the EXTERN option set in the MOUNT command, or the
#EXT_FMT compiler directive then the screen/report format must be a part of the
source code. You can import a format by loading the program in the source code
editor, pressing the ^I key (CTRL+I) and entering the path and name of the format
file.

## Error 645

A label is required for this command.

## Error 646

You must have a format name in a MOUNT command.

## Error 647

The only legal format types are (S)creen, (R)eport and
(B) Report/2

## Error 648

This error occurred during compilation of screen/report format:
The name of the screen/report format that is causing the problem will be displayed at
the end of this message.

## Error 649

This error occurred while checking all Label locations.
This is the final check to make sure that all labels that were used in a program were
accounted for.

## Error 650

The maximum number of nested functions is 10.

## Error 651

The screen/report format name used in the REMOUNT command
hasn’t been MOUNTed yet.

## Error 652

There are 1 or more SCANS without ENDS.

## Error 653

The maximum number of nested SCANs is 20.

## Error 654

SEXIT without SCAN.

## Error 655

SLOOP without SCAN.

## Error 656

ENDS without SCAN

## Error 657

You have exceeded the number of line labels specified for
this program.

## Error 658

The value in the CASE command must be an integer numeric
constant.

## Error 659

There is no START value for the FOR/NEXT loop and there
must be.

## Error 660

There is no STOP value for this FOR/NEXT loop and there
must be.

## Error 661

There is no STEP value for this FOR/NEXT loop and there
must be.

## Error 663

The Defined Field Dictionary file (FILEDFLD) was not
found.
This should be in the same subdirectory as the other dictionary files.

## Error 667

A Screen/Report format with this name has already been
found. You can not have two formats with the same name.
The compiler found two or more different formats in the same source file with the
same name. The first one was already compiled when it discovered the second.

## Error 668

The field used in the DUP option in the DEFINE command
was not found in the data dictionary.

## Error 669

You have tried to initialize a Defined Filed before all
the necessary info has been set. This includes Type,
Size (if 'A') and Array Elements (if any). Also you
can't INIT a BCD field.
The INIT option in the DEFINE command must be the last option set in the command.
PROFESSIONAL
VERSION 5.1
