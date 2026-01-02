# O

## Summary
615

## Signature
```
You must use a numeric constant for the field size.
```

## Details
616
You must use a numeric constant for the field decimal
chrs size.
617
You must use a numeric constant for the field array size.
618
Something is wrong with the array specifier.
check it.
619
The field display size specified is too long for the
field type.
620
You have not specified a field display size.
Maximum 15 chrs.
It must start with an alpha
Please
Compiler Errors
621
You cannot specify more than 8 decimal chrs in a numeric
field.
622
You have not specified a field name for this field.
624
This field was used in the program.
DEFINEd or in the data dictionary.
625
You have defined a field that is also in the data dictionary.
It must be either
This is just a warning error; however, the programmer must be sure that s/he isn’t
misusing the field.
626
You have tried to DEFINE too many fields at the same
time. The limit is 10 for a single DEFINE command.
627
The #XLATE compiler directive has been set in this program and the file cannot be found in the default directory.
The TRANSLTE.B file must be in the same directory as TASCOMP.EXE and have
the TAS50=path value set.
628
The key name was not found in FILEKNUM.B
Each key created for a file has a record in FILEKNUM.B. You can refer to this key
in any command requiring it using the key number (preceded by the at sign ‘@’) or
the key name. If the compiler cannot find the key name used, this message will be
displayed. If you add an index to a file make sure that you either initialize or reindex/
restructure that file. You won’t be able to use the key until you do.
629
The label name was used but was not set as a legal label.
This could also happen if you have used a UDF or UDC but the actual function or
command is not a part of the program or has not been included. If you can’t find the
error remove the compiler directives for the UDF and/or UDC (or UDX). This will
increase your errors but you will rapidly find the line giving you the problem.
630
The label name was already used.
UDF and UDC names are added to the label list. You must be careful not to duplicate
those names with actual labels or to duplicate label names themselves.
631
The value in this instance must be a constant.
632
You can’t specify a Pointer type constant.
633
You can’t specify a Date type constant.
=CTOD(‘date’) instead.
Use
If you are trying to set the value of a D type field you may also use the date as an
alpha constant. For example:
‘1/1/92’
‘10/10/85’
Compiler Errors
634
You can’t specify a Time type constant.
=CTOT(‘time’) instead.
Use
If you are trying to set the value of a T type field you may also use the time as an
alpha constant. For example:
‘10:10’
‘11:52’
635
An error has occured in Btrieve.
638
An error occurred while accessing the library file.
641
You may not use an expression or array field in a FIND
The name used in the FIND command as the key_or_file_name must be named
explicitly as a key or be the name of a file.
642
You must have the FD in this command.
This is for the SETACT (Set Active) command.
643
There is a limit of 20 screen/report formats in a single
program.
644
The Screen/Report Format above was not part of the source
file but was referred t in a MOUNT command
If you don't have the EXTERN option set in the MOUNT command, or the
#EXT_FMT compiler directive then the screen/report format must be a part of the
source code. You can import a format by loading the program in the source code
editor, pressing the ^I key (CTRL+I) and entering the path and name of the format
file.
645
A label is required for this command.
646
You must have a format name in a MOUNT command.
647
The only legal format types are (S)creen, (R)eport and
(B) Report/2
648
This error occurred during compilation of screen/report format:
The name of the screen/report format that is causing the problem will be displayed at
the end of this message.
649
This error occurred while checking all Label locations.
This is the final check to make sure that all labels that were used in a program were
accounted for.
650
The maximum number of nested functions is 10.
651
The screen/report format name used in the REMOUNT command
hasn’t been MOUNTed yet.
Compiler Errors
652
There are 1 or more SCANS without ENDS.
653
The maximum number of nested SCANs is 20.
654
SEXIT without SCAN.
655
SLOOP without SCAN.
656
ENDS without SCAN
657
You have exceeded the number of line labels specified for
this program.
658
The value in the CASE command must be an integer numeric
constant.
659
There is no START value for the FOR/NEXT loop and there
must be.
660
There is no STOP value for this FOR/NEXT loop and there
must be.
661
There is no STEP value for this FOR/NEXT loop and there
must be.
663
The Defined Field Dictionary file (FILEDFLD) was not
found.
This should be in the same subdirectory as the other dictionary files.
667
A Screen/Report format with this name has already been
found. You can not have two formats with the same name.
The compiler found two or more different formats in the same source file with the
same name. The first one was already compiled when it discovered the second.
668
The field used in the DUP option in the DEFINE command
was not found in the data dictionary.
669
You have tried to initialize a Defined Filed before all
the necessary info has been set. This includes Type,
Size (if 'A') and Array Elements (if any). Also you
can't INIT a BCD field.
The INIT option in the DEFINE command must be the last option set in the command.
