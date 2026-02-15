# Compiler Information

## Overview

Chapter 4
Compiler Information

INTENTIONALLY BLANK

Compiler Information
INTRODUCTION
Every TAS Professional 5.1 program must be compiled before it can be executed. When a program is
compiled several things occur. Each field used in any command or expression is checked to make sure
that it has been defined or is part of a record defined in the data dictionary being used. Each command
is checked to make sure it exists and that the options are legal; problems with these requirements cause
general syntax errors. And finally, the resulting program, with all its bits and pieces, is saved. This
entire process can take as few as 2 seconds or several seconds, depending on the computer and the size
of the program. The compiler makes just one pass through the source file. All adjustments are made
after the compilation is finished. If errors occur, the program will give the programmer the line
number, the offending value, and what the error was.
Once the compiler is complete the program will display the sizes of different parts of the finished
program. This information is really just for your enlightenment. However, it will keep you informed
of program sizes so that you can keep track of memory usage.
The code produced by the compiler is not just tokenized. It is truly compiled, just not to the proper
form for direct execution on an Intel processor.

DEFAULT COMPILER BUFFERS
The following are the buffers that are predefined in the TAS Professional 5.1 compiler. Please see
Command Line Changes below for more information. Particular settings specific to a single program
may also be entered into the CMPDFLT.B file using the COMPINFO.RUN program. For more
information about this program please refer to Chapter 3, Main Menu - Program. There is a record
in the CMPDFLT.B file with the title DEFAULT. This record of default buffer sizes is used by the
compiler when it cannot find a record set up specifically for the file being compiled. If you are running
out of room when you are trying to compile a program, you might want to reduce the values in this
record or even delete it completely. Here''s more information on the various buffers:
Buffer

CL codeWhat does it do

Run code

R

Each line that is compiled requires 10 bytes. This
buffer, at actual size, is loaded during runtime.

Constant

C

Each constant used in the program is saved in this
buffer, along with compiled screen/report formats.
This is the most likely buffer that will have to be
changed during compilation. This buffer, at actual
size, is loaded during runtime.

Spec line

S

Each command has a certain number of bytes that
defines it. Those bytes are saved here. This buffer, at
actual size, is loaded during runtime.

Screen/Report fmt
compile buffer

B

When a screen/report format is compiled this is the
temporary buffer that is used to store the compiled
code. This value will generally be more than large
enough. Once the format has been compiled, it is
stored in the constant buffer.

Internal fields (num)

I

The program uses these internal fields while resolving
expressions. This should be enough. However, if you

tas"

### `ADD_FLDS`

```tas
#ADD_FLDS 10
```

During execution of a program the programmer can add fields to the field
list thru the use of the ADD command. However, for this to be accomplished the programmer must add extra fields to the field list during the
compilation process. This compiler directive will do just that. The example
below will add 10 extra field spaces to the field list:
#ADD_FLDS 10

### `ALL_LOC`

```tas
#ALL_LOC
```

This directive will have the same effect as including the LOCAL option in
all DEFINE commands following the directive. This directive would have
to be after the PROC directive to have any effect and will be effective until
the next ENDP directive. Example:
#ALL_LOC

### `CHK_UP_VLD`

```tas
#CHK_UP_VLD
```

Normally, the VALID option in the ENTER command is always checked
unless the user presses the ESC or Up Arrow key. If you do want to check
the VALID option, even if the Up Arrow key is pressed, include this
directive. Example:
#CHK_UP_VLD

### `ENDP`

```tas
#ENDP
```

This directive is the termination for the PROC directive. Once this directive
is reached it will terminate the scope for all LOCAL defined fields within
the routine from the previous PROC directive to this line. If a field with the
same name is used outside of the routine bounded by the PROC/ENDP
directives it will be treated as a different field. Example:
#ENDP

### `EXT_FMT`

```tas
#EXT_FMT
```

In TAS Professional 5.1 all screen/report formats are normally included as
part of the source code. However, if you wish to have them as separate files
on the disk use this compiler directive. If this is set then the program
expects all formats (no matter what type) to be separate for this program.
You can also use the EXTERN option in the MOUNT command if you want
to keep just a few formats separate. This would apply when you have a
format you use in several different programs.
#EXT_FMT

### `FMT`

```tas
#FMT
```

If you are using 'old type' screen report formats then this option must be set.
If the PRO3 compiler directive is also set then the compiler expects the
screen/report formats to be in TAS Professional 3.0 form. If PRO3 is not set
then you must use the TAS Professional 4.0 method.
#FMT

### `INC`

```tas
#INC C:\ACCTG\XRTN
```

You may specify program parts or subroutines not originally part of the
source code that you want to be compiled (or included) with it. These parts
can be named here. You should note that the included programs will be
attached at the end of the original named program. These parts should be
subroutines, UDFs, or UDCs to make sure that the execution of the final
routine will be as desired or you must overtly transfer control to them
through the use of the GOTO or GOSUB command. An example of an
include file is:
#INC C:\ACCTG\XRTN

4-4

TAS Professional 5.1
Copyright  Business Tools, Inc. 1985-1996 All Rights Reserved



### `LIB`

```tas
#LIB TASRTNS
```

You might want to keep a library of commonly used routines for use in any
program. This will allow you to maintain only one copy of a set of code and
load the pieces required in your program automatically at compile time. To
specify the appropriate library file put the name after the LIB directive, i.e.:
#LIB TASRTNS
The compiler expects the file to reside in the same directory as the compiler
unless you specify the appropriate path. With TAS Professional 5.1, you
also have the ability to use a SET TASLIB= to set a path in your environment (generally in your AUTOEXEC.BAT file). This is similar to the SET
TAS50= in TAS Pro 5.1 and the SET TASPRO= command in 4.0. If you
use a SET TASLIB= statement, the compiler will look for all libraries in the
path given. This means you can have one set of libraries for all programs
you''re developing.
A library has been provided to you that includes several useful commands
and functions. It is TASRTNS.LIB.

### `PRO3`

```tas
#PRO3
```

This directive tells both the compiler and the runtime that the program is a
direct conversion from TAS Professional 3.0. Putting in this directive
activates certain features that are different from the native TAS Professional
5.1 process. These include:
a) The #OLD_MATH compiler directive is automatically set. This means
that expressions are evaluated from left to right instead of using the operator
hierarchy. (For more information, see above.)
b) If you have a POPS (Pop Stack) command in your program and have no
corresponding GOSUB (i.e., you're at the top of the stack), the command
will be ignored rather than giving an error.
c) In the TAS Professional 5.1 version of the GFLD() function (Get Field
from Buffer), if you don't provide a file handle, then you must provide a
field name. With the #PRO3 directive in place, the program looks for the
active non-TAS file number automatically if you don't provide the file
handle as the first option in the function body.
d) If you are moving a BCD (new type O) field to an alpha type field using
the EQUAL (=) command, the program will truncate the BCD field if it is
longer than the alpha. This means what was a 5 character BCD field will fit
in a 2 character alpha field if the value is less than or equal to 99. However,
even if the BCD value is greater than 99, the program will only move the last
2 characters. This only applies to BCD type fields. New I, R, and N fields
are moved from left to right without truncation unless you use the appropriate commands or function.
e) In TAS Professional versions 4.0 and 5.1, when you open a non-TAS
fixed length record, the program expects you to add 2 bytes to the record
size to allow for the CR/LF at the end. In TAS Professional 3.0, this wasn't
the case. When this directive is set, the program uses the version 3.0 rules
when reading non-TAS fixed length records. This includes not clearing the
CR/LF pair when the record is read.

f) In TAS Professional 3.0, the Help (F1), Page Break (PG_BRK) and
Related Record Search (RSRCH) traps are all forced to GOSUB if you set it
as GOTO. The same applies to TAS Professional 5.1. Please note that the
conversion program automatically converts a GOTO for these traps to
GOSUB, but even if you force the command to a GOTO, it is interpreted as
a GOSUB.
g) When using the ENTER option in a PMSG command, the UPCASE flag
is always set.
h) When concatenating (adding together) alpha type fields, TAS Professional 3.0 only moves those characters greater than binary 0. This directive
activates the same restriction.
i) When displaying or printing, the process will quit when a CR is encountered.
j) The WAIT option in the PMSG command ignores the ESC key.
k) The RCN (Get/Set Record Number) command in TAS Professional 3.0
expected a 4 byte alpha field, whereas TAS Professional versions 4.0 and 5.1
use the new R type (4 byte integer) field. If the #PRO3 directive is used, the
program will use either type of field.
l) In TAS Professional versions 4.0 and 5.1, if the FOR/NEXT loop runs to
completion, the counter is 1 more (or 1 less if the step is -1) than the stop
value. In version 3.0, the counter would never be more (or less) than the
stop value.

### `PROC`

```tas
#PROC
```

When you DEFINE fields within a program you may specify that they are
LOCAL to that routine. To ‘turn on’ the local option you must preface the
routine with this directive. Then all defined fields until the next ENDP
directive will be local to that routine only. For example, add the following:
#PROC
For more information about the effect of LOCAL on a field please refer to
the DEFINE command in the reference section, Chapter 5, Command
Reference.

### `SFLDS`

```tas
#SFLDS 20
```

When the source file is compiled, the program keeps track of the screen
formats that are compiled. A buffer of screen fields is created in the runtime
that will hold the largest of the screens. The compiler defaults to 10 screen
fields even if no screen formats are compiled. If you should want to use the
SAY ... AT command as part of the program be sure to specify the maximum number of fields to be added to the screen field buffer through the use
of this directive. The amount given here will be added to the maximum
number already calculated during the compilation process. You should put
this directive before the first MOUNT command it applies to, in order to be
sure that it is carried out correctly. An example is:
#SFLDS 20
NOTE: Always to be sure to MOUNT a screen format before using the
SAY...AT command. This will make sure that the screen buffers are
properly set up with the program before the new fields are added. You may

### `TDATA`

```tas
#TDATA 25000
```

Along with the internal fields a certain amount of data is maintained for
internal field use. The default value of this directive is 64K bytes. This
should be more than enough for most programs. However, if you receive an
error message stating that temporary data has been exhausted, this directive
will increase that amount for the specific program. You are given control
over this value since a larger buffer just takes up more space. Just specify
the number of bytes (characters) to be allocated to this buffer. For example:
#TDATA 25000
will allocate 25000 bytes to the temporary data buffer.

### `UDC`

```tas
#UDC
```

You must include this directive if you are going to have User Defined
Commands (UDCs) within the program. If you do not, and you try to use a
command that doesn’t exist normally in TAS Pro 5.1, you will receive an
appropriate error message. To activate this option include the following:
#UDC
No other entry is required.

### `UDF`

```tas
#UDF
```

You must include this directive if you are going to have User Defined
Functions (UDFs) within the program. If you do not, and you try to use a
function that doesn’t exist normally in TAS Pro 5.1, you will receive an
appropriate error message. To activate this option include the following:
#UDF
No other entry is required.

### `UDX`

```tas
#UDX
```

This directive allows both UDCs and UDFs.

### `XLATE`

```tas
#XLATE
```

TAS Professional 5.1 offers an option unique in a 4GL. If you don’t like the
command or option names, create different ones. Thru the use of the
translation file you can assign new names that will be the equivalents of the
current ones. Then you can use either one within the program. If this
directive is specified, the compiler will look for the appropriate command or
option within the translation file. To turn this option on set the directive:
#XLATE
No other entry is required. The translation file (TRANSLTE.B) must be in
the same directory as the compiler.
NOTE: This has the same effect as the -T option in the compiler command
line.

## Notes

- NOTE: Since the compiler is part of the TAS Professional 5.1 runtime the -C after the TPC50 tells TAS to compile this program instead of running it.
- The error file created will be PRGNAME.ERR, the constant buffer will be increased in size to 24k or 24756 bytes, and the alternate dictionary C:\ACCTG\SRC will be used. The use of lower or upper case characters does not matter.
- If a buffer size is exceeded the compiler will alert you to that problem. You can then adjust the buffers as needed and compile the program again. In the case of the number of internal fields the problem may not be seen until the program is run. The solution is still to recompile the program with the -I number set to something greater than 30. In a situation of this sort you can generally increase the number of internal fields to 35 or 40 and will have more than enough. In general, if you have an expression so complex that you find the need to increase the number of temporary fields you are better off breaking the expression into two parts.
- NOTE: VERY IMPORTANT!! If you have a continuation character at the end of a line, it will be treated as such even if it is after a remark character. Remark characters include both ';' and '&&'. If you can't figure out why a line isn't being compiled, look for this situation immediately before that line.
- NOTE: This has the same effect as the -T option in the compiler command line.
- NOTE: Always to be sure to MOUNT a screen format before using the SAY...AT command. This will make sure that the screen buffers are properly set up with the program before the new fields are added. You may
- MOUNT a blank format if there is nothing else you want displayed on the screen.
- NOTE: The Program Editor cannot edit lines with more than one command per line. You must either edit these using the Alt_E option or convert them to multiple line commands.
