# Runtime Errors

Total: 146

## Error 1

TAS50.OVL not found or error while loading.

## Error 3

BTRIEVE has not been loaded.
For some reason BTRIEVE was not loaded properly. Make sure it is on your disk,
preferably in your TAS50 subdirectory. You can also try loading it with the LB.BAT
program. If it still doesn’t load call support.

## Error 6

The RUN program specified was not found.
The program name that was included on the command line could not be found. Don’t
include the .RUN extension when typing in the program name. If you have not
included a program name, TAS Professional 5.1 looks for MENU.RUN. If it doesn’t
exist you will also get this message. You must either name a run program at the DOS
command line or have MENU.RUN in your subdirectory. Also, make sure the
programs are not set as read-only.

## Error 8

An error occured while reading the RUN program.
Something has corrupted the .RUN program or it is from a previous version. Make
sure it is up to date.

## Error 9

No memory space is available.
The user needs to add more RAM memory.

## Error 10

The temporary data space is too small to accommodate the
calculations desired. Please specify larger in the
source program.
Set the TDATA compiler directive to something over 64k bytes or more if necessary.
You might also want to look at simplifying the expression that gave you this error.

## Error 16

again with a larger stack.
Please try
This occurs when the program has executed too many nested GOSUBs, PUSHs,
UDCs, or UDFs. The programmer needs to increase the internal stack value. Please
see Chapter 1, Installation and General Information for instructions.

## Error 17

POPs or RETURNs.
Too many
The program has executed more POPs or RETURNs than PUSHs or GOSUBs.

## Error 18

There are no more temporary fields available.
increase initial value and try again.
Please
Increase the number of temporary fields using the -I compiler command line option.
Default number is 30. You probably have a UDF that isn’t returning properly. You
might also try simplifying any expressions (split them over multiple lines if necessary).

## Error 19

The field you are trying to use is unallocated. It may
belong to a file that hasn’t been opened yet, or was an
array that was removed from memory - fieldname
Make sure that the file this field belongs to has been opened. This generally happens
when the programmer mounts a screen before the applicable files have been opened.
The field name will be displayed at the end of the error message.

## Error 20

The window title must be an ALPHA field.
Can occur during the WINDOW command.

## Error 21

You must have a SAVE_TO field for this command.
Can occur during the WINDEF (Window Define) command.

## Error 22

The SAVE_TO field you have specified is too small for its
purpose.
The window and attached information as defined is too large to fit in the field you
have specified.

## Error 23

This file number field must be type I or R.
All file_numbers must be integers (I or R type).

## Error 24

This field must be of an Integer type.
Displayed in situations where you should’ve used an I type field.

## Error 25

This field must be of a Record type.
Displayed in situations where you should’ve used an R type field.

## Error 26

The field in this command must be of an Alphanumeric
type.
Displayed in situations where you should’ve used an A type field.

## Error 27

The field in this command must be a regular field, expressions or constants are not allowed.

## Error 28

The field must be of type ‘F’ or ‘P’.
You have used the redirection symbol (&) on a field that wasn’t a pointer.

## Error 29

The receiving field in this command must be of type ‘F’
or ‘P’.
The program expected a pointer as the receiving field.

## Error 30

The receiving field in this command must be of type ‘F’.
In this situation the pointer must be of type F.

## Error 31

The array element specified is out of range for this
field.
You have tried to access an array element that is beyond the number defined for the
field.

## Error 32

File names, including paths, cannot be more than 59 chrs
in length - @

## Error 39

Internal TAS Professional 5.1 error.
Call for support.

## Error 40

The VALID expression returned false.
done again.
The entry must be
You have specified a valid option in the ENTER command but no valid_msg option
was set. This error message is the default.

## Error 41

The field to be entered was not in the screen buffer.
Please make sure you have ‘MOUNTED’ a screen and that the
field is on that screen.
The program has tried to ENTER a field for which no column,row option was set
and the field is not in the active screen buffer.

## Error 42

An error has occured while accessing a non-TAS file - @
A DOS error occured during reading or writing.

## Error 43

There are no more extra fields available.
Too many fields have been added thru the ADD command. Use the ADD_FLDS
compiler directive to provide more slots.

## Error 44

The field you are trying to add has an unknown type.
See Chapter 1, Installation and General Information, section Field Types for
allowable types.

## Error 45

The field being added cannot have a size of 0.
A size must be specified.

## Error 46

The maximum number of decimal chrs for the field being
added is 8.

## Error 47

An error has occured while attempting to open FILELOC.
This will occur when TASPRO is first executed. Make sure that FILELOC.B is
present in the same directory as TASPRO.EXE and that the DOS SET value for
TAS50= is correct, if applicable.

## Error 48

A record was not found in FILELOC for file - @1
There must be a record for each file opened.
.
When a file is opened you need to either specify the fd and path values in the
OPENV command, or a record for that file must be in the FILELOC.B file.

## Error 51

An error has occured while trying to open file - @1
The file either isn’t where it is supposed to be or something has happened to it. The
file name will be displayed as part of the error message.

## Error 52

The OWNER command will work with TAS Professional 5.1
files only.
You have set the owner option in the OPENV (Open Variable) command for a nonTAS file.

## Error 53

You have specified too many RELATEDs.
for all programs currently being run.
The limit is 32

## Error 54

You have specified too many menu choices, max is 25.

## Error 55

You have specified a key number for which there is no
key.
Check the keys for the file in the Maintain Data Dictionary program
(TASDMGR.RUN). If you add a key to the FD you must either initialize the file or
reindex/restructure it before the key can be used.

## Error 56

You have not specified either a SEARCH FILE value, nor a
file_number this particular command. You must do one or
the other.

## Error 57

You have not specified either a SEARCH FILE key number,
nor a key value in this particular command. You must
have one or the other.

## Error 58

You must have a legal file_number for this command.
The value being passed as the file_number is incorrect.

## Error 59

You can’t delete records in non-TAS files - @
Records can be deleted only in TAS Professional 5.1 files.

## Error 60

The RAP buffer number specified was either 0 or more than
the max number of buffers available.
The maximum number of RAP buffers is 10.

## Error 61

This file was not opened in a previous program - @1
You have used the ROPEN (Reopen file) command and the file wasn’t opened in a
previous program.

## Error 62

The file being used has not been opened yet.

## Error 65

In this command you must specify the NUMBER of elements.

## Error 66

You have used an illegal type spec in the REDEFINE command.

## Error 67

An Alpha field must have a size specified in the REDEFINE
command.

## Error 68

You have specified a handle number and there is no corresponding file.

## Error 69

The program does not allow adding new items to this list,
however, there are no lines to list at this time.
This will occur in a LISTM command when there are no lines to list and NOADD
option is set.

## Error 70

You are trying to change this list and there are not
enough unused lines to do so.
The program needs at least one extra line to use as a holder during any change, add or
insert operation in a LISTM command.

## Error 72

You can’t convert a TIME field to an INTEGER field.

## Error 73

You can’t convert a DATE field to an INTEGER field.

## Error 74

You can’t convert a POINTER field to an INTEGER field.

## Error 75

You can’t convert a TIME field to a BYTE field.

## Error 76

You can’t convert a DATE field to a BYTE field.

## Error 77

You can’t convert a POINTER field to a BYTE field.

## Error 78

Internal error CRF
Call for support and give error number and CRF code.

## Error 79

An error has occured in BTRIEVE.
The error occured while trying to access a file in some manner.

## Error 82

BTRIEVE error - @
This will return the file name and the error number.

## Error 105

*** BAD DATE - REINPUT ***
The date entered by the user was incorrect. It will have to be reentered.

## Error 111

You must have a Buffer name for every non-TAS file - @
The BUFFER option in the OPENV (Open Variable) command was not set and the
file was not defined in the File Data Dictionary.

## Error 112

You must specify a record size of every non-TAS file.
The SIZE option in the OPENV (Open Variable) command was not set and the file
was not defined in the File Data Dictionary.

## Error 113

The maximum number of elements in an array to be sorted
must be at least 1 greater than the number of elements to
be sorted.
The program needs an extra element to use as temporary storage.

## Error 114

The field in the FFLD() function was not found in the
field list.

## Error 116

The field in this command/expression must be of ‘N’ type.

## Error 117

You must MOUNT a Report Format before you can use the
PRINT FORMAT command.

## Error 118

The Format number specified in the PRINT FORMAT LINE
command is beyond the end of the Report Format that is
currently mounted.

## Error 119

The major field in an UPDATE ARRAY command must be in
array format, i.e., X[Y], even if it is the first element, or X[1].

## Error 120

You have tried to use a field in an UPDATE ARRAY ADD/SUB
command that isn’t of type N, I, R, or P. Those are the
only ones allowed.

## Error 121

The only math allowed on ptrs is add and subtract.

## Error 123

An error has occured while trying to open the print to
file. Please try with a different file name: Occurs during the PON (Print On) command when specifying a Disk file name.

## Error 125

The READ/WRITE commands may not be used with TAS Pro 5.1
files - @
These commands were meant for non-TAS files only.

## Error 126

An error occured while trying to set the position as
required in a READ/WRITE command - @
The position specified was beyond the end of the file.

## Error 128

The program has tried to open more files than are allowed.
You may have 100 files open at any one time in all programs currently active.

## Error 129

An error has occured while opening or reading the printer
driver file.
This file contains the printer control codes and should be in the TAS50 path. This
can also occur if you don't have enough files specified in your CONFIG.SYS file.

## Error 130

The printer control command was not found.
You have specified a code name in the PCHR (Print Chr) command that could not be
found in the active printer driver file.

## Error 131

The pointer field in this command must be of ‘P’ type.

## Error 135

An error has occured while creating file - @
This message could occur while creating a new file or initializing a current one in the
Initialize file program (TASINIT.RUN).

## Error 136

**** BAD TIME - REINPUT ****
The time value entered by the user was incorrect. It will have to be reentered.

## Error 137

There is no space available in the Screen Field Buffer
and another field cannot be added with the SAY command.
You should increase the number of extra screen fields in the buffer using the SFLDS
compiler directive.

## Error 143

You can’t export to or import from a TAS Professional 5.1
file. You must use standard OPEN/FIND/SAVE commands.

## Error 144

The program cannot find the import file specified in the
command.
Make sure you have the proper path and file name. Don’t forget: you need to
include the applicable file extension, if any.

## Error 145

An error has occurred while attempting to open or write
to the Export file.

## Error 146

The maximum size for an IMPORT/EXPORT record is 64k and
you have exceeded that size.

## Error 147

An error has occurred while trying to read a record from
the Import file.

## Error 148

The field being passed and the associated receiving field
in the PARAMETER command must be of the same type.

## Error 154

An error occurred while opening or reading the ACS file
as specified in the TAS50.OVL file.

## Error 155

The field provided is not large enough to hold the entire
TRAP list.
This occurs during an XTRAP (Trap Operations) command when the field specified
to hold the traps is not big enough. You need 1000 bytes to hold the entire trap list.
We recommend that you don't specify a field at all. When the field is not specified
the program will create a buffer for the TRAP list and will delete automatically when
you RSTR.

## Error 156

The search and array field types in the ALOC function
must be of the same type.
You have tried to search for an element in an array field using a value of a different
type than that of the array field itself.

## Error 157

The second field in the ALOC function must be an array
field.

## Error 200

Error during disk read/write.

## Error 201

Must open file before accessing it.

## Error 202

Tried to save a duplicate key when not allowed.
- @

## Error 203

Changed keys and tried to do Next, Previous, etc.
Btrieve - @

## Error 204

Tried to modify a key value set as non-modifiable.
Btrieve - @

## Error 205

An error has occurred while trying to access/create/open
the pre-image file. Btrieve - @

## Error 206

The disk is full.

## Error 207

A major (unrecoverable) error has occurred.
backups. Btrieve - @

## Error 208

The file you have tried to open is not a standard TAS
Professional 5.1 (Btrieve) file. Please set the TYPE in
the Open command. Btrieve - @

## Error 209

Need to add the /T to the Btrieve trailer before the
Transactions commands can be used. Please refer to Chapter 3, Main Menu - Utilties - Set Configuration.
Btrieve - @
Btrieve - @
Btrieve
Btrieve - @
Use your
Change the value in the TAS50.OVL file for the Btrieve Load String. Add ‘ /T’ just
past the last character in the current load string.

## Error 210

You cannot nest Transactions.
before another can BEGIN.
One has to be COMMITed

## Error 211

An error occurred during the Transaction process. You
will need to restore the appropriate files from your
backups. The disk is probably full. Btrieve - @

## Error 212

You must have a Transaction Begin before a Transaction
Rollback. Btrieve - @

## Error 213

The maximum number of files that may be included in a
Transaction update is 12. You have exceeded that number.
Btrieve - @

## Error 214

File is read-only, you cannot write or delete any
records. Btrieve - @

## Error 215

The number of buffers available has exceeded the number
of files opened. Please reset the /M option. For more
information please see Chapter 3, Main Menu - Utilities Set Configuration.
Increase the value of the number after the /M in the Btrieve load string.

## Error 216

The Owner is already set for the file.
Btrieve - @

## Error 217

The correct Owner name has not been provided for this
file and it cannot be opened. Btrieve - @

## Error 218

An error has been received by Btrieve from the Expanded
Memory Manager. Btrieve - @
This is a common error with some expanded memory managers. You need to add a /
E to the end of the Btrieve load string. This will keep Btrieve from using expanded
memory. You can also use 386Maxtm from Qualitas. They know what the problem is
and have worked around it.

## Error 219

This record has been changed by another user since you
read it. It cannot be written back until you read it
again. Another option is to lock the record when it is
read. Btrieve - @
In this situation you have read the record with a FIND or RCN (Record Number Get/
Set) command and the no_lock option was set. You then tried to write the record
back to the file. It is best, in this type of situation where you know you’re going to be
saving the record back, to avoid using the no_lock option.

## Error 220

The lock table is full. Reset the /L option in the
Btrieve load string. Please refer to Chapter 3, Main
Menu - Utilities - Set Configuration for more information.
Increase the value of the number after the /L in the Btrieve load string. Or, if it
doesn’t exist, add it.

## Error 221

The record you have tried to access has been deleted
previously by another user. If you use the locking process this shouldn’t occur. Btrieve - @
This is the same situation as 219 in that you read the record with no_lock.

## Error 222

All records that are updated/deleted within a Transaction
must be read during that transaction and not before.
Btrieve - @
You can’t read a record before a TRANSX begin command and then try to delete it
before the TRANSX commit.

## Error 223

The number of files you have tried to open is greater
than the FILES= parameter in CONFIG.SYS. It needs to be
increased. Btrieve - @

## Error 225

DOS is restricting access to this file/record.
@

## Error 226

This is not a TAS Professional 5.1 Program.
Btrieve -
You have tried to CHAIN/RUN a program that was not compiled with TAS Professional 5.1.

## Error 227

This field is not indexed and cannot be used for searching within a file.
The user tried to use the search keys while the cursor was in a field that is not also
named as an index in the file definition. If you have named the field as an index
make sure that the file has been initialized or reindexed/restructured.

## Error 228

The file was not found in the list of opened files.

## Error 229

You have exceeded the maximum number of lines that can be
wrapped, this is 2000.

## Error 230

The maximum number of WRAP’d fields in any one print line
is 10.

## Error 231

You have specified a buffer for a non-TAS Btrieve file
that is smaller than that specified by the file itself.

## Error 232

The maximum ENTER field size is 255 characters.

## Error 233

An error has occurred while attempting to compile an
expression.
This will occur while trying to compile an expression as a filter. Make sure all fields
in the expression are named in the program and that you haven’t used a UDF, a
function that doesn’t exist, or an operator that doesn’t exist.

## Error 234

You have used too many internal screen buffers in the
program.
The maximum is 20. This means you may have up to 20 different MOUNT format
commands.

## Error 235

You have tried to REDISPLAY a screen from an internal
buffer and it wasn’t previously saved.
You have a REDSP command without the corresponding previous SAVES command.

## Error 237

You have tried to load a program that cannot fit in the
space allocated for it. Please allocate a larger space
for the General Buffer in TAS50.OVL and try again.
Please refer to Chapter 3, Main Menu - Utilities - Set Configuration.

## Error 238

The Thru value in the PRINT FORMAT command is less than
the last From value.

## Error 239

The printer is not operating.
screen.
Output will default to the
The program tried to access the printer and couldn’t. It will send the report to the
screen. If you wish, press the ESC key and the program will quit.

## Error 240

The printer number must be from 1 thru 3, program will
default to 1.
This corresponds to LPT1, LPT2 or LPT3.

## Error 241

You are trying to close a file that was opened in a previous program.
Only the program that opens a file may close it. If the program doesn’t close the file
it is done automatically when the program quits.

## Error 242

Only F type pointers are allowed in the FLIST option.

## Error 243

You have specified a file number value that is not legal.

## Error 261

You have specified an array field for the NMENU command
and the number of choices is set to 0.
When you use the array option in the NMENU command you must also tell the
program how many choices there are with the NCH option.

## Error 262

An error has occured while trying to output to the printer.
you want to try again? Y
Do
This is the message that will appear initially if you try to print to the printer and
something goes wrong. If you answer N to this question output will go to the screen.

## Error 263



## Error 264

You are trying to open a file without specifying the
record size. You have probably specified an FD name.
this case you must also provide the record size.
In
You have tried to Execute a program and did not supply a
name.

## Error 265

The record in file: xxx is locked by another user.

## Error 267

The number of active elements is greater than or equal to the
number of maximum elements. In this case you can't add
or change a line.
This occurs in a LISTM command when the number of elements that have values
(ACTV) is equal to the maximum number of elements (MAXA).

## Error 273

Extra Memory Area numbers must be 1-4 only.

## Error 274

You have specified an Extra Memory Area number of 0.

## Error 275

The action you desire would exceed the size of the Extra Memory
buffer area.
If you need to increase the size of the Extra Memory buffer areas see Chapter 3,
Main Menu - Utilities - Set Configuration.

## Error 276

You may set the the special file number to 1, 2 or 3 only.
have tried to set a value outside that range.
You
This would occur in the SSPCF (Set Special File Number) command. I must be from
1 through 3.

## Error 277

The receiving offset field you have specified is not large
enough. It must be of type R or, if type O the display
size must be 10 chrs or more.
In TAS Professional 5.1 all memory offsets can be up to 10 characters in size due to
the 32bit flat access. You need to make sure the receiving field is large enough to
hold this value.

## Error 278

You have used too many internal trap buffers in the program.
You can nest up to 10 XTRAP SAVE commands where you haven't specified a FLD
value.

## Error 279

You have tried to restore the TRAP table from an internal buffer and it was not previously saved.
This would occur when the program encounters a XTRAP RSTR command where
the FLD value is not specified and there are no buffers to restore.
PROFESSIONAL
VERSION 5.1
