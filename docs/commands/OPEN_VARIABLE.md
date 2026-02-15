# OPEN VARIABLE

| | |
|---|---|
| **Category** | Command |

## Description

This command will set up a file so that you may access data from it or write records to it from within your program. This is the ‘preferred’ method of opening files since it affords more flexibility than the standard OPEN command. With this command you may open dBaseIII+, Text, and Fixed Length records and treat them as though they were standard TAS Pro 5.1 files. This means you can do standard searches on the files, read and write records at will and in general treat them just as though they were Btrieve files. ALL commands that access files can be used to access these non-TAS files also. You would also use this command to open non-TAS Btrieve files where the file layout is a group of defined fields in memory (predefined or added during operation) instead of a standard defined record in the data dictionary. This command brings a level of flexibility to file handling not found in other 4GLs. You can also define non-TAS 4.0 files in the file Data Dictionary. Make sure you set the proper file type when initializing the file. When a file is in the file Data Dictionary all the rules that apply to a standard TAS Pro 5.1 file also apply to it, including setting up the file name, etc. This gives you the ability to access non-TAS files as easily as regular TAS files. NOTE: The maximum number of files that may be open at one time with either the OPEN or OPENV command is 100. This limit applies across all programs that are currently active.

## Syntax

```text
OPENV filename FNUM file_number TYPE file_type EXT extension
LOCK lock_type ERR error_label OWNER owner PATH path
FD file_descriptor SIZE size BUFF buffer_field_name
CREATE NOCLR
```

## Parameters

- **`filename`** · `f/c/e` · *Required*

  The name of the file to be opened. You may specify the entire file name including disk drive specifier, path, filename and extension. If this is a TAS Pro 5.1 file then the extension defaults to .B (other extensions are provided by the extension option or are blank by a previously set default). The filename can be a field, expression or alpha constant. If a path is specified in FILELOC.B and not here, that value will be used. If a path is included here it will be used no matter what’s in FILELOC.B for this file.

- **`file_number`** · `fn/v` · *Required*

  After the program opens a file (of any type) it returns a value that represents the number of the file opened. It is used by all the other commands that access the file for any purpose. This must be an I type field.

- **`file_type`** · `TDFXB` · *Optional*

  The type of file being opened. The options are: T - TAS Pro 5.1, D - dBASEIII+, F - Fixed length records (SDF), X - Text type and B - non-TAS Pro 5.1 Btrieve files. TEXT is for files such as letters, or others that have records that are not all the same size. Each TEXT record is terminated by a CR/LF (carriage return/line feed - 0Dh/0Ah) pair. In FIXED files all records are of the same length; they are also terminated with CR/LF pairs. D is for files created by dBASE III+ or those that fit the same requirements. You must still set the size for the D files, although the program knows where the data actually starts and can keep track of the number of records. NOTE: This value will also default to that value set in FILELOC.B for file type.

- **`extension`** · `f/c/e` · *Optional*

  Optional - If you wish to open a copy of the file with an extension other than the current default value (.B or whatever the company_code value is set to), you may specify up to 3 characters here.

- **`lock_type`** · `NRFXA` · *Optional*

  Optional - The type of locking to use. N - none, R - record, F - file, X fix, and A - accelerated access (essentially file locks). The default is N - none. By specifying A you can significantly speed up operations to Btrieve files as long as you are not concerned with recovering data in case of a failure and you are running on a single-user system. Specifying X tells the program to open the Btrieve file in readonly or FIX mode. You will then be able to read records in direct mode (i.e., without an index). This feature is also part of the Reindex and Restructure routines and should allow you to recover from some Btrieve errors without restoring the file from a backup.
  
  NOTE: This option is ignored for non-TAS files. They are opened automatically as shared if the multi-user flag is set to Y in the setup.

- **`error_label`** · `label` · *Optional*

  Optional - Where to transfer control if an error occurs while attempting to open the file. If this is omitted you may check for an error opening the file through the use of the OPEN() function. If you do not want to have the program display an error if the file can’t be opened use the label name NO_ERR.

- **`owner`** · `f/c/e` · *Optional*

  Optional - A maximum 8 character code to be specified if the file is protected with the OWNER command. This applies to Btrieve files only.

- **`path`** · `f/c/e` · *Optional*

  Optional - You may specify the path for this file if you wish to override the value in the FILELOC.B file.
  NOTE: The path value must end with a backslash character. See the CHK_PATH_CHR() UDF in TASRTNS.LIB for an easy method of making sure that the last backslash is always there.

- **`file_descriptor`** · `f/c/e` · *Optional*

  Optional - You may specify the file_descriptor name for this file if it is not in FILELOC.B and it is defined in the file Data Dictionary. This allows you to compile a program using one data dictionary and then run it on a system that has a different data dictionary without having to worry whether or not that FD exists in the new dictionary.

- **`size`** · `f/c/e` · *Optional*

  Optional - In the case of a non-TAS Pro 5.1 file you must specify the maximum size of the record. In the case of a text or fixed length type file this must also include the final CR/LF pair (2 extra characters).
  NOTE: If the non-TAS file is defined in the data dictionary you do not need to include this value.

- **`buffer_field_name`** · `fn/v` · *Optional*

  Optional - In the case of a non-TAS Pro 5.1 file you must specify the buffer that will hold the record when it is read, unless it is defined in the file Data Dictionary. This allows you to compile a program using one data dictionary and run it on a system that has a different data dictionary without having to worry whether or not that FD exists in the new dictionary. You may use multiple fields to make up a buffer; however, you would specify just the first field in this option. You may also define a buffer in a program for a TAS Pro 5.1 file. To use the buffer in the program instead of the normal buffer created in memory specify the name of the first field here just as you would with a non-TAS file.
  If you are specifying a buffer for a non-TAS file don’t forget to include the extra CR/LF bytes at the end of the record in both the size of the buffer and the size of the record if stipulated in this command.
  NOTE: Non-TAS files that are defined in the data dictionary allocate their own buffers automatically just like regular TAS files.

- **`CREATE`** · `flag` · *Optional*

  Optional - If this option is included in the command the program will automatically create a new file upon opening. If the file exists it will be deleted. This only applies to file types: F and X. To create a dBaseIII+ file use the Initialize File option in the Database sub-menu. For more information please refer to Chapter 3, Main Menu Programs.

- **`NOCLR`** · `flag` · *Optional*

  Optional - Normally the buffer is cleared when a non-TAS Pro 5.1 file is opened. If you have data in it already, and do not want to clear it then include this option in the command.

## Comments

If the file is defined in the File Data Dictionary any fields that are a part of it cannot be accessed in the program until the file is opened. Any attempt to do so will generate a warning error at runtime.

## See Also

- [CLOSE](CLOSE.md)
- [FIND](FIND.md)
- [SAVE](SAVE.md)
- [DEL](DEL.md)
- [FINDV](FINDV.md)
- [DEL](DEL.md)
