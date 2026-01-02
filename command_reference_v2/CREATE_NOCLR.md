# CREATE NOCLR

## Summary
filename - f/c/e - Required - The name of the file to be opened. You may specify the entire file
name including disk drive specifier, path, filename and extension. If this is a TAS Pro
5.1 file then the extension defaults to .B (other extensions are provided by the extension option or are blank by a previously set default). The filename can be a field,
expression or alpha constant. If a path is specified in FILELOC.B and not here, that
value will be used. If a path is included here it will be used no matter what’s in
FILELOC.B for this file.
file_number - fn/v - Required - After the program opens a file (of any type) it returns a value
that represents the number of the file opened. It is used by all the other commands
that access the file for any purpose. This must be an I type field.
file_type - TDFXB - Optional - The type of file being opened. The options are: T - TAS Pro
5.1, D - dBASEIII+, F - Fixed length records (SDF), X - Text type and B - non-TAS
Pro 5.1 Btrieve files. T is the default value; it need not be specified. TEXT is for
files such as letters, or others that have records that are not all the same size. Each
TEXT record is terminated by a CR/LF (carriage return/line feed - 0Dh/0Ah) pair. In
FIXED files all records are of the same length; they are also terminated with CR/LF
pairs. D is for files created by dBASE III+ or those that fit the same requirements.
You must still set the size for the D files, although the program knows where the data
actually starts and can keep track of the number of records.
NOTE: This value will also default to that value set in FILELOC.B for file type.
extension - f/c/e - Optional - If you wish to open a copy of the file with an extension other than
the current default value (.B or whatever the company_code value is set to), you may
specify up to 3 characters here.
lock_type - NRFXA - Optional - The type of locking to use. N - none, R - record, F - file, X fix, and A - accelerated access (essentially file locks). The default is N - none. By
specifying A you can significantly speed up operations to Btrieve files as long as you
are not concerned with recovering data in case of a failure and you are running on a
single-user system. Specifying X tells the program to open the Btrieve file in readonly or FIX mode. You will then be able to read records in direct mode (i.e., without
an index). This feature is also part of the Reindex and Restructure routines and should
allow you to recover from some Btrieve errors without restoring the file from a
backup.

## Signature
```
NOTE: This option is ignored for non-TAS files. They are opened automatically as
shared if the multi-user flag is set to Y in the setup.
```

