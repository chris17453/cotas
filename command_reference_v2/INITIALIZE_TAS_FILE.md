# INITIALIZE TAS FILE

## Summary
This command is used to create (initialize) TAS Professional 5.1 files.
INIFLE filename SPECS specifications CNF
filename - f/c/e - Required - The name of the file to be created. The file name must include the
standard file name, the extension, and the file path, if any.
specifications - fn/v - Required - This is the field containing the information Btrievetm will use
to create the data file.
CNF - Optional - If this option is included in the command the program will confirm that the
user wants to create the file before trying to create it. However, if the file already
exists, the program will alert the user that the file exists and confirm that they wish to
initialize it irregardless of this option.

## Signature
```
COMMENTS
You must have a full copy of the developer’s version of Btrievetm available from Novell before attempting to use this command. The requirements for properly setting up the specifications buffer are beyond
the scope of this document.
```

## Details
PROGRAM EDITOR
System -> File commands -> Init File
