# OPEN NON-TAS FILE

| | |
|---|---|
| **Category** | Command |

## Description

This is a TAS Professional 3.0 command here for compatibility. The preferred method is to use the OPENV command.

## Syntax

```text
OPNO filename TYPE file_type SIZE size BUFF buffer_field_name
```

## Parameters

- **`filename`** · `f/c/e` · *Required*

  The name of the file to be opened. You may specify the entire file name including disk drive specifier, path, filename and extension.

- **`file_type`** · `FX` · *Required*

  The type of file being opened. The options are: F - Fixed length records (SDF) or X - Text type and B - non-TAS Pro 5.1 Btrieve files. TEXT is for letters, or others that have records that are not all the same size. Each TEXT record is terminated by a CR/LF (carriage return/line feed - 0Dh/0Ah) pair. In FIXED files all records are of the same length; they are also terminated with CR/LF pairs.

- **`size`** · `f/c/e` · *Required*

  You must specify the maximum size of the record. This must also include the final CR/LF pair (2 extra characters).

- **`buffer_field_name`** · `fn/v` · *Required*

  You must specify the buffer that will hold the record when it is read. You may use multiple fields to make up a buffer; however, you would specify just the first field in this option. Don’t forget to include the extra CR/LF bytes at the end of the record in both the size of the buffer and the size of the record.

## Comments

This is the equivalent of the TAS Professional 3.0 command Open Non-TAS File.
