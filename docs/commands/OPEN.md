# OPEN

| | |
|---|---|
| **Category** | Command |

## Description

This command will set up a file so that you may access data from it or write records to it from within your program. Use of this command forces you to refer to the file in every other command through the use of the file name as a special alpha constant (sac). However, this command is included to provide upward compatibility to TAS Professional 3.0 programs.

## Syntax

```text
OPEN filename EXT extension LOCK lock_type ERR error_label OWNER owner PATH path FD file_descriptor
```

## Parameters

- **`filename`** · `sac` · *Required*

  A special alpha constant with a maximum of 8 characters that is the actual file name. No extension or path is given as part of the filename. For example: BKARCUST

- **`EXT`** · `extension` · *Required*

  extension

- **`LOCK`** · `lock_type` · *Required*

  lock_type

- **`ERR`** · `error_label` · *Required*

  error_label

- **`OWNER`** · `owner` · *Required*

  owner

- **`PATH`** · `path` · *Required*

  path

- **`FD`** · `file_descriptor` · *Optional*

  file_descriptor
