# OWNER

| | |
|---|---|
| **Category** | Command |

## Description

This command is used to encrypt, or protect against unauthorized usage, a standard TAS Professional 5.1 file.

## Syntax

```text
OWNER name FILE filename/@file_number CLR/SET ENC RDOK
```

## Parameters

- **`name`** · `f/c/e` · *Required*

  - This is either the ‘password’ provided during the original execution of this OWNER command in relation to the specific file, or the new ‘password’ for this file. The field must be of A type and cannot be more than 8 characters in length.

- **`filename/@file_number`** · `f/c/e` · *Required*

  This is the name or number of the file being set. The file must currently be opened.

- **`CLR/SET`** · `option` · *Required*

  What to do. If SET is chosen then the protection will be set; if CLR then any protection currently provided will be removed.

- **`ENC`** · `option` · *Optional*

  If this is a SET operation then you can choose to encrypt the file also. This means that if anyone should try to access that file through any other means (debugger, other applications that use Btrieve, etc.) they will not be able to make sense of the data. If this is being accomplished on an existing file the process may take a considerable length of time depending on the number of records active. To choose this option include ENC in the command.

- **`RDOK`** · `option` · *Optional*

  If this is a SET operation and this option is included in the command then subsequent programs that access this file, but don’t set the OWNER option during the OPEN or OPENV (Open Variable) command, will be able to read records from this file but won’t be able to update them.
