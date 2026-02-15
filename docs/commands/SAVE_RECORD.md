# SAVE RECORD

| | |
|---|---|
| **Category** | Command |

## Description

This command is used to save records to any file, TAS or non-TAS.

## Syntax

```text
SAVE filename/@file_number NOCNF NOCLR GOTO goto_label ERR error_lbl
```

## Parameters

- **`filename/@file_number`** · `fn/v` · *Required*

  file_expr - Required - This is the name or number of the file that will receive the record.

- **`NOCNF`** · `sac` · *Optional*

  Optional - The normal process is for the program to confirm that it is okay to save the record. If you do not want the program to confirm, include this option in the

- **`NOCLR`** · `sac` · *Optional*

  

- **`GOTO`** · `sac` · *Optional*

  

- **`goto_label`** · `e` · *Optional*

  

- **`ERR`** · `sac` · *Optional*

  
