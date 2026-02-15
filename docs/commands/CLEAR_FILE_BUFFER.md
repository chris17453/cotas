# CLEAR FILE BUFFER

| | |
|---|---|
| **Category** | Command |

## Description

This command will clear the internal record buffer for a particular file. It can also be used to clear the record number holder.

## Syntax

```text
CLR filename/@file_number BUFF/REC
```

## Parameters

- **`filename/@file_number`** · `file_expr` · *Required*

  The name or number of the file to be cleared.

- **`BUFF/REC`** · `option` · *Optional*

  What to clear. If the option is REC the program will clear just the record number holder, effectively setting the record internally as not active. If you saved the record in that situation, it would be treated as though it were a new record. If the option is BUFF, both the record number holder and the record buffer are cleared. The default value is BUFF.

- **`filename/@number`** · `file_expr` · *Required*

  The name or number of the file to be cleared.

## Comments

If a record is active in memory, there is data in both the buffer and record number holder. If this record is saved to disk it will update the record currently at that record number. If the record number holder is cleared, the program will save a new record. Through the use of this command, if you clear just the
