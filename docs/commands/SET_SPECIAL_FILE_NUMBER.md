# SET SPECIAL FILE NUMBER

| | |
|---|---|
| **Category** | Command |

## Description

This is a TAS Professional 3.0 command here for compatibility. The preferred method is to use the OPENV/FINDV commands.

## Syntax

```text
SSPCF non_TAS_file_num
```

## Parameters

- **`non_TAS_file_num`** · `f/c/e` · *Required*

  This is the number of the non-TAS file opened with the OPNO command. This must resolve to a value of 1 through 3. The files are numbered in the order they are opened.

## Comments

This is the equivalent of the TAS Professional 3.0 command Set Special File Num.

## Program Editor

`3.0 Commands -> Q - Sspcf`
