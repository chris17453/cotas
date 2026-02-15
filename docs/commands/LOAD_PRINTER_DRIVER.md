# LOAD PRINTER DRIVER

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS |

## Description

This command, and the PRT_NUM (Printer Number) command, give you great flexibility in choosing which printer and what type of printer you are going to use for any type of output. Any changes made with this command are not permanent. If the user is part of a network the driver for just the one workstation is changed, not the entire network.

## Syntax

```text
LD_PDRV ctl_file_name
```

## Parameters

- **`ctl_file_name`** · `f/c/e` · *Required*

  This is the name of the driver file. It must be in the same directory as TPC32.EXE and have the extension .CTL. All you need to provide is up to 8 characters of file name.

## Comments

This command, and the PRT_NUM (Printer Number) command, give you great flexibility in choosing which printer and what type of printer you are going to use for any type of output. Any changes made with this command are not permanent. If the user is part of a network the driver for just the one workstation is changed, not the entire network.

## Program Editor

`Reports -> Printer -> Driver`
