# PRINTER NUMBER

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS only |

## Description

Use this command to switch among the 3 LPT printers: LPT1, LPT2 or LPT3.

## Syntax

```text
PRT_NUM lpt_number
```

## Parameters

- **`lpt_number`** · `f/c/e` · *Required*

  The printer number to use as the printer output device. This may be 1, 2 or 3.

## Comments

This value defaults to what you have set in the TAS50.OVL file. You can also change the printer driver with the LD_PDRV (Load Printer Driver) command.

## Program Editor

`Reports -> Printer -> Number`
