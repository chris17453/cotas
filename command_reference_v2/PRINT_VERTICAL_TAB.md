# PRINT VERTICAL TAB

## Summary
The command will move the next output to the line specified in the command.
PVERT line_number PTW print_to
line_number - f/c/e - Required - The line number to move to. If the value specifies a row
before (or above) the current row location, this command will be ignored.
print_to - SPD - Optional - You can use this option to specify the output device. The option
ASK (a regular print_to option) is not available. The only options available are S screen, P - printer or D - default.

## Signature
```
COMMENTS
The column value is automatically reset to column 1 after this command is executed. The command
does a series of CR/LF's (0dh/0ah) until the appropriate row is reached.
```

## Details
PROGRAM EDITOR
Reports -> Vert tab
PRINTER NUMBER (DOS only)
Use this command to switch among the 3 LPT printers: LPT1, LPT2 or LPT3.
PRT_NUM lpt_number
lpt_number - f/c/e - Required - The printer number to use as the printer output device. This
may be 1, 2 or 3.
COMMENTS
This value defaults to what you have set in the TAS50.OVL file. You can also change the printer driver
with the LD_PDRV (Load Printer Driver) command.
PROGRAM EDITOR
Reports -> Printer -> Number
