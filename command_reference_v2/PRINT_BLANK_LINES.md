# PRINT BLANK LINES

## Summary
Use this command to print a certain number of blank lines or spaces to the printer, disk file or screen.
PBLNK number PTW print_to
number - f/c/e - Required - The number of blank lines to print.
print_to - SPD - Optional - You can use this option to specify where to print the blank lines.
The option ASK (a regular print_to option) is not available. The only options
available are S - screen, P - printer or D - default.

## Signature
```
COMMENTS
This command accomplishes its task through the repeated use of the CR/LF pair (carriage return/line
feed - 0Dh/0Ah).
```

## Details
PROGRAM EDITOR
Reports -> Blank lns
