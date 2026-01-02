# PRINT CONTROL CHARACTERS

## Summary
This command is used to send a series of control characters or a single control character to the printer.
You may choose to send a string of characters provided within the program or use a value saved in the
TASPRTR.CTL file (or whichever driver is currently active).
PCHR ctl_name CHR character_list PTW print_to
ctl_name - f/c/e - Optional - The name of the series of characters to send. It must correspond
to a name in the currently loaded CTL driver. If you are going to print a string of
characters then this value should be ‘’ (null string).
character_list - f/c/e1,f/c/e2,...,f/c/ex - Optional - This must be a series of characters (CHR()
functions or B type fields). The program will output the first character of each field.
If this option is specified the program will use this string instead of the ctl_name,
whether or not it is a legal name. A series of characters might be:
CHR(27),CHR(91),...
print_to - PD - Optional - You can use this option to specify where to print the characters.
The option ASK (a regular print_to option) is not available. The only options
available are P - printer or D - default.

## Signature
```
COMMENTS
For more information on the printer driver files please see Chapter 3, Main Menu -Utilties. Output
will only go to printer or disk; if output is directed to the screen (PON S) nothing will happen.
```

## Details
PROGRAM EDITOR
Reports -> Chr strg
