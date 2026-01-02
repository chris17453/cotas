# PRINT FORMAT

## Summary
This command will print a line or group of lines from a Report Format that has been previously
mounted.
PFMT format_lines THRU thru_line AT starting_column,starting_row WAIT NOCR PTW
print_to ABS
format_lines - f/c/e1, f/c/e2,..., f/c/ex - Required - This is (these are) the line number(s) to
print of the appropriate format. The first line in the format is line 1 and blank lines are
counted also. Up to 80 different format lines may be included. The line numbers
need not be sequential.
thru_line - f/c/e - Optional - If you include this option, the program will print all the report
format lines starting with the last one specified in the format_lines list through the
one specified in this option. For example:
PFMT 3,5,7 THRU 10
will print lines 3,5,7,8,9 and 10.
starting_column - f/c/e - Optional - The number of the column at which to start the printing.
The first column on the screen or printer (far left position) is number 1. The default
value, if none is provided, is the current column value as set by the last output. This
value can be easily determined through the use of the COL() function for the screen or
PCOL() for the printer.
starting_row - f/c/e - Optional - The number of the row at which to start the display. The first
row on the screen or printer (top) is number 1. The default value, if none is provided,
is the current row value as set by the last output. This value can be easily determined
through the use of the ROW() function for the screen or PROW() for the printer.
WAIT - Optional - If this option is included, it will force the program to wait after completing
this command for the user to press any key at the keyboard before continuing with the
next command.
NOCR - Optional - Normally, this command will print a CR/LF (0dh/0ah) after each format
line. If this option is included in the command that will not happen. That will force
each line, or the next line printed, to be printed on the same line until the line is
automatically truncated or a print command without this option is executed.
NOTE: If you use this feature and find that your lines are not being printed properly
make sure they don’t extend off the edge of the screen or printer. If they do use the
WRAP command (for long fields) or split up the lines.
print_to - SPD - Optional - You can use this option to specify where to print. The option
ASK (a regular print_to option) is not available. The only options available are S screen, P - printer or D - default.
ABS - Optional - If this option is included in the command the program will ignore any windows currently active and will place the box according to the location calculated from
the absolute upper left hand corner of the screen. Normally, the location would be
figured from the upper left hand corner of the current window. This only applies
when displaying to the screen.

