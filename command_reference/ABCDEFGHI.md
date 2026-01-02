# ABCDEFGHI

## Summary
You may include a maximum of 80 fields, constants, or expressions.
starting_column - f/c/e - Optional - The number of the column at which to start the display.
The first column on the screen (far left position) is number 1. The default value, if
none is provided, is the current column value as set by the last display or enter to the
screen. This value can be easily determined through the use of the COL() function.
starting_row - f/c/e - Optional - The row or line number to display at. The first row on the
screen (top) is number 1. The default value, if none is provided, is the current row
value as set by the last display or enter to the screen. This value can be easily determined through the use of the ROW() function.
WAIT - Optional - If this option is included, it will force the program to wait after completing
this command for the user to press any key at the keyboard before continuing with the
next command.
NOCR - Optional - Normally, this command will print a CR/LF (0dh/0ah) after the
message_list is printed. If this option is included in the command that will not
happen. That will force the next line printed to print on the same line until or unless
the line is automatically truncated or a print command without this option is executed.
NOTE: If you use this feature and find that your lines are not being printed properly,
make sure they don’t extend off the edge of the screen or printer. If they do, use the
WRAP command (for long fields) or split up the lines.

