# NOCOLOR ABS

## Summary
starting_column - f/c/e - Optional - The number of the column at which to start the display.
The first column on the screen (far left position) is number 1. The default value, if
none is provided, is the current column value as set by the last display or enter to the
screen. This value can be easily determined through the use of the COL() function.
row - f/c/e - Optional - The row or line number to clear. The first row on the screen (top) is
number 1. The default value, if none is provided, is the current row value as set by the
last display or enter to the screen. This value can be easily determined through the use
of the ROW() function.
number_of_characters - f/c/e - Optional - The number of characters to clear. If this option is
not set, the command will clear the remaining line.
color - f/c/e - Optional - If you do not set the NOCOLOR option described below, you can
specify the color value to be used during this operation. If this option is not set, the
program will use the regular color as currently set.
NOCOLOR - Optional - If this option is set the command will use whatever color is presently at
that position on the screen.
ABS - Optional - If this option is set the program will use absolute coordinates when determining where to clear the line. If it is not set and a window is active, the upper left hand
corner of the window is column 1, row 1. By including this option you will achieve
the same result as using the FORCE command except that you don’t have to turn it
off. It is automatically turned off after the command is finished.

## Signature
```
PROGRAM EDITOR
```

## Details
User interface -> Screen control -> clear Line
