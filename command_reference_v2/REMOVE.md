# REMOVE

## Summary
col - f/c/e - Required - The column value for the upper left corner of the hot spot. The first
column on the screen (far left position) is number 1. This value is always based on
the largest window, generally the window that is created when the program is run.
Even if the active window is smaller the column and row values are for the base
window. This value is required only when creating the hot spot initially.
row - f/c/e - Required - The row value for the upper left corner of the hot spot. The first row
on the screen (top row) is number 1. This value is always based on the largest
window, generally the window that is created when the program is run. Even if the
active window is smaller the column and row values are for the base window. This
value is required only when creating the hot spot initially.
length - f/c/e - Required - The length or height of the hot spot in rows. This is required only
when creating the hot spot initially.
width - f/c/e - Required - The width of the hot spot in columns. This is required only when
creating the hot spot initially.
key - f/c/e - Optional - The keyboard key this hot spot will emulate when it is clicked. This is
required only when creating the hot spot initially. If you don't specify a key value, or
one doesn't match those available, the program will ignore any user clicks on the hot
spot. The following are the acceptable key values:
F1 through F10
SF1 through SF10 (Shift F1 etc.)
^F1 through ^F10 (Ctrl F1 etc.)
@F1 through @F10 (Alt F1 etc.)
^A through ^Z (Ctrl A etc.)
@A through @Z (Alt A etc.)
ESC, UP (up arrow), DOWN (down arrow), LTA (left arrow), RTA (right arrow),
HOME, END, PGUP, ^PGUP, PGDN, ^PGDN, INSERT, DELETE, WDLT (ctrl left
arrow), WDRT (ctrl right arrow), TAB and BCKTAB.
An example of this option would be: ... Key 'ESC' ...
hot_spot_handle - fn/v - Required - When you initially create the hot spot you must supply a
field that will hold the hot spot number or handle. This is how you will refer to this
hot spot if you want to remove it from the screen. This must be an I type field.
REMOVE - Optional - Remove the hot spot from the screen. If the NUM (or handle) value is 0
then all hot spots will be removed from the screen.

## Details
PROGRAM EDITOR
Win Commands-> Hot Spot
