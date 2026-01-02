# PICTURE

## Summary
PICTURE (Windows only)
Use this command to put a .BMP (bitmap), .ICO (icon) or .WMF (Windows metafile) picture on the
screen. The user can also click on this picture with their mouse. Each time they click the picture it will
be just as though they had pressed the key that you setup as the related value.
PICTURE AT col,row LEN length WDT width FILE file_name KEY related_key
NUM picture_handle NO_BORDER AUTOSCROLL LOAD REMOVE
col - f/c/e - Required - The column value for the upper left corner of the picture. The first
column on the screen (far left position) is number 1. This value is always based on
the largest window, generally the window that is created when the program is run.
Even if the active window is smaller the column and row values are for the base
window. This value is required only when creating the picture initially.
row - f/c/e - Required - The row value for the upper left corner of the picture. The first row on
the screen (top row) is number 1. This value is always based on the largest window,
generally the window that is created when the program is run. Even if the active
window is smaller the column and row values are for the base window. This value is
required only when creating the picture initially.
length - f/c/e - Required - The length or height of the picture in rows. This is required only
when creating the picture initially.
width - f/c/e - Required - The width of the picture in columns. This is required only when
creating the picture initially.
file_name - f/c/e - Optional - The name of the file to be loaded, including full path and
extension. This would be specified when ever you want to display a new picture on
the screen. You can also specify the file when creating a new picture. If you don't
supply the file name the program will create a blank picture frame.
You can also load new pictures in a current picture frame by specifying the file_name
and the LOAD option in this command.
key - f/c/e - Optional - The keyboard key this picture will emulate when it is clicked. This is
required only when creating the picture initially. If you don't specify a key value, or
one doesn't match those available, the program will ignore any user clicks on the
picture. The following are the acceptable key values:
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
picture_handle - fn/v - Required - When you initially create the picture you must supply a
field that will hold the picture number or handle. This is how you will refer to this
picture whenever you want to load a new file or to remove the picture from the screen.
This must be an I type field.

