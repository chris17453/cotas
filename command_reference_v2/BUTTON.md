# BUTTON

## Summary
length - f/c/e - Required - The length or height of the button in rows. We recommend that a
button be at least 2 rows in height. This is required only when creating the button
initially.
width - f/c/e - Required - The width of the button in columns. This is required only when
creating the button initially.
main_color - f/c/e - Optional - The background color of the button. If no value is entered here
then the program will use the default color provided by Windows. For more information on Windows colors please see Chapter 11 - Windows Programming. This
value can be specified only when creating the button initially.
caption_color - f/c/e - Optional - The button text color. If no value is entered here then the
program will use the default color provided by Windows. For more information on
Windows colors please see Chapter 11 - Windows Programming. This value can
be specified only when creating the button initially.
caption - f/c/e - Optional - The caption or text that will appear on the face of the button. If you
don't specify a value here the button will be blank. This value can be specified only
when creating the button initially.
key - f/c/e - Optional - The keyboard key this button will emulate when it is clicked. This is
required only when creating the button initially. If you don't specify a key value, or
one doesn't match those available, the program will automatically treat the button as
the ENTER key. The following are the acceptable values:
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
save_to_field - fn/v - Optional - If you want to save the current button list so that you can
temporarily create a different list you would save the current list pointer or handle to
this field. The field must by of type R for this option. When this value is specified all
other options, other than ON or OFF are ignored.
restore_from_field - fn/v - Optional - If you have saved a button list pointer or handle
previously with the SAVE_TO option you would restore it with this option. When
this value is specified all other options are ignored other than ON and OFF. The field
for this option must be of type R.
using_holder_field - fn/v - Optional - If you have previously saved a button list with the
SAVE_TO option and now want to turn those buttons on or off without disturbing the
current button list you would use this option. This will allow you to manipulate more
than one button list on the screen at a time. The only options available with this
option are ON and OFF. If you want to remove the buttons permanently you should
RSTR_FROM first and then REMOVE. The field in this option must be of type R.

