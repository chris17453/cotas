# WINDOW DEFINE

## Summary
This command is used for defining a window to be opened later with the WINACT (Window Activate)
command.
WINDEF AT starting_column,starting_row LEN length WDT width
WCOLOR window_color TTLW title_where TTL title
BOX box BCOLOR box_color SHD shadow_where
SCOLOR shadow_color ICOLOR inactive_color SAVEF save_field
WTXT_COLOR window_text_color BTXT_COLOR border_text_color USE_COLORS
starting_column - f/c/e - Required - Upper left corner of the window, column value.
starting_row - f/c/e - Required - Upper left corner of the window, row value.
length - f/c/e - Required - Number of rows long, including box around the window. If there are
two lines within the window and there is a box, the length is 4.
width - f/c/e - Required - Number of columns wide, including box around the window. If the
inside width required is 10 columns and there is a box then the total width is 12.
window_color - f/c/e - Optional - The color value for the main body of the window. If nothing
is specified the program will use the current normal color.
title_where - f/c/e - Optional - If you wish to put a title message at the top of the window you
may choose where it will be. L is left side of window, R is right, C is centered and N
is none. The default is N.
title - f/c/e - Optional - If you specified a title_where value other than N you use this as the
value to be displayed.
box - f/c/e - Optional - The box type to use for defining the edge of the window. If this is
specified you need to take into consideration the 2 extra rows and columns that it
requires when determining length, width, and location. S specifies a single line box,
D is double, and C is custom (dependent on what you have set in TAS50.OVL).
box_color - f/c/e - Optional - If you have specified some sort of box you may also specify the
color for it. If this isn’t set the program will use the window_color as this value also.

