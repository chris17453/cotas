# PRINT BOX

## Summary
Use this command to display a box on the screen.
PBOX type AT up_lt_col,up_lt_row LEN length WDT width CLR COLOR color ABS
type - SDC - Optional - The box type options. S - single line, D - double line and C - custom.
The default is S.
up_lt_col - f/c/e - Required - The upper left column value. Together with the upper left row
value defines the upper left corner of the box. The upper left corner of any window is
1,1.
up_lt_row - f/c/e - Required - The upper left row value. Together with the upper left column
value defines the upper left corner of the box. The upper left corner of any window is
1,1.
length - f/c/e - Optional - The vertical depth of the box. For example, if the starting row value
is 5 and you want the lower horizontal portion of the box to be on row 10 then the
length is 6. If no value is given the default is 0. In this case the program will print a
single horizontal line.
width - f/c/e - Optional - The horizontal width of the box. For example, if the starting column
value is 5 and you want the far right corner of the box to be on column 10 then the
width is 6. If no value is given the default is 0. In this case the program will print a
single vertical line.
CLR - Optional - You can use this option if you want to clear a box that currently exists.

