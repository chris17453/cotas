# PAINT

## Summary
Use this command to ‘paint’ a block of color on the screen.
PAINT color FROM up_lt_col,up_lt_row THRU low_rt_col,low_rt_row
color - f/c/e - Required - The color to paint. Must be a value from 1 through 255.
up_lt_col - f/c/e - Required - The upper left column value. Together with the upper left row
value defines the upper left corner of the block. The upper left corner of any window
is 1,1.
up_lt_row - f/c/e - Required - The upper left row value. Together with the upper left column
value defines the upper left corner of the block. The upper left corner of any window
is 1,1.
low_rt_col - f/c/e - Required - The lower right column value. Together with the lower right
row value defines the lower right corner of the block. If the values are beyond the
current window they will be truncated to those values.
low_rt_row - f/c/e - Required - The lower right row value. Together with the lower right
column value defines the lower right corner of the block. If the values are beyond the
current window they will be truncated to those values.

## Signature
```
COMMENTS
The same effect can also be accomplished using the MOUNT command. Please see Chapter 8,
Screen/Report Format Information for more information about specifying color blocks in a screen
format.
```

## Details
PROGRAM EDITOR
User interface -> Color control -> Paint
