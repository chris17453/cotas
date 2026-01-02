# ENTER

## Summary
Use this command to allow the user to enter data into a field.
ENTER fieldname MASK mask HELP help_label/@expression AT starting_column,row COLOR
color PRE pre_enter_expression POST post_enter_expression
DFLT default_value VLD valid_check VLDM valid_message
ARRAY CNTR array_counter ENUM enumerated_field DO do_udf
UPAR up_arrow_goto_label GROUP group_num
UP ACR PSWD NOREV AUTO_SRCH NOCLICKON NOCLICKOFF
fieldname - fn/v - Required - The name of the field being entered. This field must have been
defined within the program or be part of a file that has been opened. If entering an A
type field the maximum number of characters that may be entered is 256. If you have
use of the TASEdit (tm) program you should use that program for fields larger than
the maximum size.
mask - f/c/e - Optional - In the case of an A type field this would be the allowable entry
characters. If this option is set and the user enters a character not included in this
field, the program will sound the bell and not allow the character to be entered.
This is not the same as the PICTURE. The PICTURE allows more control and can
be defined for the field within the DEFINE command or data dictionary.
help_label/@expression - label or f/c/e - Optional - You may provide a specific help message
for this ENTER command. It may be of two different types. You may specify a label
name. In that case the program passes control to that label and when the program
returns, it continues with the ENTER command. You may also specify an expression
of some type preceded by the at sign (@). This might be as simple as an alpha
constant. For example:
@’This field is the customer name.’
Or it could be a UDF that passes the number of the record in ERRMSG.B. For
example:
@help(1009)
starting_column - f/c/e - Optional - The number of the column at which to start the entry. The
first column on the screen (far left position) is number 1. If the field is not part of a
mounted screen format you must supply the column and row where entry is to begin.
If you don’t provide a column and row, an error message will be displayed and the
program will continue with the next command.
row - f/c/e - Optional - The row or line number to use in the entry. The first row on the screen
(top) is number 1. If the field is not part of a mounted screen format you must supply
the column and row where entry is to begin. If you don’t provide a column and row,
an error message will be displayed and the program will continue with the next
command.
color - f/c/e - Optional - This option can set the color to be used when entering this field. The
color will revert back to the display color after the ENTER command is completed.
The color value must resolve to a standard color value.

