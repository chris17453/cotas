# NMENU

## Summary
Create a pull down menu and allow the user to make a choice. The choice is returned to the program.
This is an alternative to the MENU command.
NMENU lines AT starting_column,starting_row LEN length WDT width ARRAY
CNTR counter_field NCH number_of_choices CHXPR choice_expression
BOX box BCOLOR box_color MCOLOR menu_color
CCOLOR choice_color SHD shadow_where SCOLOR shadow_color
TTLW title_where TTL title HELP help_udf LTA left_arrow_label
RTA right_arrow_label MTXT_COLOR menu_text_color
CTXT_COLOR choice_text_color HOLD AUTO NOWAIT USE_COLORS
lines - f/c/e1, f/c/e2,..., f/c/ex - Required - The fields/constants that make up the menu items. A
menu can have up to 20 different items. You can also put single (—) or double (==)
dividing lines between items. The user will not be able to choose these items and they
will be ignored when returning the counter_field value. For example:
‘Choice1’,’Choice2',’—’,’Choice3',’Choice4' .....

