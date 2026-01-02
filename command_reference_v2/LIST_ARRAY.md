# LIST ARRAY

## Summary
This command will generate a list of array values. The user can scroll through the list using the UP and
DOWN ARROW keys, PgUp and PgDn, or enter a single character, and depending on your options,
find a value in the list.
All of the lookup windows in the TAS Professional 5.1 utilities use either this command or the LISTF
(List File) command.
LISTM field_list ENTER enter_udf SRCH search_udf CHSE chse_expr HELP help_udf
OTH other_udf ACTV number_of_active_elements MAXA max_number_of_elements
CNTR counter_field ON_MOVE move_udf CCOLOR choice_color STYP search_type
FLINE first_line_fields CBF chrs_btwn_fields BLNES num_of_blank_lines
ETXT_COLOR enter_text_color CTXT_COLOR choice_text_color RND NOWAIT MENU
NOADD NOSHIFT USE_TRAPS INS_AT_END USE_COLORS
field_list - f/c/e1,f/c/e2,...,f/c/ex - Required - The array fields, constants or expressions to be
used in the list.
NOTE: You can separate the fields on the list by putting spaces between the fields.
For example:
field1,’ ‘,field2,’ ‘,field3
enter_udf - udf - Optional - If you specify this option the program will execute the UDF each
time the user presses the ENTER or RETURN key (Change or Add a record), the
DEL key (Delete a record) or INS (Insert a record). The type of entry and the location
of the data are passed to the routine through the ETYP() function and the value in
counter_field.

