# DISPLAY ARRAY FIELDS

## Summary
This command will display an array on the screen without having to display each element individually.
Each element will display as a separate line. Through the use of this command and by capturing key
strokes using the INKEY() function you can set up a routine where an array of alpha fields too wide to
be printed to the screen (a 132 column report printed to disk perhaps) can be ‘shifted’ left, right, up, or
down, depending on the user’s input.
DISPF field_name OFST starting_character_position AT starting_column,starting_row WDT
number_of_characters_to_display NUM number_of_lines_to_display COLOR color_values
field_name - fn/v - Required - The name of the array field. This must be an ‘A’ type field. If
you want to start with an element other than the first, the array element number can be
included in the field spec. For example:
fld[element_num]

