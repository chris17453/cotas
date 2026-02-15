# NMENU

| | |
|---|---|
| **Category** | Command |
| **Platform** | TAS Professional 5.1 |

## Description

will display (box not shown):
Choice1
Choice2
———
Choice3
Choice4
You can also specify your own dividing lines; however, the first two characters must
be single/double dashed lines (and they will be displayed). If the user chooses
‘Choice3’ the program would return 3 in counter_field. This has nothing to do with
the name (Choice3) but only reflects the fact that Choice 3 is the 3rd choice in the list,
ignoring the single dashed line. If the user presses the Dn Arrow key the ‘cursor’ will
skip the single dashed line.
This can also be an array field reference. If it is, the array option must be Y. All the
same rules apply; the only difference is that there is only one field reference for this
option instead of a list. The list is actually the array elements.

## Parameters

- **`starting_column`** · `f/c/e` · *Required*

  Upper left corner of the menu, column value.

- **`starting_row`** · `f/c/e` · *Required*

  Upper left corner of the menu, row value.

- **`length`** · `f/c/e` · *Required*

  Number of rows long, including box around the menu. If there are
  two lines within the menu the length is 4.

- **`width`** · `f/c/e` · *Required*

  Number of columns wide, including box around the menu. If the
  choices are 10 columns wide within the menu the total width is 12.

- **`ARRAY`** · `Optional` · *Optional*

  If the lines field is actually an array field this must be included in the
  command..

- **`counter_field`** · `fn/v` · *Required*

  This is an I type field that will be used for passing the user’s
  choice to your program.
  NOTE: If the user presses the ESC key this field is set to 0.

- **`number_of_choices`** · `f/c/e` · *Optional*

  How many choices the user has. This is only required
  if the ARRAY option is specified.

- **`choice_expression`** · `f/c/e` · *Optional*

  The user may choose the appropriate menu item by
  entering a single character. The default method is the first capitalized character of
  each item. You can specify an expression which will return a character that will be
  compared to that entered by the user. The program will move the cursor to the
  appropriate line. If not found the bell will sound.

- **`box`** · `f/c/e` · *Optional*

  The box type to use for defining the edge of the menu. If this is
  specified you need to take into consideration the 2 extra rows and columns that it
  requires when determining length, width, and location. S specifies a single line box,
  D is double, and C is custom (dependent on what you have set in TAS50.OVL).

- **`box_color`** · `f/c/e` · *Optional*

  If you have specified some sort of box you may also specify the
  color for it. If this isn’t set the program will use the menu_color as this value also.

- **`menu_color`** · `f/c/e` · *Optional*

  The color value for the main body of the menu. If nothing is
  specified the program will use the current normal color.

- **`choice_color`** · `f/c/e` · *Optional*

  The color used to designate the current cursor location and the
  first capitalized character in each choice line. If not specified the program uses the
  reverse color value.

- **`shadow_where`** · `f/c/e` · *Optional*

  You may specify that a ‘shadow’ is to be displayed behind
  the menu. The shadow_where options are R - right, L - left or N - none. The
  shadow is displayed two characters to the right or left, and one line down from the
  menu. If you specify the appropriate shadow_color value, the resulting display will
  give the menu a 3-D feel. The default value is N.

- **`shadow_color`** · `f/c/e` · *Optional*

  If you have specified L or R for the shadow_where option
  you may also specify the color to be used.

- **`title_where`** · `f/c/e` · *Optional*

  If you wish to put a title message at the top of the menu you
  may choose where it will be. L is left side of menu, R right, C is centered and N is
  none. The default is N.

- **`title`** · `f/c/e` · *Optional*

  If you specified a title_where value other than N you use this as the
  value to be displayed.

- **`help_udf`** · `udf` · *Optional*

  This is the UDF to execute if the user presses the F1 key.
  NOTE: The counter_field value is constantly updated by the program. This way you
  can use that value in a help message if desired and tailor the response to the current
  line or situation.

- **`left_arrow_label`** · `label` · *Optional*

  Where to transfer control if the user presses the left arrow
  key.

- **`right_arrow_label`** · `label` · *Optional*

  Where to transfer control if the user presses the right
  arrow key.

- **`menu_text_color`** · `f/c/e` · *Optional*

  Windows Only - The text color for the menu characters.
  If this value is not specified the program will use black. For more information on
  colors in Windows programs please refer to Chapter 11 - Windows Programming.

- **`choice_text_color`** · `f/c/e` · *Optional*

  Windows Only - This is the text color for the choice (or
  highlight) bar. If this value is not specified the program will use black. For more
  information on colors in Windows programs please refer to Chapter 11 - Windows
  Programming.

- **`HOLD`** · `Optional` · *Optional*

  Optional - If this is included in the command the program will not automatically clear
  the menu upon leaving this command. This will allow you to ‘stack’ menus if desired.

- **`AUTO`** · `DOS only` · *Optional*

  DOS only - Optional - If this is included in the command the program will exit the
  menu automatically when the user presses a key that matches one of the possible
  choice characters. Normally, the cursor would move to that line and the program
  would wait for him/her to press the RETURN or ENTER key.

- **`NOWAIT`** · `DOS Only` · *Optional*

  DOS Only - Optional - If this is included in the command the program will display
  the menu but not wait for any input. Should be used in conjunction with the HOLD
  option or the menu will just flash on the screen and the user will wonder what happened.

- **`USE_COLORS`** · `Windows Only` · *Optional*

  In a normal windows program the colors specified
  in a command are ignored and the program uses the colors in the TP5WIN.INI file.
  The colors in the TP5WIN.INI file become the default colors.. This is due to the
  major differences between DOS and Windows colors. If you use this option the
  program will look to the colors specified in the command first and, if they are 0, will
  then use the colors in the TP5WIN.INI file. For more information on colors in
  Windows please refer to Chapter 11 - Windows Programming.

## Program Editor

`User interface -> 4.0 Nmenu`

## See Also

- [LISTF](LISTF.md)
- [LISTM](LISTM.md)
- [LIST_EXIT](LIST_EXIT.md)
- [AUTOINC](AUTOINC.md)
- [NORSTRT](NORSTRT.md)
