# ENTER

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to allow the user to enter data into a field.

## Syntax

```text
ENTER fieldname MASK mask HELP help_label/@expression AT starting_column,row COLOR
color PRE pre_enter_expression POST post_enter_expression
DFLT default_value VLD valid_check VLDM valid_message
ARRAY CNTR array_counter ENUM enumerated_field DO do_udf
UPAR up_arrow_goto_label GROUP group_num
UP ACR PSWD NOREV AUTO_SRCH NOCLICKON NOCLICKOFF
```

## Parameters

- **`fieldname`** · `fn/v` · *Required*

  The name of the field being entered. This field must have been defined within the program or be part of a file that has been opened. If entering an A type field the maximum number of characters that may be entered is 256. If you have use of the TASEdit (tm) program you should use that program for fields larger than the maximum size.

- **`mask`** · `f/c/e` · *Optional*

  Optional - In the case of an A type field this would be the allowable entry characters. If this option is set and the user enters a character not included in this field, the program will sound the bell and not allow the character to be entered. This is not the same as the PICTURE. The PICTURE allows more control and can be defined for the field within the DEFINE command or data dictionary.

- **`help_label/@expression`** · `label or f/c/e` · *Optional*

  Optional - You may provide a specific help message for this ENTER command. It may be of two different types. You may specify a label name. In that case the program passes control to that label and when the program returns, it continues with the ENTER command. You may also specify an expression of some type preceded by the at sign (@). This might be as simple as an alpha constant. For example:
  @’This field is the customer name.’
  Or it could be a UDF that passes the number of the record in ERRMSG.B. For example:
  @help(1009)

- **`starting_column`** · `f/c/e` · *Optional*

  Optional - The number of the column at which to start the entry. The first column on the screen (far left position) is number 1. If the field is not part of a mounted screen format you must supply the column and row where entry is to begin. If you don’t provide a column and row, an error message will be displayed and the program will continue with the next command.

- **`row`** · `f/c/e` · *Optional*

  Optional - The row or line number to use in the entry. The first row on the screen (top) is number 1. If the field is not part of a mounted screen format you must supply the column and row where entry is to begin. If you don’t provide a column and row, an error message will be displayed and the program will continue with the next command.

- **`color`** · `f/c/e` · *Optional*

  Optional - This option can set the color to be used when entering this field. The color will revert back to the display color after the ENTER command is completed. The color value must resolve to a standard color value.

- **`pre_enter_expression`** · `lexpr` · *Optional*

  Optional - This is an option that you can use to make sure that all requirements are met before allowing entry to this field. If the expression returns .F., the program will not allow the entry. NOTE: A very important feature is the ability to use a UDF as the expression. This means you can transfer control within the ENTER command to another routine that can be used, not only for making sure you really want to enter this field at this time, but also to set up all appropriate traps and to display the correct information at the bottom of the screen. By using the post_enter_expression to undo what you have done with this option, you can be sure you will never have traps set that shouldn’t be or messages that don’t relate to what is really going on.

- **`post_enter_expression`** · `lexpr` · *Optional*

  Optional - If you want to do something after the entry, it can be done here. You can use this to reset traps and other housekeeping chores that need to be done before the user will be able to press another key, or you can also test against whatever criteria you desire. The result of this expression can be checked with the ENTER() function. The function will return whatever this expression returned (.T. or .F.).

- **`default_value`** · `f/c/e` · *Optional*

  Optional - If you want the field to default to a certain value upon entry the value can be set here. If the ENTER field is blank (in the case of an A type field), or 0 the default_value will be displayed as the ENTER field value. The user will be able to clear this data by pressing CTRL-U or by overwriting the value; by pressing the ENTER or RETURN key, the user can accept the value.

- **`valid_check`** · `lexpr` · *Optional*

  Optional - Through the use of this option you can create an expression that will check if the value the user entered was acceptable. The test can be any function that will return .T. or .F. If the expression returns .F., the program will look for a valid_message. If it exists it will be displayed; if not, a standard valid error message will be used. The program will then continue with the entry and will not allow the user to exit without the valid_check option returning .T. There is a case where this isn’t true and that’s when the user presses the ESC key. In this case the program will always exit the ENTER command, unless the ESC TRAP is set to ignore. A valid_check may be as simple as:
  NUM_UNITS>0
  Or it can be a UDF or a complex expression. For example:
  ROW()>3 .a. (TOTAL_SALES<1000 .o. MONTH(DATE())=3)
  NOTE: Do not do another ENTER as part of the valid_check.

- **`valid_message`** · `f/c/e` · *Optional*

  Optional - Use this option if the valid_check option is used and you want to send a special message if the expression returns .F. The message will be displayed in the standard error message location. You may include the valid_message as an alpha constant with no other requirements, if desired. For example:
  “You didn’t put in the proper value. Please try again.”

- **`ARRAY`** · `array` · *Optional*

  Optional - If this option is included in the command then the program will allow you to enter all of the elements in that array field through this one ENTER statement. The user can ‘page’ through the elements by pressing the Page Dn key for elements beyond the current value and Page Up for previous elements. Changes can be made and saved. If you use the array_counter value you can display the current element number as the user ‘pages’ through the different values. NOTE: Do NOT include an array specifier as part of the fieldname. If you wish to choose the array element directly use the array_counter option and put the appropriate element number in that field.

- **`array_counter`** · `fn/v` · *Optional*

  Optional - If you have specified that this is an array_field you can also specify an array_counter field. The program will pass the current element number to this field. If you put this field on the screen, everytime the user presses the PgDn or PgUp key the appropriate element number will be displayed. You can see this process at work in the Maintain Database program. It puts the field in the proper location within the field name so that it looks like an array specifier. You can also change the array_counter directly and control which element is displayed in cases where there are so many array elements that using the standard PgUp or PgDn key movement is too time-consuming.

- **`enumerated_field`** · `f/c/e1, f/c/e2,..., f/c/ex` · *Optional*

  Optional - You can specify an enumerated list of values that are the only acceptable values for this field. The user can page through the list by using the PgUp/PgDn keys or by pressing the space bar. The first value in the enumerated_field list will be displayed as the default field value. When the user presses the RETURN or ENTER key the value currently displayed will be stored in the ENTER field.

- **`do_udf`** · `udf` · *Optional*

  Optional - In this case you don’t want to actually enter the field but want to execute this UDF. In general, this would be to edit a large field using the TASEdittm program, or the MENU command, etc. However, you can use this for any purpose. If a value is returned from the UDF, it will be saved into the field. If no value is returned the program assumes that you made any changes desired directly into the appropriate field.
  The user can initiate this UDF by pressing the F2 key or Ctrl-Home. You should exit your routine with the standard RET command.

- **`up_arrow_goto_label`** · `label` · *Optional*

  Optional - Generally, if the user presses the UP ARROW key, the program will leave the current ENTER field and move towards the beginning of the program command line by command line until the next previous ENTER or UPAR (UP ARROW) command is found. If none is found the bell will sound and the current ENTER will be re-executed. If you want to send the program to a specific line label and not allow this ‘search’ then this option may be used.
  In general it is much wiser to use the UPAR command and the pre_enter_expression option to control the process by which the user moves around on the screen.

- **`group_num`** · `Windows Only - f/c/e` · *Optional*

  Optional - When a user selects a field on the screen by clicking on it with the mouse the first thing the field select process does is check the GROUP value for the current field. If there is no value set by the programmer the internal value is set to 0. The process will then allow the user to only select fields with the same GROUP value. By setting all fields to different GROUP values you can easily keep the user from selecting any fields with the mouse.

- **`UP`** · `Optional` · *Optional*

  Optional - If this option is included in the command the program will force to upper case all alphanumeric characters entered. Even if this option isn’t set in the ENTER command, it will apply if it is set in the field definition.

- **`ACR`** · `Optional` · *Optional*

  Optional - If this option is included in the command the ENTER routine will automatically exit upon filling the last character in the field. Normally, the user would have to press the ENTER or RETURN key to finish the ENTER command.

- **`PSWD`** · `Optional` · *Optional*

  Optional - If this option is included in the command the characters the user enters are not echoed to the screen. The program will display question marks (?) instead of the entered characters.

- **`NOREV`** · `Optional` · *Optional*

  Optional - Normally the program uses the reverse color value when displaying and entering fields. If this option is included in the command, the ENTER command will use the normal color value.

- **`AUTO_SRCH`** · `Optional` · *Optional*

  Optional - If this option is included in the command and the field is listed as an index in the Data Dictionary, then upon exiting the ENTER command the program will attempt to find a matching record. If a record is already active for this file, this option will be ignored.

- **`NOCLICKON`** · `Windows Only - Optional` · *Optional*

  Optional - If you set this option the user will not be able to select this field with the mouse.

- **`NOCLICKOFF`** · `Windows Only - Optional` · *Optional*

  Optional - If you set this option the user will not be able to select a different field with the mouse when they are currently on this field.

## Comments

In all situations if the user presses a control key or function key and a trap is set for that key this command will act as though s/he pressed the ENTER key first, and the entry will be saved before exiting the command. However, if the ESC key is pressed the data entered is discarded.

## Sample Program

`ENTTEST, MTEST`

## Program Editor

`User interface -> Enter`

## See Also

- [NOVLDMSG](NOVLDMSG.md)
- [ENTER()](../functions/ENTER.md)
