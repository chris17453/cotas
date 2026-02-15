# LIST FILE

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `LISTF` |

## Description

This command will generate a list of records directly from a file. The user can scroll through the list using the UP and DOWN ARROW keys, PgUp and PgDn, or enter a single character, and depending on your options, find a value in the list. You can also display lines wider than the window; the user shifts the display right or left using the RIGHT or LEFT ARROW keys.
All of the lookup windows in the TAS Professional 5.1 utilities use either this command or the LISTM (List Array) command.

## Syntax

```text
LISTF field_list ENTER enter_udf SRCH search_udf CHSE chse_expr HELP help_udf
OTH other_udf FILE filename/@file_number KEY keyname/@key_number
START start_value FOR for_filter_expression WHILE while_filter_expression
ON_MOVE move_udf ECOLOR enter_color CCOLOR choice_color STYP search_type
FLINE first_line_fields CBF chrs_btwn_fields BLNES num_of_blank_lines
ETXT_COLOR enter_text_color CTXT_COLOR choice_text_color RND NOWAIT MENU
NOADD UP NOSHIFT END USE_TRAPS INS_AT_END USE_COLORS
```

## Parameters

- **`field_list`** · `f/c/e` · *Required*

  The file fields, constants or expressions to be used in the list. The fields can be from other files (generally related).
  NOTE: You can separate the fields on the list by putting spaces between the fields.
  For example:
  field1,’ ‘,field2,’ ‘,field3

- **`enter_udf`** · `udf` · *Optional*

  If you specify this option the program will execute the UDF each time the user presses the ENTER or RETURN key (Change or Add a record), the DEL key (Delete a record) or INS (Insert a record). The type of entry is passed to the routine through the ETYP() function. The record is active in the normal record buffer and can be accessed using the standard field names.

- **`SRCH`** · `udf` · *Optional*

  If this is specified the program will execute this UDF if the user presses the F2 key. If you want to reset the display to a specific record then leave that record active and return a .T. value; return .F. if you want to leave everything as it is.

- **`CHSE`** · `expression` · *Optional*

  Using this option you can specify the field to be used as the choice field for this command. The program will use the first character of the field

- **`HELP`** · `udf` · *Optional*

  help_udf

- **`OTH`** · `udf` · *Optional*

  OTH other_udf

- **`FILE`** · `filename/@file_number` · *Optional*

  FILE filename/@file_number

- **`KEY`** · `keyname/@key_number` · *Optional*

  KEY keyname/@key_number

- **`START`** · `start_value` · *Optional*

  START start_value

- **`FOR`** · `for_filter_expression` · *Optional*

  FOR for_filter_expression

- **`WHILE`** · `while_filter_expression` · *Optional*

  WHILE while_filter_expression

- **`ON_MOVE`** · `move_udf` · *Optional*

  ON_MOVE move_udf

- **`ECOLOR`** · `enter_color` · *Optional*

  ECOLOR enter_color

- **`CCOLOR`** · `choice_color` · *Optional*

  CCOLOR choice_color

- **`STYP`** · `search_type` · *Optional*

  STYP search_type

- **`FLINE`** · `first_line_fields` · *Optional*

  FLINE first_line_fields

- **`CBF`** · `chrs_btwn_fields` · *Optional*

  CBF chrs_btwn_fields

- **`BLNES`** · `num_of_blank_lines` · *Optional*

  BLNES num_of_blank_lines

- **`ETXT_COLOR`** · `enter_text_color` · *Optional*

  ETXT_COLOR enter_text_color

- **`CTXT_COLOR`** · `choice_text_color` · *Optional*

  CTXT_COLOR choice_text_color

- **`RND`** · `RND` · *Optional*

  RND

- **`NOWAIT`** · `NOWAIT` · *Optional*

  NOWAIT

- **`MENU`** · `MENU` · *Optional*

  MENU

- **`NOADD`** · `NOADD` · *Optional*

  NOADD

- **`UP`** · `UP` · *Optional*

  UP

- **`NOSHIFT`** · `NOSHIFT` · *Optional*

  NOSHIFT

- **`END`** · `END` · *Optional*

  END

- **`USE_TRAPS`** · `USE_TRAPS` · *Optional*

  USE_TRAPS

- **`INS_AT_END`** · `INS_AT_END` · *Optional*

  INS_AT_END

- **`USE_COLORS`** · `USE_COLORS` · *Optional*

  USE_COLORS
