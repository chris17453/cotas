# LISTM

| | |
|---|---|
| **Category** | Command |

## Description

(Description omitted in this excerpt)

## Syntax

```text
LISTM field_list ENTER enter_udf SRCH search_udf CHSE chse_expr HELP help_udf ACTV number_of_active_elements MAXA max_number_of_elements CNTR counter_field ON_MOVE move_udf CCOLOR choice_color STYP search_type FLINE first_line_fields CBF chrs_btwn_fields BLNES num_of_blank_lines ETXT_COLOR enter_text_color CTXT_COLOR choice_text_color RND NOWAIT MENU NOADD NOSHIFT USE_TRAPS INS_AT_END USE_COLORS
```

## Parameters

- **`field_list`** · `f/c/e` · *Required*

  The array fields, constants or expressions to be used in the list.

- **`ENTER`** · `udf` · *Optional*

  Optional - The program will execute the UDF each time the user presses the ENTER key.

- **`SRCH`** · `udf` · *Optional*

  Optional - Search UDF (first stage of filtering).

- **`search_udf`** · `udf` · *Optional*

  Optional - The search UDF used with SRCH.

- **`CHSE`** · `udf` · *Optional*

  Optional - Chse expression for additional filtering.

- **`chse_expr`** · `expression` · *Optional*

  Chse expression content.

- **`HELP`** · `udf` · *Optional*

  Optional - Help UDF.

- **`help_udf`** · `udf` · *Optional*

  Help UDF.

- **`ACTV`** · `n` · *Optional*

  Optional - number of active elements.

- **`number_of_active_elements`** · `n` · *Optional*

  Number of active elements.

- **`MAXA`** · `n` · *Optional*

  Optional - Maximum active elements.

- **`max_number_of_elements`** · `n` · *Optional*

  Maximum number of elements.

- **`CNTR`** · `fn/v` · *Optional*

  Optional - Counter field.

- **`counter_field`** · `fn/v` · *Optional*

  Counter field.

- **`ON_MOVE`** · `udf` · *Optional*

  Optional - UDF for ON_MOVE.

- **`move_udf`** · `udf` · *Optional*

  Move UDF.

- **`CCOLOR`** · `color` · *Optional*

  Optional - Color selection.

- **`choice_color`** · `color` · *Optional*

  Choice color.

- **`STYP`** · `n` · *Optional*

  Optional - Style type.

- **`search_type`** · `n` · *Optional*

  Search type.

- **`FLINE`** · `f` · *Optional*

  First line fields.

- **`first_line_fields`** · `f` · *Optional*

  First line fields.

- **`CBF`** · `n` · *Optional*

  Chrs between fields.

- **`chrs_btwn_fields`** · `n` · *Optional*

  Characters between fields.

- **`BLNES`** · `n` · *Optional*

  Number of blank lines before/after.

- **`num_of_blank_lines`** · `n` · *Optional*

  Number of blank lines.

- **`ETXT_COLOR`** · `color` · *Optional*

  Enter text color.

- **`enter_text_color`** · `color` · *Optional*

  Enter text color.

- **`CTXT_COLOR`** · `color` · *Optional*

  Choice text color.

- **`choice_text_color`** · `color` · *Optional*

  Choice text color.

- **`RND`** · `boolean` · *Optional*

  Optional - Randomization flag.

- **`NOWAIT`** · `boolean` · *Optional*

  Optional - Do not wait for user input.

- **`MENU`** · `udf` · *Optional*

  Menu UDF.

- **`NOADD`** · `boolean` · *Optional*

  Optional - Do not add.

- **`NOSHIFT`** · `boolean` · *Optional*

  Optional - No shift.

- **`USE_TRAPS`** · `boolean` · *Optional*

  Optional - Use traps.

- **`INS_AT_END`** · `boolean` · *Optional*

  Optional - Insert at end.

- **`USE_COLORS`** · `boolean` · *Optional*

  Optional - Use colors.

- **`USE_COLORS`** · `boolean` · *Optional*

  Optional - Use colors.

- **`INS_AT_END`** · `boolean` · *Optional*

  Optional - Insert at end.
