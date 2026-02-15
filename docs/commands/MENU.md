# MENU

| | |
|---|---|
| **Category** | Command |
| **Platform** | TAS Professional 5.1 |

## Description

Display a bar-type menu and prompt for a valid choice. The NMENU command is an alternative for accomplishing the same thing.

## Syntax

```text
MENU AT starting_column,starting_row LEN length WDT width CPC choices_per_column FLD menu_lines_field CNTR counter_field NCH maximum_number_of_choices MCW choice_print_width ESC esc_key_label HELP help_label TTL menu_title HOLD
```

## Parameters

- **`starting_column`** · `f/c/e` · *Required*

  Upper left corner of the menu, column value.

- **`starting_row`** · `f/c/e` · *Required*

  Upper left corner of the menu, row value.

- **`length`** · `f/c/e` · *Required*

  Number of rows long, including box around the menu. If there are two lines within the menu the length is 4.

- **`width`** · `f/c/e` · *Required*

  Number of columns wide, including box around the menu. If the choices are 10 columns wide within the menu the total width is 12.

- **`choices_per_column`** · `f/c/e` · *Required*

  If there are multiple columns in the menu this is the number of choices within a single column.

- **`menu_lines_field`** · `fn/v` · *Required*

  The array field that contains the menu lines. Each menu line is made up of 2 parts. The first is the displayed portion; this is what your user will see. The second is the control information. This consists of 5 characters. The first two characters indicate the starting column value for this line within the

- **`CNTR`** · `unknown` · *Required*

  counter_field

- **`NCH`** · `unknown` · *Required*

  maximum_number_of_choices

- **`MCW`** · `unknown` · *Required*

  choice_print_width

- **`ESC`** · `unknown` · *Required*

  esc_key_label

- **`HELP`** · `unknown` · *Required*

  help_label

- **`TTL`** · `unknown` · *Required*

  menu_title

- **`HOLD`** · `unknown` · *Required*

  

## Comments

Display a bar-type menu and prompt for a valid choice. The NMENU command is an alternative for accomplishing the same thing.
