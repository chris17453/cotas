# WINDOW OPEN

| | |
|---|---|
| **Category** | Command |

## Description

This command is used for ‘opening’ a window on the screen. From the program's perspective, once this command is executed the screen becomes the window. Thus, the screen is the same size as the window, and the upper left hand corner of the window is column 1, row 1 for commands that display data to the screen. This will apply until a previously saved screen is redisplayed, or the SCRN R (Screen Reset) command is run.
WINDOW AT starting_column,starting_row LEN length WDT width
WCOLOR window_color TTLW title_where TTL title BOX box
BCOLOR box_color SHD shadow_where SCOLOR shadow_color
WTXT_COLOR window_text_color BTXT_COLOR border_text_color USE_COLORS
starting_column - f/c/e - Required - Upper left corner of the window, column value.
starting_row - f/c/e - Required - Upper left corner of the window, row value.

## Syntax

```text
WINDOW OPEN
This command is used for ‘opening’ a window on the screen. From the program's perspective, once this
command is executed the screen becomes the window. Thus, the screen is the same size as the window,
and the upper left hand corner of the window is column 1, row 1 for commands that display data to the
screen. This will apply until a previously saved screen is redisplayed, or the SCRN R (Screen Reset)
command is run.
WINDOW AT starting_column,starting_row LEN length WDT width
WCOLOR window_color TTLW title_where TTL title BOX box
BCOLOR box_color SHD shadow_where SCOLOR shadow_color
WTXT_COLOR window_text_color BTXT_COLOR border_text_color USE_COLORS
starting_column - f/c/e - Required - Upper left corner of the window, column value.
starting_row - f/c/e - Required - Upper left corner of the window, row value.
```

## Parameters

- **`starting_column`** · `f/c/e` · *Required*

  Upper left corner of the window, column value.

- **`starting_row`** · `f/c/e` · *Required*

  Upper left corner of the window, row value.

- **`LEN`** · `length` · *Required*

  length

- **`WDT`** · `width` · *Required*

  width

- **`WCOLOR`** · `f/c/e` · *Optional*

  window_color

- **`TTLW`** · `f/c/e` · *Optional*

  title_where

- **`TTL`** · `f/c/e` · *Optional*

  title

- **`BOX`** · `f/c/e` · *Optional*

  box

- **`BCOLOR`** · `f/c/e` · *Optional*

  box_color

- **`SHD`** · `f/c/e` · *Optional*

  shadow_where

- **`SCOLOR`** · `f/c/e` · *Optional*

  shadow_color

- **`WTXT_COLOR`** · `f/c/e` · *Optional*

  window_text_color

- **`BTXT_COLOR`** · `f/c/e` · *Optional*

  border_text_color

- **`USE_COLORS`** · `f/c/e` · *Optional*

  USE_COLORS

## See Also

- [WINACT](WINACT.md)
