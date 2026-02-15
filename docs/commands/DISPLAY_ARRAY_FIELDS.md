# DISPLAY ARRAY FIELDS

| | |
|---|---|
| **Category** | Command |

## Description

By incrementing or decrementing the element number you can make the information appear to scroll down or up the screen.

## Syntax

```text
DISPLAY ARRAY FIELDS
```

## Parameters

- **`starting_character_position`** · `f/c/e` · *Optional*

  This is the position number of the first character to be displayed. The first character in the array element is at position 1. Even though this is an optional field, you increment or decrement this value to move right or left across the screen. The default value is the first character in the field, or 1.

- **`starting_column`** · `f/c/e` · *Optional*

  The number of the column at which to start the display. The first column on the screen (far left position) is number 1. The default value, if none is provided, is the current column value as set by the last display or enter to the screen. This value can be easily determined through the use of the COL() function.

- **`starting_row`** · `f/c/e` · *Optional*

  The number of the row at which to start the display. The first row on the screen (top) is number 1. The default value, if none is provided, is the current row value as set by the last display or enter to the screen. This value can be easily determined through the use of the ROW() function.

- **`number_of_characters_to_display`** · `f/c/e` · *Optional*

  The number of characters to display on a single line, from a single array element. If this is not included the program will use the display size of the field or the width of the current screen window, whichever is less.

- **`number_of_lines_to_display`** · `f/c/e` · *Optional*

  The number of lines (array elements) to display at one time. If this is not included the program will use the maximum number of array elements in the field or the number of rows in the current screen window, whichever is less.

- **`color_values`** · `NY` · *Optional*

  If Y then this array is actually color values and should be displayed appropriately.

## Comments

The array being displayed must be of type A. By manipulating the array element value within the array field spec and the starting_character_position value, you can give the impression of a window being moved across a much larger data block.

## Sample Program

`DISPARY`

## Program Editor

`Field -> Array -> Disp Array`

## See Also

- [RDA](RDA.md)
