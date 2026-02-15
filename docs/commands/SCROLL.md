# SCROLL

| | |
|---|---|
| **Category** | Command |

## Description

This command will move a set of characters on the screen as a block.

## Syntax

```text
SCROLL AT starting_column,starting_row
LEN number_of_rows
WDT number_of_columns NUM how_many_to_move
UP/DN/LEFT/RIGHT
```

## Parameters

- **`starting_column`** · `f/c/e` · *Required*

  The upper left column value. Together with the starting_row value defines the upper left corner of the block. The upper left corner of any window is 1,1.

- **`starting_row`** · `f/c/e` · *Required*

  The upper left row value. Together with the starting_column value defines the upper left corner of the block. The upper left corner of any window is 1,1.

- **`number_of_rows`** · `f/c/e` · *Optional*

  The number of rows in the block. If this is not included the block will be 1 row long.

- **`number_of_columns`** · `f/c/e` · *Optional*

  The number of columns in the block. If this is not included the block will be 1 column wide.

- **`how_many_to_move`** · `f/c/e` · *Optional*

  How many rows/columns to move the block of characters. If this is not included the program will clear the entire block.

- **`UP/DN/LEFT/RIGHT`** · `f/c/e` · *Required*

  Which direction to move the block of characters.

## Comments

This command will not move the background colors, only the characters displayed on the screen.

## Program Editor

`User interface -> Screen control -> Scroll`
