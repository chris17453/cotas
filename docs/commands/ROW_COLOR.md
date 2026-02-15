# ROW COLOR

| | |
|---|---|
| **Category** | Command |
| **Platform** | Windows only |

## Description

Use this command to change the color of a single row on the screen.

## Syntax

```text
ROW_COLOR FROM from_row THRU thru_row COLOR background_color
TEXT_COLOR text_color
```

## Parameters

- **`from_row`** · `f/c/e` · *Required*

  from_row - f/c/e - Required - The starting row number. The first row in a window is number 1. This value will always be related to the current active window.

- **`thru_row`** · `f/c/e` · *Required*

  thru_row - f/c/e - Required - The ending row number. If you only want to change the color in a single row then this should be the same value as the FROM value.

- **`background_color`** · `n` · *Optional*

  COLOR background_color

- **`text_color`** · `n` · *Optional*

  TEXT_COLOR text_color
