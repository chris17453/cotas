# SCRCHR

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS |

## Description

Use this command to get or put a single display character or attribute on the screen.

## Syntax

```text
SCRCHR AT column,row DISP display_field ATRB attribute_field GET/SET
```

## Parameters

- **`column`** · `f/c/e` · *Required*

  The column position on the screen for the character. The upper left corner of the active window (or the full screen) is column 1, row 1.

- **`row`** · `f/c/e` · *Required*

  The row position on the screen for the character. The upper left corner of the active window (or the full screen) is column 1, row 1.

- **`display_field`** · `fn/v` · *Required*

  The name of the field that either holds the character to be displayed (set) or will receive the character currently on the screen (get). This must be a 1 character alpha (type A) field.

- **`attribute_field`** · `fn/v` · *Required*

  The name of the field that either holds the color attribute to be displayed (set) or will receive the color attribute currently on the screen (get). This must be a 1 character alpha (type A) field.
