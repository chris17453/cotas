# REDISPLAY SCREEN

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `REDSP` |
| **Platform** | TAS Professional 5.1 |

## Description

This command will redisplay a screen that was previously saved using the SAVES (Save Screen) command.

## Syntax

```text
REDSP screen_holder_name
```

## Parameters

- **`screen_holder_name`** · `fn/v` · *Optional*

  The name of the field that was used in the SAVES (Save Screen) command. If you did not specify a field name in that command (i.e., you saved the screen to an internal buffer), then you should not specify one here.
  NOTE: If you do specify a field name it must be large enough to hold the entire screen and certain information about the current screen environment. This is approximately 4300 bytes.
