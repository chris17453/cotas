# CLEAR SCREEN FIELDS

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `CLRSF` |

## Description

This command will remove the active screen fields added either through the MOUNT or SAY command.

## Syntax

```text
CLRSF
```

## Comments

Each time a record is found or the screen is refreshed the program redisplays all fields (if any) in the screen field buffer. If you want to eliminate this redisplay, then you must use this command. Once the
