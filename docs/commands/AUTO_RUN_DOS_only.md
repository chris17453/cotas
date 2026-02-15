# AUTO RUN (DOS only)

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS |

## Description

This command can be used to record characters from the keyboard or play back those recorded earlier.

## Syntax

```text
AUTO_RUN ARN field DELAY f/c/e RECORD
```

## Parameters

- **`ARN field`** · `fn/v` · *Required*

  The field that contains the characters to be used as input. This field should contain the values in the .ARN file created either by the RECORD option or with the Convert .CHR to .ARN program (see Chapter 3, Main Menu - Convert .CHR to .ARN)

- **`DELAY`** · `f/c/e` · *Optional*

  If you are playing back characters this is the number of 64k loops you want the system to do between each character. The higher the number the slower the program will run. You would set this value only if you wanted the user to be able to watch the progress of the play back. Otherwise, do not set this value and the program will run as fast as possible.

- **`RECORD`** · `f/c/e` · *Optional*

  If you are recording characters entered from the keyboard then you would set this value.
