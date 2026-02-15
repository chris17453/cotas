# EXIT_IF

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether to exit from a WHILE loop. This is one part of a complete command structure.

## Syntax

```text
EXIT_IF expression
```

## Parameters

- **`expression`** · `lexpr` · *Required*

  expression - lexpr - Required - If the expression resolves to .T. then the program will transfer control to the line following the appropriate ENDW (Endwhile) command.
