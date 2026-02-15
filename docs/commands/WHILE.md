# WHILE

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether a command, or group of commands will be executed. This is one part of a complete command structure.

## Syntax

```text
WHILE expression
```

## Parameters

- **`expression`** · `lexpr` · *Required*

  expression - lexpr - Required - If the expression resolves to .T., the command lines between the WHILE command and the next LOOP_IF, EXIT, EXIT_IF or ENDW (Endwhile) command will be executed.

## Comments

For more information on the different structured programming commands, and the WHILE command in particular, please see Chapter 7, Structured Programming Commands.

## See Also

- [EXIT](EXIT.md)
- [EXIT_IF](EXIT_IF.md)
- [LOOP](LOOP.md)
- [LOOP_IF](LOOP_IF.md)
- [ENDW](ENDW.md)
