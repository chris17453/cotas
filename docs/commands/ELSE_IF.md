# ELSE_IF

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether a command, or group of commands will be executed. This is one part of a complete command structure.

## Syntax

```text
ELSE_IF else_if_expression
```

## Parameters

- **`else_if_expression`** · `lexpr` · *Required*

  else_if_expression - lexpr - Required - If the else_if_expression resolves to .T., the command lines between the ELSE_IF command and the next ELSE_IF, ELSE or ENDIF command will be executed.

## Comments

For more information on the different structured programming commands, and the ELSE_IF command
in particular, please see Chapter 7, Structured Programming Commands.

## Program Editor

`Prg control -> If -> eLse_if`

## See Also

- [IF](IF.md)
- [ELSE](ELSE.md)
- [ENDIF](ENDIF.md)
