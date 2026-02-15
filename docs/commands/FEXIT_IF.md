# FEXIT_IF

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether to exit from a FOR/NEXT loop. This is one part of a complete command structure.

## Syntax

```text
FEXIT_IF expression
```

## Parameters

- **`expression`** · `e` · *Required*

  lexpr - Required - If the expression resolves to .T., then the program will transfer control to the line following the appropriate NEXT command. The counter value will remain at the last value.

## Comments

For more information on the different structured programming commands, and the FEXIT_IF command in particular, please see Chapter 7, Structured Programming Commands.

## See Also

- [FOR](FOR.md)
- [FLOOP](FLOOP.md)
- [FLOOP_IF](FLOOP_IF.md)
- [FEXIT](FEXIT.md)
- [NEXT](NEXT.md)
