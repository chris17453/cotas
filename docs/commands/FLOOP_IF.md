# FLOOP_IF

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether to loop back to the beginning in a FOR/NEXT loop. This is one part of a complete command structure.

## Syntax

```text
FLOOP_IF expression
```

## Parameters

- **`expression`** · `lexpr` · *Required*

  expression - lexpr - Required - If the expression resolves to .T., then the program will transfer control to the appropriate FOR line. This would be the equivalent of executing the NEXT command.

## Comments

For more information on the different structured programming commands, and the FLOOP_IF command in particular, please see Chapter 7, Structured Programming Commands.

## Program Editor

`Prg control -> For -> loop_If`

## See Also

- [FOR](FOR.md)
- [FLOOP](FLOOP.md)
- [FLOOP_IF](FLOOP_IF.md)
- [FEXIT](FEXIT.md)
- [NEXT](NEXT.md)
