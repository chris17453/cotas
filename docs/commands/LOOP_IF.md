# LOOP_IF

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether to loop back to the beginning in a WHILE/ENDW loop. This is one part of a complete command structure.

## Syntax

```text
LOOP_IF expression
```

## Parameters

- **`expression`** · `lexpr` · *Required*

  If the expression resolves to .T. then the program will transfer control to the appropriate WHILE line. This would be the equivalent of executing the ENDW command.

## Comments

For more information on the different structured programming commands, and the LOOP_IF command in particular, please see Chapter 7, Structured Programming Commands.

## Program Editor

`Prg control -> While -> loop_If`

## See Also

- [WHILE](WHILE.md)
- [LOOP](LOOP.md)
- [EXIT](EXIT.md)
- [EXIT_IF](EXIT_IF.md)
- [ENDW](ENDW.md)
