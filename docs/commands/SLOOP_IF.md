# SLOOP_IF

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether to loop back to the beginning in a SCAN/ENDS loop. This is one part of a complete command structure.

## Syntax

```text
SLOOP_IF expression
```

## Parameters

- **`expression`** · `lexpr` · *Required*

  expression - lexpr - Required - If the expression resolves to .T. then the program will transfer control to the appropriate SCAN line. This would be equivalent to executing the ENDS (Endscan) command.

## Comments

For more information on the different structured programming commands, and the SLOOP_IF command in particular, please see Chapter 7, Structured Programming Commands.

## Program Editor

`fiLe -> Mult rec cmds -> Scan -> loop_If`

## See Also

- [SCAN](SCAN.md)
- [SEXIT](SEXIT.md)
- [SEXIT_IF](SEXIT_IF.md)
- [SLOOP](SLOOP.md)
- [ENDS](ENDS.md)
