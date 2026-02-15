# SEXIT_IF

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether to exit from a SCAN loop. This is one part of a complete command structure.

## Syntax

```text
SEXIT_IF expression
```

## Parameters

- **`expression`** · `lexpr` · *Required*

  If the expression resolves to .T. then the program will transfer control to the line immediately following the appropriate ENDS (Endscan) command.

## Comments

For more information on the different structured programming commands, and the SEXIT_IF command in particular, please see Chapter 7, Structured Programming Commands.

## Program Editor

`fiLe -> Mult rec cmds -> Scan -> exiT_if`

## See Also

- [SCAN](SCAN.md)
- [SEXIT](SEXIT.md)
- [SLOOP](SLOOP.md)
- [SLOOP_IF](SLOOP_IF.md)
- [ENDS](ENDS.md)
