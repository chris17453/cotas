# POP STACK

| | |
|---|---|
| **Category** | Command |

## Description

Remove the value pushed on the internal stack by a previous GOSUB command.

## Syntax

```text
POP STACK
```

## Comments

This command needs to be used with great caution. However, it will allow you to do something other than just returning from a GOSUB if necessary.
You may use this command only in connection with a GOSUB or PUSHx command. Each POPS removes 2 bytes from the internal stack. You may NOT use this command in connection with a UDF or UDC.

## Program Editor

`prg Control -> Goto/gosub -> Pop stack`

## See Also

- [GOSUB](GOSUB.md)
