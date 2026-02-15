# CASE

| | |
|---|---|
| **Category** | Command |

## Description

This is a process control command, i.e., it will control whether a command, or group of commands will be executed. This is one part of a complete command structure.

## Syntax

```text
CASE option_number
```

## Parameters

- **`option_number`** · `c` · *Required*

  option_number - c - Required - Must be an integer value. If the value corresponds to the result in the SELECT command, the command lines between the CASE command and the next CASE, OTHERWISE or ENDC (Endcase) command will be executed.

## Comments

For more information on the different structured programming commands, and the CASE command in particular, please see Chapter 7, Structured Programming Commands.\nIf you are missing an ENDC (End Case) command in a SELECT/CASE structure you will get the If without Endif error message during the compile process.

## Program Editor

`prg Control -> Case -> Case`

## See Also

- [SELECT](SELECT.md)
- [OTHERWISE](OTHERWISE.md)
- [ENDC](ENDC.md)
