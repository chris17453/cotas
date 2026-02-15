# IF

| | |
|---|---|
| **Category** | Command |
| **Platform** | TAS Professional 5.1 |

## Description

IF This is a process control command, i.e., it will control whether a command, or group of commands, will be executed. This is one part of a complete command structure.

## Syntax

```text
IF if_expression what_to_do goto_gosub_label
```

## Parameters

- **`if_expression`** · `lexpr` · *Required*

  - lexpr - Required - If the if_expression resolves to .T., the what_to_do action will be taken. If it is .F. and the what_to_do action is not THEN, the program will look for the next ELSE_IF or ELSE command. If one is not found the program will transfer control to the line following the appropriate ENDIF command.

- **`what_to_do`** · `enum` · *Required*

  - Required - The action to take if the expression resolves to .T. Possible actions include:
  DO - Do the following commands until the program reaches an ELSE, ELSE_IF or ENDIF command. This is the default value and doesn’t have to be specified.
  THEN - Execute the following command. The command to execute in this case must follow on the same physical line.
  GOTO - Goto the line label specified in the command.
  GOSUB - Gosub the line label specified in the command.
  REENT - Reenter the last ENTER command.
  RET - Return.

- **`goto_gosub_label`** · `label` · *Required*

  - label - Required - If the what_to_do option is GOTO or GOSUB then you must specify the label name here.

## Comments

For more information on the different structured programming commands, and the IF command in particular, please see Chapter 7, Structured Programming Commands.

## Program Editor

`Prg control -> If -> If`

## See Also

- [ELSE](ELSE.md)
- [ELSE_IF](ELSE_IF.md)
- [ENDIF](ENDIF.md)
