# IF DUPLICATE RECORD

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `IFDUP` |
| **Platform** | TAS Professional 5.1 |

## Description

This is a process control command, i.e., it will control whether a command, or group of commands, will be executed. This is one part of a complete command structure.

## Syntax

```text
IFDUP keyname what_to_do goto_gosub_label
```

## Parameters

- **`keyname`** · `sac` · *Required*

  - sac - Required - The program checks the file to see if a record exists with the value in the key field(s). If it does, the what_to_do action will be taken. If it does not and the what_to_do action is not THEN, the program will look for the next ELSE_IF or ELSE command. If one is not found, the program will transfer control to the line following the appropriate ENDIF command.

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
