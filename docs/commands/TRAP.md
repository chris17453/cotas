# TRAP

| | |
|---|---|
| **Category** | Command |

## Description

This command is used to set internal traps so that if a certain key is pressed the program will take an appropriate action.

## Syntax

```text
TRAP trap_name_list GOTO/GOSUB/IGNR/DFLT label
```

## Parameters

- **`trap_name_list`** · `sac` · *Required*

  The name of the trap. These are listed below. You may include multiple trap names for a single trap command. Each of the traps will be set to the same values. This is generally used when you are setting a group of traps to IGNR (ignore) or DFLT (default).
