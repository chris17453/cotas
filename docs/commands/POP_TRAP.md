# POP TRAP

| | |
|---|---|
| **Category** | Command |

## Description

Restore a value in a trap or group of traps that was saved previously using the PUSHT (Push Trap) command.

## Syntax

```text
POPT trap_list
```

## Parameters

- **`trap_list`** · `trap_list` · *Required*

  trap1,trap2,...,trapx - Required - The list of traps to restore.

## Comments

When you use the PUSHT command the current trap settings are saved on an internal stack. This stack, just like all others used in microcomputers, is a Last In- First Out process. That means that the last value
