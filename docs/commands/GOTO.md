# GOTO

| | |
|---|---|
| **Category** | Command |
| **Platform** | TAS Professional 5.1 |

## Description

This command will ‘permanently’ transfer control to a different part of the program. The first line of
that routine must be a standard line label.

## Syntax

```text
GOTO goto_label
```

## Parameters

- **`goto_label`** · `label` · *Required*

  goto_label - label - Required - Transfer control to the line label.

## Comments

This command, as opposed to the GOSUB, has no automatic way to return. You must use another
GOTO to return to the next command, if desired. Generally, this command is used only in situations
where you do not want to execute the lines following this command.
This command is not part of the regular ‘structured’ commands that a programmer would use in creating
a program. Much effort has been made in TAS Professional 5.1 so that this command need not be used
at all. However, it remains here for those times you find it the expedient choice.

## Program Editor

`Prg control -> Goto/gosub -> Goto`

## See Also

- [GETLBL](GETLBL.md)
