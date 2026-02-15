# JUSTIFY FIELD

| | |
|---|---|
| **Category** | Command |

## Description

LEFT/RIGHT/CENTER - Optional - Move the characters to the LEFT in the field, to the RIGHT, or CENTER them.

## Syntax

```text
JUSTIFY FIELD LEFT/RIGHT/CENTER
```

## Parameters

- **`alignment`** · `sac` · *Optional*

  Specifies how the field's contents are aligned. Allowed values are LEFT, RIGHT, and CENTER. Optional.

## Comments

When you use the function JUST() the program puts the field in the temporary data space. If you are trying to JUST a field that is larger than that space you will get the message that the program is out of room in the temporary data area. This command is provided to you so that you can JUST a field larger than the temporary data area within its own space. Using this command you will not run out of memory.

## Program Editor

`Field -> alpha Fld cmds -> Justify`

## See Also

- [TRIM()](../functions/TRIM.md)
