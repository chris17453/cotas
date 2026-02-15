# AUTO DECREMENT LIST

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `AUTODEC` |

## Description

This is used in connection with the LISTM (List Array) and LISTF (List File) commands. If you are executing one of those commands and are in a routine ‘outside’ of the command (e.g., an enter process), you can use this command to automatically force the cursor to go back to the previous line in the list when you return to it.

## Syntax

```text
AUTODEC No options
```

## Program Editor

`User interface -> Lists -> autoDec`

## See Also

- [AUTOINC](AUTOINC.md)
- [LISTF](LISTF.md)
- [LISTM](LISTM.md)
- [AUTOENTER](AUTOENTER.md)
