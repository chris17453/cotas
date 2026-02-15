# CLEAR PROGRAM ERROR

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `CLRPE` |

## Description

This command will clear the system program error number holder. This is set anytime the program encounters an error while executing a program or if you execute the ERR (Error) command.

## Syntax

```text
CLRPE
```

## Comments

You would use this command when making calls to a routine that may or may not fail. This is the only way to clear the error number before calling the routine again. Also, the program error holder is not reset when the program returns to the calling (previous) program. This allows you to check if an error occurred in the chained program. Use the ERR() function to check for an error value.

## Program Editor

`User interface -> Messages -> Clear prg error`

## See Also

- [ERR](ERR.md)
- [ERR()](../functions/ERR.md)
