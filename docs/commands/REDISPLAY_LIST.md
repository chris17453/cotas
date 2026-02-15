# REDISPLAY LIST

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `RDLIST` |
| **Platform** | TAS Professional 5.1 |

## Description

Use this command when you are in a subroutine called from the LISTM (List Array) or LISTF (List File) command and you want to redisplay all the current lines when you return.

## Syntax

```text
RDLIST
```

## Comments

You can see this process at work in the Maintain Database, Import and Export utilities. If you press the F3 key to choose all the fields in a record the program calls a UDF to do the work and then executes this command so that the ‘stars’ will appear when the process is finished.

## Program Editor

`User interface -> Lists -> Redisplay list`

## See Also

- [LISTF](LISTF.md)
- [LISTM](LISTM.md)
