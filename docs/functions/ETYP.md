# ETYP()

| | |
|---|---|
| **Category** | Function |
| **Name** | `ETYP` |
| **Returns** | `A` |

## Purpose

If the program is executing a LIST FILE or LIST ARRAY command, and an ENTER UDF option is included, and the user presses the ENTER, RETURN, DELETE or INSERT key, this function will return the type of enter.

*No parameters.*

## Return Type

A A single character is returned.

## Comments

The possible returns are:
C - The cursor is on an active (existing) line. So this is a Change type.
I - The user has pressed the Insert key.
D - The cursor is on an active (existing) line, and the user has pressed the Delete key.
A - The cursor is on the line after the last active line and the user has pressed the Enter or Insert key. This is an Add enter instead of an Insert since the new entry will be added to the end of the list.
