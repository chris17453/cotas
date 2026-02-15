# OPEN()

| | |
|---|---|
| **Category** | Function |
| **Name** | `OPEN` |
| **Returns** | `I` |

## Purpose

This function will return the appropriate TAS Professional 5.1 error message number for the last OPEN (or OPENV) command.

*No parameters.*

## Return Type

I

## Comments

If the file was opened properly this function will return 0. If the file was a Btrieve type file the function
FLERR() will return the Btrieve error number.
