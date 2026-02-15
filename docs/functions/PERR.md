# PERR()

| | |
|---|---|
| **Category** | Function |
| **Name** | `PERR` |
| **Returns** | `I` |

## Purpose

This function returns the error number of the last program error.

*No parameters.*

## Return Type

I

## Comments

Once a program error occurs the appropriate error number will remain until it is cleared by the CLEAR
PRG_ERR command or the current program terminates. The error number returned will correspond to
the standard TAS Professional runtime error numbers. For more information please refer to Chapter
10, Runtime Errors.
NOTE: The value 999 is returned if the user has quit a subsequent program by pressing the Esc key.
