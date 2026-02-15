# PRINT_CANCEL()

| | |
|---|---|
| **Category** | Function |
| **Name** | `PRINT_CANCEL` |
| **Platform** | Windows Only |
| **Returns** | `L` |

## Purpose

When the user is given the choices of where to print under Windows one of those choices is Cancel. If they choose to cancel the output then this function will return .T. By itself, this function will NOT cancel the report. It is up to the programmer to add the 1 or 2 lines of code necessary to actually exit the report. This will just notify the programmer of the user's choice.

*No parameters.*

## Return Type

L If the user chooses Cancel in the Choose Output options this function will return .T.
