# WIN_LASER_PRT() (Windows Only)

| | |
|---|---|
| **Category** | Function |
| **Name** | `WIN_LASER_PRT` |
| **Platform** | Windows |
| **Returns** | `L` |

## Purpose

In some programs it is important for the report to know whether it is being printed on a laser printer or not. Since laser printers will only print 60 lines normally you may need to tighten up the output. This function will return the value set for this printer in the TP5WIN.INI file.

*No parameters.*

## Return Type

L If the current (active) printer is set as type laser this will return .T.

## Comments

For more information about printing in Windows please refer to Chapter 11 - Windows Programming.
