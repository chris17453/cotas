# MOUSE_ACT()

| | |
|---|---|
| **Category** | Function |
| **Name** | `MOUSE_ACT` |
| **Platform** | DOS only |
| **Returns** | `L` |

## Purpose

This function returns information about mouse actions. Once you test for a specific action that flag is cleared until it happens again.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Mouse action to check. 0 - movement 1 - Left button down 2 - Left button up 3 - Right button down 4 - Right button up |

## Return Type

L If the action you are testing for has happened since the last time you checked the function will return
.T. Once you check it clears that flag, so if you checked immediately again and no action had occurred
since your last check the function would return .F.

## Comments

There are many situations where use of the mouse traps won't be effective and you need a different
method for determining actions. You would use this function in those cases. A good example of this
use is in the programming utilities for TAS Professional 5.1, i.e, TASEDPRG.SRC (editor),
TASEDSCR.SRC (screen painter), TASEDRPT.SRC (report writer) and TASRPT.SRC (report/2
writer).
