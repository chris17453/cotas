# WINDOW_PTR() (Windows Only)

| | |
|---|---|
| **Category** | Function |
| **Name** | `WINDOW_PTR` |
| **Platform** | Windows |
| **Returns** | `R` |

## Purpose

This function will return the handle or pointer to the currently active window.

*No parameters.*

## Return Type

R

## Comments

The value returned by this function can be used in conjuction with the WINACT command. By getting the window handle you can use the WINACT command even though you didn't set the window up with the WINDEF command. If you are manipulating several windows on the screen at the same time you may find this more convenient. For more information about windows in Windows please refer to Chapter 11 - Windows Programming.
