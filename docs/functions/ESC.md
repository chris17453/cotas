# ESC()

| | |
|---|---|
| **Category** | Function |
| **Name** | `ESC` |
| **Returns** | `L` |

## Purpose

If the user presses the ESC key at a message and the appropriate ESC traps are set to ignore then the internal esc_pressed flag is set to 1. You can test for that flag being set using this function.

*No parameters.*

## Return Type

L If the user pressed the ESC key this will be .T..

## Comments

Once this flag is checked it will be reset to .F.. It is also cleared before the next MESSAGE or ENTER command.
