# MOUSE ON/OFF

## Summary
ON/OFF - Required - If this option is ON the program will display the mouse on the screen
and allow the user to take advantage of the features relating to the mouse. To return to
normal mode the option would be OFF.

## Signature
```
COMMENTS
Other commands within TAS Professional 5.1 will take advantage of the mouse if it has been turned on.
With some commands this is more or less automatic; with others you have to set up some supporting
mouse commands. The various traps that work with the mouse (e.g., for detecting movement) are
covered under the TRAP command. Current mouse location is available using two functions and there
is also a function for determining what the mouse is doing. See the documentation in Chapter 6,
Function Reference for more information on MCOL(), MROW() and MOUSE_ACT().
To turn the mouse on automatically you can set the Mouse On value to Y in the Set Configuration
program (Utilities-G). Please refer to the Set Configuration documentation in Chapter 3, Main Menu
Programs. If you have that value set to Y you do not need to use this command.
If there is no mouse connected to the computer this command will be ignored.
```

