# INT

## Summary
Executes trap if user presses key
Executes trap if user presses SHIFT & key
Executes trap if user presses CTRL & key
Executes trap if user presses CTRL & key
Executes trap if user presses CTRL & key
Executes trap if user presses ALT & key
Executes trap if user presses CTRL & key
Executes trap if user presses ALT & key
Executes trap if user presses Escape key during entry
Executes trap if user presses Escape key during program
operation, not at an entry
Executes trap if user presses Escape key during entry.
Temporary and will override the ESC trap. This is
automatically cleared after any ENTER or RETURN.
Executes trap if user presses up arrow (^) key
Executes trap if user presses down arrow key
Executes trap if user presses left arrow
(<-) key
Executes trap if user presses right arrow
(->) key
Executes trap if user is at the beginning of the field and
presses the left arrow key
Executes trap if user is at the end of the field and presses
the right arrow key
Executes trap if user presses HOME key
Executes trap if user presses END key
Executes trap if user presses Page Up key
Executes trap if user presses Page Down key
Executes trap if user presses Insert key
Executes trap if user presses DEL or Delete key
Executes trap if user presses the CTRL (control) and left
arrow (<-) keys at the same time
Executes trap if user presses the CTRL (control) and right
arrow (->) keys at the same time
Executes trap if user presses the TAB key
Executes trap if user presses the SHIFT and TAB key at
the same time
Executes if user searches for a record in a file using either
the default system keys or a FINDx command. NOTE:
This trap cannot use the GOTO option. If it is not DFLT
or IGNR (both have same effect) then it must be GOSUB.
Even if you set the do what option to GOTO it will still be
treated as a GOSUB.
Executes trap if the program attempts to read a record that
is locked (it has been read by another user’s program) or if

## Signature
```
T_ESC
```

## Details
UPAR
DNAR
LT_A
RT_A
LT_A_AS
RT_A_AS
HOME
END
PG_UP
PG_DN
