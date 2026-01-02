# WINDOW ACTIVATE

## Summary
This command will activate a window defined previously with the WINDEF (Window Define) command.
WINACT fieldname
fieldname - fn/v - Required - The name of the field specified as the save_field in the WINDEF
(Window Define) command.

## Signature
```
COMMENTS
Once the window is activated, it is just as though you had executed the standard WINDOW command.
When you activate the window the program restores the contents to what they were when it was last
active. When another window is activated, or you run the standard WINDOW command, the program
will automatically save the information currently displayed within the window. This allows you to
maintain independent windows, none of which will change the others when they are activated. Please
note that if you use the standard WINDOW command to open a window and then later redisplay a
screen, the current contents of the window are not saved.
There's an alternative to the WINDEF/WINACT commands which requires a bit of explanation.
Whenever a record is found or an expression is evaluated, the program looks at all of the fields that are
currently displayed on the screen (by a previous MOUNT command) and redisplays those that are
```

