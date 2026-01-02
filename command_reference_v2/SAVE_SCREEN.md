# SAVE SCREEN

## Summary
This command will save a screen so that it can later be redisplayed using the REDSP (Redisplay
Screen) command.
SAVES screen_holder_name
screen_holder_name - fn/v - Optional - The name of the field that is to be used for saving the
screen. If you did not specify a field name in this command, the program will allocate
memory space for a temporary buffer to hold the screen and the appropriate information. In this case, when you execute the REDSP (Redisplay Screen) command you
would leave out the field name there also.
NOTE: If you do specify a field name it must be large enough to hold the entire
screen and certain information about the current screen environment. This is approximately 4300 bytes.

## Signature
```
COMMENTS
If you allow the program to save and redisplay the screen from an internal buffer you need to be sure
that you balance your SAVES and REDSP (Redisplay Screen) commands. Each time the program
saves a screen to an internal buffer it allocates the memory required to do so. If you don’t redisplay the
screens and continue to save them you will eventually run out of memory. This does not apply to the
situation where you save the screen to a named field.
When you QUIT the program any buffers used for saving screens are automatically released.
```

## Details
PROGRAM EDITOR
User interface -> Windows -> Save scrn
