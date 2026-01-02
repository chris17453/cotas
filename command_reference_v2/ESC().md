# ESC()

## Summary
PURPOSE
If the user presses the ESC key at a message and the appropriate ESC traps are set to ignore then the
internal esc_pressed flag is set to 1. You can test for that flag being set using this function.

## Signature
```
NO OTHER PARTS
RETURN TYPE
L If the user pressed the ESC key this will be .T..
```

## Details
COMMENTS
Once this flag is checked it will be reset to .F.. It is also cleared before the next MESSAGE or ENTER
command.
