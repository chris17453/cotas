# BUTTON

| | |
|---|---|
| **Category** | Command |

## Description

Creates a button control on the screen that can emulate a keyboard key when clicked. You specify the button's size (length/height and width), colors, caption, and the key it will emulate. The command supports saving and restoring button lists, and turning lists on or off, or removing them. When a button is created it is automatically added to the current button list. Multiple lists can be active using SAVE_TO, RSTR_FROM and USING options. Buttons remain active when visible and can be clicked to trigger the corresponding key. Turn buttons off before showing lists or menus to avoid unexpected effects.

## Syntax

```text
BUTTON length width main_color caption_color caption key save_to_field restore_from_field using_holder_field OFF ON REMOVE
```

## Parameters

- **`length`** · `f/c/e` · *Required*

  The length or height of the button in rows. We recommend that a button be at least 2 rows in height. This is required only when creating the button initially.

- **`width`** · `f/c/e` · *Required*

  The width of the button in columns. This is required only when creating the button initially.

- **`main_color`** · `f/c/e` · *Optional*

  The background color of the button. If no value is entered here then the program will use the default color provided by Windows. For more information on Windows colors please see Chapter 11 - Windows Programming. This value can be specified only when creating the button initially.

- **`caption_color`** · `f/c/e` · *Optional*

  The button text color. If no value is entered here then the program will use the default color provided by Windows. For more information on Windows colors please see Chapter 11 - Windows Programming. This value can be specified only when creating the button initially.

- **`caption`** · `f/c/e` · *Optional*

  The caption or text that will appear on the face of the button. If you don't specify a value here the button will be blank. This value can be specified only when creating the button initially.

- **`key`** · `f/c/e` · *Optional*

  The keyboard key this button will emulate when it is clicked. This is required only when creating the button initially. If you don't specify a key value, or one doesn't match those available, the program will automatically treat the button as the ENTER key. The following are the acceptable values:
  F1 through F10
  SF1 through SF10 (Shift F1 etc.)
  ^F1 through ^F10 (Ctrl F1 etc.)
  @F1 through @F10 (Alt F1 etc.)
  ^A through ^Z (Ctrl A etc.)
  @A through @Z (Alt A etc.)
  ESC, UP (up arrow), DOWN (down arrow), LTA (left arrow), RTA (right arrow),
  HOME, END, PGUP, ^PGUP, PGDN, ^PGDN, INSERT, DELETE, WDLT (ctrl left
  arrow), WDRT (ctrl right arrow), TAB and BCKTAB.
  An example of this option would be: ... Key 'ESC' ...

- **`save_to_field`** · `fn/v` · *Optional*

  If you want to save the current button list so that you can temporarily create a different list you would save the current list pointer or handle to this field. The field must by of type R for this option. When this value is specified all other options, other than ON or OFF are ignored.

- **`restore_from_field`** · `fn/v` · *Optional*

  If you have saved a button list pointer or handle previously with the SAVE_TO option you would restore it with this option. When this value is specified all other options are ignored other than ON and OFF. The field for this option must be of type R.

- **`using_holder_field`** · `fn/v` · *Optional*

  If you have previously saved a button list with the SAVE_TO option and now want to turn those buttons on or off without disturbing the current button list you would use this option. This will allow you to manipulate more than one button list on the screen at a time. The only options available with this option are ON and OFF. If you want to remove the buttons permanently you should RSTR_FROM first and then REMOVE. The field in this option must be of type R.

- **`OFF`** · `option` · *Optional*

  OFF - Optional - Turn the current button list off. This means make them disappear from the screen. This will also work with the SAVE_TO, RSTR_FROM and USING options.

- **`ON`** · `option` · *Optional*

  ON - Optional - Turn the current button list on. This means to make them appear on the screen after they have been turned off previously. This will also work with the SAVE_TO, RSTR_FROM and USING options.

- **`REMOVE`** · `option` · *Optional*

  REMOVE - Optional - Delete the current button list. This will also remove the buttons from the screen.

## Comments

Each time you create a new button it is automatically added to the current button list. Its position on the list doesn't have any effect on where it appears on the screen, it's just where it is internally in the program. You can keep multiple lists active at the same time by using the SAVE_TO, RSTR_FROM and USING options. Even though the button may not be part of the current active list, if it's on the screen and the user clicks it, it's just as though the user pressed the appropriate key. Another thing to remember is that buttons are always active. This means if the button is on the screen (visible) the user can always click on it, regardless of what else is active. Even if just a little bit of the button is showing under another window, or menu, the user can still click on that and it's just as though they pressed the corresponding key. Since this could have unexpected effects on your program you need to be sure that you've turned the buttons off before bringing up a list or menu, etc.
For example, let's say you have buttons on the screen but you want to display a list with different buttons. You also want to redisplay those original buttons when you are finished with the list. To accomplish this you would do something like the following:
Button save_to field off

Button at x,y ....
Saves

Window at x1,y1 ...
Listf ....
Redsp
Button rstr_from field on

;this would save the current
;button list and turn the
;current buttons off
;create new button or buttons.
;save current screen before
;displaying the window.
;NOTE: SAVES and REDSP have
;no effect on buttons.
;create the appropriate
;window for the list
;do the listf
;get rid of the window
;now restore the original
;buttons.

This is a very simple example but it gives you an idea of how the process would work.
You also don't have to worry about removing buttons created during a program. That will be done automatically for you.

## Sample Program

`WINTEST`

## Program Editor

`Win Commands-> Button`

## See Also

- [HOT_SPOT](HOT_SPOT.md)
- [PICTURE](PICTURE.md)
