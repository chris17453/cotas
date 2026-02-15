# MESSAGE

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to display a message at the standard message location.

## Syntax

```text
MSG expression WINDOWS icon_type NOWAIT
```

## Parameters

- **`expression`** · `f/c/e` · *Required*

  You may use this in two ways. The expression can be set as a A type field and the program will display the field as the message. It can also be set as a numeric value (any appropriate type) and the program will read the appropriate record in the ERRMSG.B file. The value set by you must relate to the ERROR_NUM field in the ERRMSG record. For more information please see Chapter 3, Main Menu - Utilities.

- **`icon_type`** · `f/c/e` · *Optional*

  Windows Only - Optional - If you want to display a standard Windows information or warning dialog box you can use this option. By setting this value to 'info' or 'warn' (note: only the first character matters) the program will display a dialog box with an information icon or with a warning icon, the message and an OK button at the bottom of the box. The program will stop until the user clicks on the button or press es the ENTER key. If you use this option you must supply the message expression. It cannot be retrieved from the ERRMSG.B file. Also, in a normal MSG command you are limited to 320 characters. That limit does not apply when this option is used. If this option is specified the NOWAIT option is ignored.

- **`NOWAIT`** · `flag` · *Optional*

  Optional - If this option is included in the command the message will be displayed but it will not wait for the user to press a key to continue as would be normal. The message will remain displayed on the screen. It is then up to you to either REDSP (Redisplay) a previously saved screen or CLRSCR (Clear Screen) to remove the message.

## Comments

By using the ERRMSG.B file as the repository of any messages (e.g., help messages), you can easily
add, change, delete, etc. any appropriate message. You should start any message numbers at 5000 or
more. This will help make certain that no messages will have to change due to interference with records
written by Business Tools and provided with the system. Obviously, the ERROR_NUM values don’t
have to be sequential.
NOTE: In a normal MSG command (where the WINDOWS option is not specified) you are limited to
320 characters total for any message.

## Sample Program

`WINTEST (Windows Only)`

## Program Editor

`User interface -> Messages -> Disp message`
