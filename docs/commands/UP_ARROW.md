# UP ARROW

| | |
|---|---|
| **Category** | Command |

## Description

When the Up Arrow key is pressed and you have not set an UPAR trap, the Up Arrow prompts the program to move backwards from the currently executing line in the program to the next previous ENTER command. Using the UPAR command you can control this process. For example, suppose you want to use a FOR/NEXT loop to enter a group of fields with a single command. You might write the following code:
START:
ENTER CUSTOMER_CODE
FOR(cntr;1;10;1)
UPAR PREV_ELEMENT() GOTO START
ENTER SALES[CNTR]
NEXT
.... (rest of the program)
QUIT
FUNC PREV_ELEMENT
IF CNTR = 1 THEN RET .F.
DEC CNTR
RET .T.
The program need only be concerned when the user presses the up arrow key. When the DN ARROW
or ENTER key is pressed the FOR/NEXT loop will automatically increment the CNTR field. When the
UP ARROW key is pressed the UDF PREV_ELEMENT() will be executed. If CNTR>1 it will be
decremented, the function will return .T. and the ENTER command within the FOR/NEXT loop will be
executed again, this time with the previous element number. Each time the user presses the UP ARROW key the function will decrement CNTR one more. When CNTR=1 the function will return .F. and
the program will transfer control to the line label START.
This program code assumes that all the fields being entered are part of a screen format that has been mounted previously.
If you use the UPAR command alone without any other options the program will not allow the user to progress any further ‘up’ the program and will effectively put a cap on the Up Arrow key.
WINDOWS NOTE: When running under Windows the UPAR (without any options) will automatically limit the field search process by setting the start line value to the UPAR line instead of checking from the beginning of the program. This along with CLIK_SRCH_LIMIT will control where, in your program, the field search process will look for matching ENTER commands to the field the user clicks on. Each time the UPAR command is executed the upper limit value is changed. You need to be sure that if you have limited the search in a subroutine of your program that you reset that upper limit when you return to the main part. The main thing to remember is that the UPAR command with no other options and the CLIK_SRCH_LIMIT will define the group of lines the field search process will check. If there is no CLIK_SRCH_LIMIT command then the process will continue to the end of the program.

## Syntax

```text
UP ARROW
```

## Parameters

- **`goto_label`** · `label` · *Optional*

  If the test_udf returns a .F. value and you have provided this goto_label, the program will transfer control to that line. If you don’t provide a goto_label, the program will continue with the line immediately above this command line.

## Example

```tas
EXAMPLE
When the user presses the Up Arrow key (and no UPAR trap is set) the program progresses backwards
from the currently executing line in the program to the next previous ENTER command. Using the
UPAR command you can control this process. For example, suppose you want to use a FOR/NEXT
loop to enter a group of fields with a single command. You might write the following code:
START:
ENTER CUSTOMER_CODE
FOR(cntr;1;10;1)
UPAR PREV_ELEMENT() GOTO START
ENTER SALES[CNTR]
NEXT
.... (rest of the program)
QUIT
FUNC PREV_ELEMENT
IF CNTR = 1 THEN RET .F.
DEC CNTR
RET .T.
The program need only be concerned when the user presses the up arrow key. When the DN ARROW
or ENTER key is pressed the FOR/NEXT loop will automatically increment the CNTR field. When the
UP ARROW key is pressed the UDF PREV_ELEMENT() will be executed. If CNTR>1 it will be
decremented, the function will return .T. and the ENTER command within the FOR/NEXT loop will be
executed again, this time with the previous element number. Each time the user presses the UP ARROW key the function will decrement CNTR one more. When CNTR=1 the function will return .F. and
the program will transfer control to the line label START.
This program code assumes that all the fields being entered are part of a screen format that has been mounted previously.
If you use the UPAR command alone without any other options the program will not allow the user to progress any further ‘up’ the program and will effectively put a cap on the Up Arrow key.
WINDOWS NOTE: When running under Windows the UPAR (without any options) will automatically limit the field search process by setting the start line value to the UPAR line instead of checking from the beginning of the program. This along with CLIK_SRCH_LIMIT will control where, in your program, the field search process will look for matching ENTER commands to the field the user clicks on. Each time the UPAR command is executed the upper limit value is changed. You need to be sure that if you have limited the search in a subroutine of your program that you reset that upper limit when you return to the main part. The main thing to remember is that the UPAR command with no other options and the CLIK_SRCH_LIMIT will define the group of lines the field search process will check.
If there is no CLIK_SRCH_LIMIT command then the process will continue to the end of the program.
```
