# FOR/NEXT

| | |
|---|---|
| **Category** | Structured Programming |
| **Max Nesting** | 20 deep |

## Description

FOR/NEXT
This is a loop that will continue until the stop level is reached. The general layout of the structure is :
FOR(counter;start_value;stop_value;step_value)
FEXIT
FEXIT_IF lexpr
FLOOP
FLOOP_IF lexpr
NEXT

FOR etc.
There are four different sections to this command. They are the counter field, a starting value, a stop value and a step value.
counter
The name of the field used as a loop counter. The counter can be I or R type.
start_value
The first value to be used in the loop. The counter field is initialized to this value the first
time through.

TAS Professional 5.1
Copyright  Business Tools, Inc. 1985-1996 All Rights Reserved

stop_value
When the counter reaches this value the program knows to exit the loop at the NEXT command.
step_value
How much to add or subtract from counter each time through the loop. If you wish to
subtract from counter (i.e., decrement), precede the value with the minus sign. For example,
a step_value of -1 will reduce the counter value by 1 each time the loop is executed.
Each time through the loop (except the first time) the program adds (or subtracts if step_value
is negative) the step_value. If the result is greater than the stop_value (in the case of a
positive step_value) or less than the stop_value (in the case of a negative step_value), the
program will transfer control to the line after the appropriate NEXT command.
In TAS Professional 3.0, if you pressed the UP ARROW key (assuming no traps), and the
program encountered a FOR/NEXT loop, the counter would be decremented or incremented
as appropriate. This feature is also available in TAS Professional 5.1. You do not need to set
the #PRO3 compiler directive to activate this feature. Please be aware that in TAS Professional 5.1, the counter is always +1 (or -1) greater (or less) than the stop_value at the end of
the loop. If the #PRO3 directive is set, the counter will be equal to the stop_value when the
loop is finished.

FEXIT
FEXIT_IF lexpr
These elements of the FOR/NEXT loop will allow you to exit the loop before the counter
reaches the stop_value. The FEXIT option exits the loop immediately. The FEXIT_IF lexpr
will exit the loop if the expression resolves to .T.; otherwise nothing will happen. The lexpr
must be in the form of a comparison expression. For example:
FEXIT_IF x = 1
The FEXIT and FEXIT_IF (assuming the lexpr resolves to .T.) will transfer control to the
next line after the NEXT command.
As many FEXIT and FEXIT_IF commands may be included in a single FOR/NEXT loop as
the programmer desires.

FLOOP
FLOOP_IF lexpr
These elements of the FOR/NEXT loop will allow the program to transfer control to the
beginning of the loop (FOR command) without having to reach the NEXT command. The
FLOOP option transfers control immediately. The FLOOP_IF lexpr will transfer control if
the expression resolves to .T.; otherwise nothing will happen. The lexpr must be in the form
of a comparison expression. For example:
FLOOP_IF x = 1
As many FLOOP and FLOOP_IF commands may be included in a single FOR/NEXT loop
as the programmer desires.

NEXT
This is the final step in the FOR/NEXT loop. When this command is executed control is
returned to the FOR command line. Each FOR must have a NEXT to finish the loop. If the
NEXT is not included an error message will be displayed during compilation.

## Syntax

```tas
FOR(counter;start_value;stop_value;step_value)
FEXIT
FEXIT_IF lexpr
FLOOP
FLOOP_IF lexpr
NEXT
```

## Keywords

### `counter`

The name of the field used as a loop counter. The counter can be I or R type.

### `start_value`

The first value to be used in the loop. The counter field is initialized to this value the first time through.

### `stop_value`

When the counter reaches this value the program knows to exit the loop at the NEXT command.

### `step_value`

How much to add or subtract from counter each time through the loop. If you wish to subtract from counter (i.e., decrement), precede the value with the minus sign. For example, a step_value of -1 will reduce the counter value by 1 each time the loop is executed. Each time through the loop (except the first time) the program adds (or subtracts if step_value is negative) the step_value. If the result is greater than the stop_value (in the case of a positive step_value) or less than the stop_value (in the case of a negative step_value), the program will transfer control to the line after the appropriate NEXT command. In TAS Professional 3.0, if you pressed the UP ARROW key (assuming no traps), and the program encountered a FOR/NEXT loop, the counter would be decremented or incremented as appropriate. This feature is also available in TAS Professional 5.1. You do not need to set the #PRO3 compiler directive to activate this feature. Please be aware that in TAS Professional 5.1, the counter is always +1 (or -1) greater (or less) than the stop_value at the end of the loop. If the #PRO3 directive is set, the counter will be equal to the stop_value when the loop is finished.

### `FEXIT`

These elements of the FOR/NEXT loop will allow you to exit the loop before the counter reaches the stop_value. The FEXIT option exits the loop immediately.

### `FEXIT_IF lexpr`

The FEXIT_IF lexpr will exit the loop if the expression resolves to .T.; otherwise nothing will happen. The lexpr must be in the form of a comparison expression. For example: FEXIT_IF x = 1

### `FLOOP`

These elements of the FOR/NEXT loop will allow the program to transfer control to the beginning of the loop (FOR command) without having to reach the NEXT command. The FLOOP option transfers control immediately.

### `FLOOP_IF lexpr`

The FLOOP_IF lexpr will transfer control if the expression resolves to .T.; otherwise nothing will happen. The lexpr must be in the form of a comparison expression. For example: FLOOP_IF x = 1

### `NEXT`

This is the final step in the FOR/NEXT loop. When this command is executed control is returned to the FOR command line. Each FOR must have a NEXT to finish the loop. If the NEXT is not included an error message will be displayed during compilation.
