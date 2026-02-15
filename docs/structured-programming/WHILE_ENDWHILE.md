# WHILE/ENDWHILE

| | |
|---|---|
| **Category** | Structured Programming |
| **Max Nesting** | 20 deep |

## Description

WHILE
This structure is a loop that will continue until the WHILE lexpr returns .F. or the loop is exited. The
general layout of the structure is :
WHILE lexpr
EXIT
EXIT_IF lexpr
LOOP
LOOP_IF lexpr
ENDW

WHILE lexpr
The lexpr in this command must be in the form of a comparison expression. For example:
WHILE x = 1
If this expression resolves to .T., the line after the WHILE command will be executed;
otherwise control will be transferred to the line after the ENDW command. This expression is
checked each time the loop is executed.
The programmer can stay in the loop until an EXIT or EXIT_IF command is executed by
making sure this command will always return .T. This can be accomplished easily by the following:
WHILE .T.

If an EXIT or EXIT_IF command is never executed the loop will continue on and will never quit. (Unless, of course, an error occurs.)

EXIT
EXIT_IF lexpr
These elements of the WHILE/ENDW loop will allow you to exit the loop. The EXIT option
exits the loop immediately. The EXIT_IF lexpr will exit the loop if the expression resolves to
.T.; otherwise nothing will happen. The lexpr must be in the form of a comparison expression.
For example:
EXIT_IF x = 1
The EXIT and EXIT_IF (assuming the lexpr resolves to .T.) will transfer control to the next
line after the ENDW command.
As many EXIT and EXIT_IF commands may be included in a single WHILE/ENDW loop as you desire.

LOOP
LOOP_IF lexpr
These elements of the WHILE/ENDW loop will allow you to transfer control to the beginning of
the loop (WHILE command) without having to reach the ENDW command. The LOOP option transfers control immediately. The LOOP_IF lexpr will transfer control if the expression resolves to .T.; otherwise nothing will happen. The lexpr must be in the form of a
comparison expression. For example:
LOOP_IF x = 1
The WHILE comparison expression is executed and then, if the expression resolves to .T., the
loop will continue.
As many LOOP, LOOP_IF commands may be included in a single WHILE/ENDW loop as you desire.

ENDW
This is the final step in the WHILE/END loop. When this command is executed control is returned to the WHILE command line. Each WHILE must have a ENDW to finish the loop.
If the ENDW is not included an error message will be displayed during compilation.

## Syntax

```tas
WHILE lexpr
EXIT
EXIT_IF lexpr
LOOP
LOOP_IF lexpr
ENDW
```

## Keywords

### `EXIT`

EXIT
This exits the loop immediately.

### `EXIT_IF lexpr`

EXIT_IF lexpr will exit the loop if the expression resolves to .T.; otherwise nothing will happen. The lexpr must be in the form of a comparison expression.

### `LOOP`

LOOP transfers control to the beginning of the loop immediately.

### `LOOP_IF lexpr`

LOOP_IF lexpr transfers control to the beginning of the loop if the expression resolves to .T.; otherwise nothing will happen.

### `ENDW`

ENDW is the final step in the WHILE/ENDW loop. When this command is executed control is returned to the WHILE command line.

## Example

```tas
WHILE x = 1
...
IF x = 1 ...
ENDW

WHILE .T.
EXIT_IF x = 5
LOOP
...
ENDW
```
