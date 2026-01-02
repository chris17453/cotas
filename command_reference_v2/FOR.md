# FOR

## Summary
This is the first part of the FOR/NEXT loop. In this part of the command you can specify the counter,
start value, stop value, and the amount to add to or subtract from the counter each time the loop is
executed.
FOR(counter;start_value;stop_value;step_value)
counter- fn - Required - The name of the field used as a loop counter.
start_value - f/c/e - Required - The first value to be used in the loop. The counter field is
initialized to this value the first time through.
stop_value - f/c/e - Required - When the counter reaches this value the program knows to exit
the loop at the NEXT command.
step_value - f/c/e - Required - How much to add to or subtract from counter each time through
the loop. If you wish to subtract (or decrement) counter in the loop, precede the
value with the minus sign. For example, a step_value of -1 will reduce the counter
value by 1 each time the loop is executed.

## Signature
```
COMMENTS
All fields/values in the FOR command except counter must be of I type. The counter can be either I or
R type. The loop will always be executed at least once so you need to make sure that the actions to be
taken are acceptable at all times. The loop will continue through the stop_value.
For more information on the different structured programming commands, and the FOR command in
particular, please see Chapter 7, Structured Programming Commands.
```

## Details
PROGRAM EDITOR
