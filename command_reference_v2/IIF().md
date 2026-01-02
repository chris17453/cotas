# IIF()

## Summary
NOTE: You must be careful about what you put into the True/False options in this function. Due to
the way the expression compiler and executor work, if the values to be returned are expressions also
(including UDFs), they will be resolved (executed) before the value of the IIF() is determined. For
example:
x=5
y=10
? iif(x>y,fill(x-y,’ ‘),2)
If x>y was true then the routine above would use the difference to calculate how many space characters
(‘ ‘) to print. However, since the inner expressions are evaluated before the IIF() an unexpected result
could occur, as in this case. The program will attempt to make a string of spaces 65530 characters long
(the FILL() function converts numbers to I type automatically). The proper way to set this up would
be:
? fill(iif(x>y,x-y,2), ' ')
This would work properly each time.

