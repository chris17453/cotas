# function_parameters

| | |
|---|---|
| **Category** | Command |
| **Platform** | TAS Professional 5.1 |

## Description

The values being passed to the UDF. A maximum of 20 fields may be specified. The field names are separated with commas.

## Syntax

```text
function_parameters - fn/v1,fn/v2,...,fn/vx - Optional - The values being passed to the UDF.
```

## Parameters

- **`function_parameters`** · `fn/v` · *Optional*

  The values being passed to the UDF. A maximum of 20 fields may be specified. The field names are separated with commas.

## Comments

You must include the compiler directive UDF before the first user defined function is referenced in the program. All functions end with the command RET (Return). You may return a single value of any type, including an expression. Fields within the UDF are not automatically local. However, you may use the PUSHF (Push Field)/POPF (Pop Field) commands for saving/restoring all appropriate field values. Through this process the UDF may be made reentrant.
Any legal TAS Professional 5.1 command may be used in conjunction with a UDF.
Any UDF can be called as though it were a subroutine instead of a function. For example, a UDF with the name of CALC_EXP can be included as part of an expression:
TOTAL_EXP=CALC_EXP()
You can also GOSUB CALC_EXP. Note that in this case, you don\'t include the parentheses after the name, and you cannot pass data to the expression. So: you have two different ways to access any function.
If you do a GOSUB to a function, there is no built-in method of finding out what the returned value was.
A function called RETVAL() allows you to do just that and must be the next executed line after the GOSUB.
Suppose you have a subroutine you want to be able to access two different ways: both as a true UDF and as a regular subroutine. However, you want to check for successful execution by returning .T. or .F.
Structured as a standard UDF, that\'s very simple. For example:
IF DO_SUB() xxxx
will do or not do whatever depending on whether DO_SUB() returns .T. or .F. If you instead GOSUB
DO_SUB and place this:
IF RETVAL() xxxx
on the next line after the GOSUB, the effect would be the same. RETVAL can be used only for testing
the returned value from a UDF. You cannot just RET a value from a standard subroutine; the value will
be ignored.
This process is used extensively in the Report/2 format report writer. Header blocks are accessed both
as standard subroutines and as UDFs. The routines are actually called using GOSUBL, since it\'s easy to
obtain a line number for the UDF just as though it were a standard line label. You can see this in action
by creating a report using this utility and generating the source code as appropriate.

## Example

```tas
#udf
define x type n
clrscr
x = user_func(1,3,’A’)
?x
quit
define y,z,a type n
define do type a size 1
func user_func y,z,do
if do = ‘A’
a = y+z
else_if do = ‘S’
a = y-z
else_if do = ‘M’
a = y*z
else_if do = ‘D’
a = y/z
endif
ret a

The above is a complete program. The result would be:
4.00
```

## Sample Program

`UDFTEST.SRC`

## Program Editor

`System -> Programming -> udF`
