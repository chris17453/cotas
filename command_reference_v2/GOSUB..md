# GOSUB.

## Summary
Suppose you have a subroutine you want to be able to access two different ways: both as a true UDF
and as a regular subroutine. However, you want to check for successful execution by returning .T. or .F.
Structured as a standard UDF, that's very simple. For example:
IF DO_SUB() xxxx
will do or not do whatever depending on whether DO_SUB() returns .T. or .F. If you instead GOSUB
DO_SUB and place this:
IF RETVAL() xxxx
on the next line after the GOSUB, the effect would be the same. RETVAL can be used only for testing
the returned value from a UDF. You cannot just RET a value from a standard subroutine; the value will
be ignored.
This process is used extensively in the Report/2 format report writer. Header blocks are accessed both
as standard subroutines and as UDFs. The routines are actually called using GOSUBL, since it's easy to
obtain a line number for the UDF just as though it were a standard line label. You can see this in action
by creating a report using this utility and generating the source code as appropriate.

## Details
PROGRAM EDITOR
System -> Programming -> udF
