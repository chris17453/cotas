# LISTF_CHRS()

| | |
|---|---|
| **Category** | Function |
| **Name** | `LISTF_CHRS` |
| **Returns** | `A` |

## Purpose

This function will return the characters entered by the user when searching for records during a LISTF
command.

*No parameters.*

## Return Type

A The characters entered by the user using Fast Search.

## Comments

This function can be used at the end of a command to put the characters entered by the user into the
ENTER field if a record wasn't found. For example:
LISTF bkar.custcode, ' ', bkar.custname . . .
IF bkar.custcode= ' ' THEN bkar.custcode=LISTF_CHRS()
RET
