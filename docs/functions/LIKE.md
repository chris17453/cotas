# LIKE(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `LIKE` |
| **Returns** | `L` |

## Purpose

This function will return a logical value indicating whether the two fields are alike. You may use the
skeleton characters ? and * in the first field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Comparison field 1. If you are going to use the skeleton characters (variable character specifiers) ? or *, they must be in this field. Must be an A type value. |
| 2 | `f/c/e` | Comparison field 2. Must be an A type field. |

## Return Type

L If the fields match the function will return .T., otherwise .F.

## Comments

The character ? will match any other single character in that same position in field 2. The character *
will match all following characters from that position on. If comparison field 1 is shorter than field 2
and the last character in field 1 is NOT * the fields will not match and .F. will be returned. Differences
in upper and lower case are treated as differences in characters.

## Example

```tas
:? like(‘ABC’,’abc’)
.F.

&& same characters, different case

? like(‘this*’,’this is a test’)
.T.

&& last character in
&& comp fld1 is ‘*’ and
&& first chrs match

? like(‘th?s’,’this’)
.T.

&& ? will match any single chr.
```
