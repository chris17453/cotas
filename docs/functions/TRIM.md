# TRIM(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `TRIM` |
| **Returns** | `A` |

## Purpose

This function will return an A type field with the leading or trailing spaces deleted.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The value to be converted. The original field is not changed. |
| 2 | `f/c/e` | 'T' - trim trailing spaces; this is the default value. 'L' - trim leading spaces. |

## Return Type

A If the receiving field is not long enough, the remaining characters will be truncated.

## Comments

You would use this function when concantenating (adding) A type fields together. If the spaces aren’t trimmed, there will probably be extra spaces in the resulting value. This function can get rid of them.
You can also use the ‘*’ when concatenating the fields. For more information please see Chapter 1, Installation and General Information.
