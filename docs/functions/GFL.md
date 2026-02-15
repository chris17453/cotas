# GFL(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `GFL` |
| **Returns** | `A` |

## Purpose

This function will take a format line from a mounted report format and convert it to an alpha field with
all appropriate values set.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The number of the format line to use. |

## Return Type

A If the format line number is not accurate or if a report format has not been mounted, a blank value
will be returned. If the receiving field is not large enough to contain the entire line the remaining
characters will be truncated.

## Comments

This is the same as printing the report format line except the line is being placed in an A type field.
