# WRAP(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `WRAP` |
| **Returns** | `A` |

## Purpose

This function will wrap a long string value (type A) so that it may be output to the screen or printer as a
block of characters. As opposed to the WRAP command this function would be used in a report format
or in a PRINT MESSAGE command.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The string value to use. The original field remains unchanged. |
| 2 | `f/c/e` | How many columns (characters) wide to make the block. |
| 3 | `f/c/e` | WRAP()  Whether or not to trim the trailing spaces at the end of the block. This will allow the next line to be output immediately after the end of the last non-space line. ‘Y’ - trim trailing spaces. This is the default value. ‘N’ - don’t trim spaces. |

## Return Type

A If the receiving field is too short, the remaining characters will be truncated.

## Example

```tas
field1 = ‘abcd’
field2 = ‘now is the time for all good men to come to the aid of their party.’
field3 = 100.00
? field1,’ ‘,wrap(field2,20),’ ‘,field3
abcd now is the time for 100.00
all good men to come
to the aid of their
party.

SAMPLE PROGRAMS
WRAPTEST.SRC, WRPTEST2.SRC
```
