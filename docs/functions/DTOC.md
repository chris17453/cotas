# DTOC(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `DTOC` |
| **Returns** | `A` |

## Purpose

This function will convert a D type field into an A type field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The D type field to be converted. The original field value is not changed. |
| 2 | `f/c/e` | The size of the A type version of the date to return. The default is 8 (mm/dd/yy). You can use any legal date size as the value. |

## Return Type

A If the receiving field is not long enough the returned value will be truncated.

## Example

```tas
SAMPLE PROGRAM
DATETEST.SRC
```
