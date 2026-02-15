# MDY(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `MDY` |
| **Returns** | `A` |

## Purpose

This function returns a date value in the form of Month, Day, Year.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Date value to use. |
| 2 | `f/c/e` | If set to ’S’, only the first three characters of the month value are used (e.g., Jan = January). |

## Return Type

A The size of the string returned depends on the month.

## Example

```tas
? mdy(date(),’s’)
Jun 23, 94
```
