# DMY(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `DMY` |
| **Returns** | `A` |

## Purpose

This function returns a date value in the form of Day Month Year.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Date value to use. |
| 2 | `f/c/e` | If set to ‘S’ only the first three characters of the month value are used (e.g., Jan = January) and only the last two digits of the year. If set to ‘L’ (long) then the full month name and the full year are used. |

## Return Type

A

## Example

```tas
? dmy(date(),’s’)
23 Jun 94
? dmy(date(),’l’)
23 June 1994
```
