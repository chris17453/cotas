# DTOS(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `DTOS` |
| **Returns** | `A` |

## Purpose

This function will return a date value as a string in the form YYYYMMDD.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Date value to use. |

## Return Type

A An 8 character string is always returned.

## Example

```tas
? dtos(date())
19940623
```

## Sample Program

`LOCTEST.SRC`
