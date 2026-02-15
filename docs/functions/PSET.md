# PSET(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `PSET` |
| **Returns** | `I` |

## Purpose

This function will return the currently set value for printer width, printable lines, or maximum lines.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | What to get: ‘W’ - printer width, ‘P’ - printable lines, or ‘M’ - maximum lines. |

## Return Type

I The current value depending on the option.

## Example

```tas
? pset(‘w’)
80
? pset(‘m’)
66
```
