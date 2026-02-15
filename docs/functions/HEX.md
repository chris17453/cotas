# HEX(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `HEX` |
| **Returns** | `A` |

## Purpose

This function converts a numeric value to hexidecimal number and returns that number as an A type
field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The number to be converted. |

## Return Type

A

## Comments

All values are converted to R type first and then to hex.

## Example

```tas
? hex(100)
64
? hex(110)
6e
```
