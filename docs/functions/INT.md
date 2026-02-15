# INT(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `INT` |
| **Returns** | `N` |

## Purpose

This function will return the integer portion only of a N type field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The N type field to use. The original field is unchanged. |

## Return Type

N

## Comments

The decimal value is not rounded up but just eliminated. The decimal characters are still there; it's just
that the value of those characters will be 0.

## Example

```tas
? int(2.935)
2.000
```
