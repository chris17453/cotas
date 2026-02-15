# ACOS(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ACOS` |
| **Returns** | `N` |

## Purpose

This function returns the arccosine of the N type field specified.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The N value. |

## Return Type

N The result will be from 0 to PI in radians.

## Comments

The value specified must be between -1 and 1.

## Example

```tas
? acos(-0.539)
2.14004577
```
