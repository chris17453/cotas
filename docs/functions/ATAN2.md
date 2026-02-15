# ATAN2(1,2) (DOS only)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ATAN2` |
| **Platform** | DOS only |
| **Returns** | `N` |

## Purpose

This function returns the arctangent of field value1/field value2.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Field 1, must be N type. |
| 2 | `f/c/e` | Field 2, must be N type. |

## Return Type

N The result will be from -PI to PI in radians.

## Example

```tas
? atan2(-0.539,-1.325)
-2.75524446
```
