# VAL(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `VAL` |
| **Returns** | `N` |

## Purpose

This function converts a string value (type A) to a numeric value (type N).

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The value to be converted. The original value remains unchanged. |

## Return Type

N

## Example

```tas
? val(‘100.2’)
100.20
? val(‘abcd’)
0.00
```

## Sample Program

`TRANSTST.SRC`
