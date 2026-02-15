# UP(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `UP` |
| **Returns** | `A` |

## Purpose

This function returns the string value (type A) specified in upper case characters.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The value to use. The original value remains unchanged. |

## Return Type

A If the receiving field is too short, the remaining characters will be truncated.

## Example

```tas
? up(‘abcdEFG’)
ABCDEFG
```

## Sample Program

`LUPTEST.SRC`
