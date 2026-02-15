# LOW(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `LOW` |
| **Returns** | `A` |

## Purpose

This function returns the A type field specified in lower case characters.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The value to use. The original value is left unchanged. |

## Return Type

A If the receiving field is too short, the remaining characters will be truncated.

## Example

```tas
? low(‘ABcdEFG’)
abcdefg
```
