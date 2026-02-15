# DIFF(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `DIFF` |
| **Returns** | `I` |

## Purpose

Using a process similar to the SNDX() function, this function will return a value that can be used to determine how ‘close’ the two values ‘sound.’

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | Comparison value 1. Must be type A. |
| 2 | `f/c/e` | Comparison value 2. Must be type A. |

## Return Type

I The return value is from 0 thru 4. 0 is no match at all; 4 is a very close match.

## Example

```tas
? DIFF(‘translate’,’trnslt’)
4
? DIFF(‘John’,’Paul’)
0
```
