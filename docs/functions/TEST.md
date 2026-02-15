# TEST(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `TEST` |
| **Returns** | `L` |

## Purpose

This function will determine whether a bit or bits are set in a byte.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `fn/v` | Field to check. The function will only test the first byte/character. |
| 2 | `f/c/e` | This value determines which bits to check. To set the proper value use the following\nguide: Bit# Value  7 128  6 64  5 32  4 16  3 8  2 4  1 2  0 1  |

## Return Type

L If the bit(s) is (are) set (1 not 0), the function will return .T. Otherwise it will return .F.

## Example

```tas
SAMPLE PROGRAM
CAPSLOCK.SRC
```
