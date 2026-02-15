# OFST(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `OFST` |
| **Platform** | DOS Only |
| **Returns** | `R` |

## Purpose

This function returns the offset for a field or memory area. This is the position of the first character in memory.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `fn/v` | Field to use. |
| 2 | `f/c/e` | The memory area number. If this is set properly, you do not have to specify part 1. However, you do still need to put the comma in for place keeping, i.e., =loc(,1) |

## Return Type

R This returns the absolute (flat) memory location.
