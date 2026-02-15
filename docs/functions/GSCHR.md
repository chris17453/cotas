# GSCHR(1,2,3)

| | |
|---|---|
| **Category** | Function |
| **Name** | `GSCHR` |
| **Returns** | `A` |

## Purpose

This function returns the character or color attribute from a specific location on the screen.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | If ‘C’ the function will return the displayed character, if any, from the screen. If ‘A’, the function will return the attribute, or color number. |
| 2 | `f/c/e` | The row value. The top row in the active window is 1. |
| 3 | `f/c/e` | The column value. The leftmost column in the active window is 1. |

## Return Type

A If part 1 is ‘C’
B If part 1 is ‘A’

## Comments

TAS Professional 5.1
Copyright © Business Tools, Inc. 1985-1996 All Rights Reserved
