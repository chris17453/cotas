# MAKE_DIR(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `MAKE_DIR` |
| **Platform** | Windows Only |
| **Returns** | `L` |

## Purpose

This function creates a new directory.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The name of the directory to create. |

## Return Type

L If the directory is created properly the function returns .T., otherwise it returns .F.

## Comments

This is the way you create a new directory in Windows. You cannot execute the DOS command MD.
