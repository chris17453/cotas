# COPY_FILE(1,2) (Windows Only)

| | |
|---|---|
| **Category** | Function |
| **Name** | `COPY_FILE` |
| **Platform** | Windows Only |
| **Returns** | `L` |

## Purpose

You must use this function to copy a file in Windows. You cannot use standard DOS commands.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | From file name. Must be type A. |
| 2 | `f/c/e` | To file name. Must be type A. |

## Return Type

L If the copy is complete the function returns .T., otherwise it returns .F..
