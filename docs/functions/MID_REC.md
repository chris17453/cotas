# MID_REC(1,2,3)

| | |
|---|---|
| **Category** | Function |
| **Name** | `MID_REC` |
| **Returns** | `A` |

## Purpose

This function will return a portion of an active record in a specified file.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The file_number of the file to use. |
| 2 | `f/c/e` | The starting character position. The first character of a record is position 1. |
| 3 | `f/c/e` | The number of characters to return. If the value given would go beyond the end of the record, only those characters up to the end of the record will be passed. |

## Return Type

A If the receiving field is too short, the remaining characters will be truncated.
