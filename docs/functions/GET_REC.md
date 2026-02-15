# GET_REC(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `GET_REC` |
| **Returns** | `A` |

## Purpose

This function will return an individual record from a buffer that has been used to store an entire file.
Before using this function you should have read a non-TAS type file (general text or X type) into a field
in your program that you are using as a temporary buffer.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `fn/v` | The name of the field you are using as a buffer. |
| 2 | `f/c/e` | The record number you want to retrieve. The first record in the buffer is 1. |

## Return Type

A The size of this field depends on the number of characters in the record. The termination
characters(s) at the end of the record is not returned as part of the record.

## Comments

Each record in the buffer must be terminated by a carriage return (CR) or a carriage return/line feed
pair (CR/LF) or a binary 0. If you attempt to get more records than are in the file, the function will
return a blank field with a length of 0. This also applies if there are no characters in the record to
return.
