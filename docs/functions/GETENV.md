# GETENV(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `GETENV` |
| **Returns** | `A` |

## Purpose

This function will check for a matching value in the DOS environment area and, if one is found, will return the characters after the =.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The environment variable name to search for. Don’t include the equal sign (=). |

## Return Type

A If a match is not found a null string will be returned.

## Example

```tas
? getenv(‘tas’) 
D:\PROF\
```
