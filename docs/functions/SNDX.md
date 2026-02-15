# SNDX(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `SNDX` |
| **Returns** | `A` |

## Purpose

This function returns a 4 character value representing the ‘sound’ of the field. These values can be used in comparing fields (see the DIFF() function also).

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The A type field to be converted. The original value is unchanged. |

## Return Type

A A 4 character string is returned.

## Example

```tas
? sndx(‘translate’)
T652
? sndx(‘trnslt’)
T652
? sndx(‘John’)
J500
? sndx(‘Paul’)
P400
```
