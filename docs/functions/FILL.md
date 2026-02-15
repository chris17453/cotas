# FILL(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `FILL` |
| **Returns** | `A` |

## Purpose

This function will return an A type field of a specific size and made up entirely of a specific character.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The size of the field to be returned. |
| 2 | `f/c/e` | The fill character to be used. This must be an A type field and only the first character of that field will be used. |

## Return Type

A An A type field made up of the specified fill character and of the requested size.

## Example

```tas
? FILL(10,’*’)
```
