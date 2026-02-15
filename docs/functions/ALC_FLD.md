# ALC_FLD(1,2)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ALC_FLD` |
| **Returns** | `I` |

## Purpose

You can use this function to allocate a single previously defined field.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `fn/v` | The field to allocate. This must be a type A field. |
| 2 | `f/c/e` | The new desired size. Maximum size possible is 65,535. If the amount of available memory is less than the size given the program will be allocated as much as possible. |

## Return Type

I The actual size allocated.

## Comments

The field (part 1) used in this command cannot be a part of a standard TAS Professional 5.1 file. It must have been created using the DEFINE command. Once this function has been used to allocate the field the data space originally used by the field is lost to use. Due to this you should define the field with a size of 1 initially. This keeps the amount of wasted space to a minimum.
