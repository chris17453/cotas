# GFLD(1,2,3,4)

| | |
|---|---|
| **Category** | Function |
| **Name** | `GFLD` |
| **Returns** | `A` |

## Purpose

This function returns a field from a delimited file buffer previously read with the READ or FINDV
(Find Variable) command.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The field number in the delimited record. The first value in the record is field 1. |
| 2 | `f/c/e` | The file_number for the file that contains the delimited records. You must provide a file_number or provide a field or constant in part 4. |
| 3 | `f/c/e` | The delimiter between fields in the buffer. If this value is not provided the program assumes a comma ‘,’. |
| 4 | `f/n/v` | A field to be used instead of a file_number. |

## Return Type

A

## Comments

The quotes surrounding A type fields in the delimited record are automatically removed before the
value is returned.
If you specify a field name as part 4, the program will treat that field as a record buffer so that the
overall process will remain the same as if you had specified a file_number. If you use this option, be
sure to put in a place-holding comma where the file_number would ordinarily go. In the example
below, the programmer has not specified a file_number and has used the default delimiter value:
x=glfd(5,,,field_holder)
Please note that if the #PRO3 compiler directive is set, then the program will use the non-TAS file
pointed to by the special file number as the buffer.
