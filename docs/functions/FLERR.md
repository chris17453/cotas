# FLERR(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `FLERR` |
| **Returns** | `I` |

## Purpose

This function will return the error number for the last access for a particular file.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The file_number of the file to be checked. |

## Return Type

I The last file error number.

## Comments

If the last access was successful the FLERR() value returned will be 0. This function works for both TAS and non-TAS files. The error number returned will correspond to standard TAS Professional runtime errors. For more information please refer to Chapter 10, Runtime Errors.
