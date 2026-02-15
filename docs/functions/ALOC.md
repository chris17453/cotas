# ALOC(1,2,3)

| | |
|---|---|
| **Category** | Function |
| **Name** | `ALOC` |
| **Returns** | `I` |

## Purpose

You can use this function to find an array element number based on a value you specify.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The field value to search for. |
| 2 | `fn/v` | The array field to search. |
| 3 | `f/c/e` | The array element value to start with. If none is provided the program will start with the first. |

## Return Type

I The element number if found.

## Comments

If there is a match for the search field value the program will return the array element number. If no match is found the return value will be 0. NOTE: Do not specify an array element in the array field to search for (part 2). If you wish to start with an element other than 1 specify that value in part 3.
