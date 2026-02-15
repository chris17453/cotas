# IIF(1,2,3)

| | |
|---|---|
| **Category** | Function |
| **Name** | `IIF` |
| **Returns** | `?` |

## Purpose

This function can return many different values depending on whether the logical expression provided
returns .T. or .F.

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `lexpr` | The expression to evaluate to determine what should be returned. |
| 2 | `f/c/e` | If the lexpr in part 1 evaluates to .T. the function will return this value. |
| 3 | `f/c/e` | If the lexpr evaluates to .F. the function will return this value. |

## Return Type

? The return type depends entirely on you.

## Comments

If you are only concerned about one type of return (i.e., only .T. or only .F.), the other part need not be
included.

## Example

```tas
x=1
? iif(x=1,,’False’)
(the return above will be blank (“”) )
? iif(x=1,’True’)
True
```

## Sample Program

`IIFTEST.SRC, IIFTEST2.SRC`
