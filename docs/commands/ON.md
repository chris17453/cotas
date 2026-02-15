# ON

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to transfer control to a line label depending on a value. This is almost like a flat CASE command.

## Syntax

```text
ON expression GOTO/GOSUB line_label_list
```

## Parameters

- **`expression`** · `f/c/e` · *Required*

  The value to test. Must resolve to a numeric value.

- **`GOTO/GOSUB`** · `keyword` · *Required*

  GOTO or GOSUB to the appropriate line.

- **`line_label_list`** · `label_list` · *Required*

  The list of line labels used to determine where to transfer control. The first label corresponds to a value of 1, the second to 2, etc. If the expression resolves to a value of 0 or greater than the number of labels -1 then control is transferred to the last label in the list. The line labels are separated by commas.

## Program Editor

`prg Control -> Goto/gosub -> On`

## See Also

- [GOSUB](GOSUB.md)
- [GOTO](GOTO.md)
