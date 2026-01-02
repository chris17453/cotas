# ON

## Summary
Use this command to transfer control to a line label depending on a value. This is almost like a flat
CASE command.
ON expression GOTO/GOSUB line_label_list
expression - f/c/e - Required - The value to test. Must resolve to a numeric value.
GOTO/GOSUB - Required - GOTO or GOSUB to the appropriate line.
line_label_list - label1,label2,...,labelx - Required - The list of line labels used to determine
where to transfer control. The first label corresponds to a value of 1, the second to 2,
etc. If the expression resolves to a value of 0 or greater than the number of labels -1
then control is transferred to the last label in the list. The line labels are separated by
commas.

## Signature
```
PROGRAM EDITOR
```

## Details
prg Control -> Goto/gosub -> On
