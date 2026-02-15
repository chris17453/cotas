# REWRAP

| | |
|---|---|
| **Category** | Command |

## Description

This command will re-word wrap a field that has been previously set up with the WRAP command.

## Syntax

```text
REWRAP starting_line_number
```

## Parameters

- **`starting_line_number`** · `f/c/e` · *Required*

  starting_line_number - f/c/e - Required - The line number at which to start the word wrap

## Comments

By starting at a different line than the first you can speed up the word wrap process. Also, the program already knows what the field is, where it’s located, and has done all the preliminary work which also speeds up the process.

## Program Editor

`User interface -> rewrAp`

## See Also

- [WRAP](WRAP.md)
- [WRAP()](../functions/WRAP.md)
- [WRAPO()](../functions/WRAPO.md)
- [WRAPL()](../functions/WRAPL.md)
- [WRAPS()](../functions/WRAPS.md)
