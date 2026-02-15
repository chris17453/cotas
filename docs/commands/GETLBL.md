# GETLBL

| | |
|---|---|
| **Category** | Command |
| **Platform** | TAS Professional 5.1 |

## Description

This command will return the line number of a label for future use in the GOTOL (Goto Line) or GOSUBL (Gosub Line) command.

## Syntax

```text
GETLBL label_name FLD receiving_field
```

## Parameters

- **`label_name`** · `label` · *Required*

  The label name you want to convert to a line number.

- **`receiving_field`** · `fn/v` · *Required*

  The field that will receive the line number. Must be of type I.
