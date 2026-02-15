# LABEL

| | |
|---|---|
| **Category** | Command |
| **Platform** | PROGRAM EDITOR |

## Description

Specify a label name.

## Syntax

```text
label_name:
```

## Parameters

- **`label_name`** · `sac` · *Required*

  label - Required - The name of the label. Cannot have been used as a label anywhere else in the program.

## Comments

The label must be the first characters on the line, must be no more than 14 characters long, and must be terminated with a colon (:). Also, there can be no other commands on the same line.

## Program Editor

`Prg control -> Line label`
