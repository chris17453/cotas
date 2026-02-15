# INCREMENT

| | |
|---|---|
| **Category** | Command |

## Description

Increment an I or R type field.

## Syntax

```text
INC fieldname
```

## Parameters

- **`fieldname`** · `fn/v` · *Required*

  The name of the field being incremented. Must be of I or R type.

## Comments

This is the same as adding 1 to the field. That is:
INC CNTR
is equivalent to:
CNTR = CNTR + 1
