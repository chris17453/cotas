# DEALLOCATE FIELD

| | |
|---|---|
| **Category** | Command |

## Description

This command will deallocate (remove from memory and allow reuse of previously allocated space) a field previously allocated using any of the allocation commands or functions.

## Syntax

```text
DEALOC fieldname
```

## Parameters

- **`fieldname`** · `fn/v` · *Required*

  The name of the field being deallocated. The field must have been allocated using one of the appropriate commands or functions.

## Comments

Once this command has been used the field being deallocated will not be able to be used in a program until it is allocated again.

## Program Editor

`Field -> Create/chg -> dEallocate`

## See Also

- [ALLOC](ALLOC.md)
- [ALOCARY()](../functions/ALOCARY.md)
