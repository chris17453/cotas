# REMOVE ARRAY

| | |
|---|---|
| **Category** | Command |
| **Platform** | PROGRAM EDITOR |

## Description

This command will deallocate an array previously allocated using the ALLOC (Allocate Field) command or ALOCARY() function. The memory used will be released to be used again, if desired.

## Syntax

```text
REMVA array_field_name
```

## Parameters

- **`array_field_name`** · `fn/v` · *Required*

  The name of the field that had been previously allocated.

## Comments

This command will not remove any fields that have been defined in the original program, only those that
have been allocated with the ALOCARY() function, or ALLOC (Allocate Field) command while
running the program.

## Program Editor

`Field -> Array -> Remove`
