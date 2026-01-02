# REMOVE ARRAY

## Summary
This command will deallocate an array previously allocated using the ALLOC (Allocate Field) command or ALOCARY() function. The memory used will be released to be used again, if desired.
REMVA array_field_name
array_field_name - fn/v - Required - The name of the field that had been previously allocated.

## Signature
```
COMMENTS
This command will not remove any fields that have been defined in the original program, only those that
have been allocated with the ALOCARY() function, or ALLOC (Allocate Field) command while
running the program.
```

## Details
PROGRAM EDITOR
Field -> Array -> Remove
