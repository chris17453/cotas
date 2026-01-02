# DECREMENT

## Summary
Decrement an I or R type field.
DEC fieldname
fieldname - fn/v - Required - The name of the field being decremented. Must be of I or R type.

## Signature
```
COMMENTS
This is the same as subtracting 1 from the field. For example:
DEC CNTR is equivalent to:
CNTR = CNTR - 1
This command provides a more succinct method to subtract 1 from a field. This command also runs a
little faster than the standard subtraction routine.
```

## Details
PROGRAM EDITOR
Field -> Decrement
