# POINTER

## Summary
You can use this command to get the internal ‘pointer’ value for the field.
receiving_field -> target_field
receiving_field - fn/v - Required - The receiving field. The internal ‘pointer’ value for
target_field will be saved into receiving_field. If you are saving the pointer for a
non-array field it may be saved into either a type F or P field. However, if the pointer
is for an array field, the result must be saved into a type P field or an inaccurate value
will be saved.
-> - Required - The pointer command symbol.
target_field - fn/v - Required - The field being pointed at.

## Signature
```
COMMENTS
The difference in receiving fields comes from the way fields are accessed in TAS Professional 5.1. An
F type pointer keeps track of the field type and address in the field list buffer. Only 5 bytes are required
for this type of field. The P type pointer uses the actual location in memory and keeps track of the type,
size, decimal characters, etc. of the field being pointed to. The P type is necessary when pointing to an
array field since the array value is not kept in an F type pointer. For more information about the layout,
```

