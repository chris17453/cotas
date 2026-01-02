# TRANSACTION

## Summary
Using this command you can be assured that an entire set of file updates will be completed. Otherwise,
they can be rolled back to an original state.
TRANSX what_to_do ERR error_field
what_to_do - BCR - Required - B - Begin; this option is used at the beginning of the transaction. C - Commit; will finalize the transaction and permanently update the files. R Rollback; will return the files to the state before the original Begin command.
error_field - fn/v - Required -You must provide a field that will be used to receive any error
number provided by the routine. If the command step is successful the value returned
is 0. This field must be of type I.

## Signature
```
COMMENTS
The correct transaction process is:
```

