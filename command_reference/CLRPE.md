# CLRPE

## Summary
No options

## Signature
```
COMMENTS
You would use this command when making calls to a routine that may or may not fail. This is the only
way to clear the error number before calling the routine again. Also, the program error holder is not
reset when the program returns to the calling (previous) program. This allows you to check if an error
occurred in the chained program. Use the ERR() function to check for an error value.
```

## Details
PROGRAM EDITOR
User interface -> Messages -> Clear prg error
