# TRIM FIELD

## Summary
This will change the leading or trailing spaces in an A type field to binary 0s.
TRIM fieldname TRAIL/LEAD
fieldname - fn/v - Required - The name of the field to be trimmed.
TRAIL/LEAD - Optional - Trim the TRAIL - trailing, or LEAD - leading spaces in the field.
Default is TRAIL.

## Signature
```
COMMENTS
When you use the function TRIM() the program puts the ‘trimmed’ field in the temporary data space. If
you are trying to TRIM a field that is larger than that space you will get the message that the program is
out of room in the temporary data area. This command is provided to you so that you can TRIM a field
larger than the temporary data area within its own space. Using this command you will not run out of
memory.
```

## Details
PROGRAM EDITOR
Field -> alpha Fld cmds -> Trim
