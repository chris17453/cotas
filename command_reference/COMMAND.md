# COMMAND

## Summary
This will define the beginning of a User Defined Command (UDC).
CMD command_name command_parameters
command_name - sac - Required - This is a special alpha constant. It is the name you give to
this command. It may be up to 14 characters long. It must not be the same as any line
label used anywhere else in the program.
command_parameters - fn/v1,fn/v2,...,fn/vx - Optional - The values being passed to the UDC.
A maximum of 20 fields may be specified. The field names are separated with
commas.

## Signature
```
COMMENTS
You must include the compiler directive #UDC or #UDX before the first user defined command is
referenced in the program.
```

## Details
PROGRAM EDITOR
System -> Programming -> udC
