# PUSHT

## Summary
PORT (DOS only)
Use this command to output a byte or string of bytes to a specific computer port.
PORT portnum FLD giving/receiving_fld IN/OUT
portnum - f/c/e - Required - The actual computer port number. Normally port numbers are in
hex form, this must be in decimal, i.e., port 200h is 512 decimal.
giving/receiving_fld - fn/v - Required - The field that contains the value to be output to the
port or input from the port. If you are using a 16550 UART you can have a FLD
buffer of more than 1 character, otherwise, you should use get/receive only one
character at a time. This command will attempt to get/send the same number of
characters as the size of this field.
IN/OUT - Required - IN gets characters from the port, OUT sends characters out the port.

## Signature
```
PROGRAM EDITOR
```

## Details
System -> poRt
