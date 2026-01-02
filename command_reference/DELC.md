# DELC

## Summary
INTERRUPT (DOS only)
Use this command to call any DOS, BIOS, etc. interrupt directly from TAS Professional 5.1.
INT int_num REGS register_field
int_num - f/c/e - Required - This is the decimal form of the interrupt number. Make sure
you’re using the decimal and not hex representation.
register_field - fn/v - Required - The first field in the register list.

## Signature
```
COMMENTS
the rest. This is like writing in assembly language without having to worry about all the problems you
typically encounter when writing in assembly. You can access many things in Netware, or
LANTASTIC, etc. without having to write outside programs.
A word of warning: if you don’t understand everything about the interrupt you’re using, don’t try this.
You can easily shut down the system using this feature. It is provided so that skilled programmers can
more easily and quickly access features they couldn’t before.
```

