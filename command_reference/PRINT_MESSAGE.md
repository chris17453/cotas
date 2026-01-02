# PRINT MESSAGE

## Summary
The command will allow you to send a message, data, or combination of both to the SCREEN, PRINTER
or default device.
PMSG message_list AT starting_column,starting_row WAIT NOCR PTW print_to
ENTER enter_field COLOR color ABS
message_list - f/c/e1, f/c/e2,..., f/c/ex - Required - Any combination of constants, expressions,
and or field values. These will print in a continuous line. For example:
PMSG ‘ABC’,’DEF’,X
assuming X = ‘GHI’, will display:

