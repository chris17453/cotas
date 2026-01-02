# FILL

## Summary
Use this command to put a single character multiple times into a single field or multiple array elements
of the same field.
FILL fieldname TRAIL/LEAD/ALL CHR character_to_use TIMES times
fieldname - fn/v - Required - The field to be filled. May not be a pointer (P or F), but any
other type is legal. You can use the redirector and a pointer to point to a legal field.
TRAIL/LEAD/ALL - Optional - If the field being filled is of A type you may set whether to fill
the trailing (TRAIL) or leading (LEAD) characters, or all (ALL). The program will put
the character_to_use in each receiving position starting with the last (and then
decrementing) or the first (and then incrementing) until the program finds the first
character with an ASCII value greater than a space (ASCII 32). In the case of ALL the
program will replace all characters in the field.
NOTE: All displayable characters are greater than a space. The default value is

