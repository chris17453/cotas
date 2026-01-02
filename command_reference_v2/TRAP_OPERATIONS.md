# TRAP OPERATIONS

## Summary
This command can be used to save, restore or change the entire set of traps.
XTRAP SAVE/RSTR/CHG FLD save_to_field NOCHG/DFLT/IGNR
SAVE/RSTR/CHG - Required - SAVE - save the trap values to the save_to_field. RSTR restore the trap values from the save_to_field. CHG - change the trap values only.
save_to_field - fn/v - Optional - The total size for all traps is 1000 bytes. If you do not specify
a field, the program will allocate one during SAVE and deallocate during RSTR.
NOCHG/DFLT/IGNR - Optional - What to do with the traps. NOCHG - make no changes.
DFLT - reset traps to their default values. IGNR - reset to ignore.
NOTE: If you choose IGNR the program will set all traps to DFLT (which for most is
the equivalent of IGNR), and then the following are actually set to IGNR: F1 - F10
and ESC. This allows other keys that would be used in the normal course of operations to remain available, such as Up Arrow, Dn Arrow, Pg Up, Pg Dn, Pg_Brk, etc.
If you wish to set those to IGNR also you must do so with the TRAP command.

## Signature
```
COMMENTS
This command will allow you to treat the traps as a block. If you only need to save 2 or 3 you should
use the PUSHT (Push Trap)/POPT (Pop Trap) commands and reset them with the standard TRAP
command.
```

## Details
PROGRAM EDITOR
prg Control -> Trap; Xtrap
