# POP FIELD

## Summary
Restore a value in a field or group of fields that was saved previously using the PUSHF (Push Field)
command.
POPF field_list
field_list - fn/v1,fn/v2,...,fn/vx - Required - The list of fields to restore.

## Signature
```
COMMENTS
When you use the PUSHF command the field values are saved on an internal stack. This stack, just like
all others used in microcomputers, is a Last In-First Out process. That means that the last value pushed
on the stack must be the first value popped out. The TAS Professional 5.1 stack, however, is different
from a ‘normal’ stack in that it is intelligent. It knows the sizes of the values being pushed/popped and
automatically allows for the differences.
```

