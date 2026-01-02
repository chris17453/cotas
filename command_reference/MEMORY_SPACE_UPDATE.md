# MEMORY SPACE UPDATE

## Summary
MEMORY SPACE UPDATE (DOS only)
This is a TAS Professional 3.0 command here for compatibility. There is no preferred TAS Professional 5.1 command/function.
MEM_SPC mem_area# START start_val STOP stop_val NCHR #_of_chrs DOwhat_to_do
mem_area# - f/c/e - Required - This is the memory area to use. This must resolve to a value of
1 through 4.
start_val - f/c/e - Required - The character position of the beginning of the block of characters.
The first character in the memory area is at position 1.
stop_val - f/c/e - Required - The character position of the end of the block of characters.
num_of_chrs - f/c/e - Required - Number of characters to add or subtract.
what_to_do - f/c/e - Required - This must resolve to either A (add) or S (subtract).

## Signature
```
COMMENTS
This is the equivalent of the TAS Professional 3.0 command Memory Space Update.
```

## Details
PROGRAM EDITOR
3.0 Commands -> Mem space
