# FILL MEMORY AREA

## Summary
This is a TAS Professional 3.0 command here for compatibility. There is no preferred TAS Professional 5.1 command/function.
FILLMEM mem_area# START start_val NCHR num_chrs CHR chr_to_use
mem_area# - f/c/e - Required - This is the memory area to fill. This must resolve to a value of
1 through 4.
start_val - f/c/e - Required - The starting character in the memory area. The first character is
at position 1.
num_chrs - f/c/e - Required - The number of characters to fill.
chr_to_use - f/c/e - Required - What to use as a fill character.

## Signature
```
COMMENTS
This is the equivalent of the TAS Professional 3.0 command Fill Memory.
```

## Details
PROGRAM EDITOR
3.0 Commands -> Fillmem
FILTER (DOS only)
This command will set an expression as a for_filter to be used during a file search unless overridden in
the command. This will affect all searches from the keyboard.
FILTER expression
expression - lexpr - Required - This is the expression to test against. It will literally test the
expression against each record found. If it resolves to .F., the program will search for
the next record and will continue until it reaches the end of the file. Unlike a normal
search if the program reaches the beginning or end of the file the record will be
cleared.
COMMENTS
This is a very powerful feature of TAS Professional 5.1. Through the use of this command you are able
to change the ‘look’ of the file at runtime, on the fly, with no changes to your program. We use this
feature in the Maintain Database and Export and Import utilities. Also, any programs you generate from
