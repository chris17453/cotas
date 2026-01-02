# INKEY(1)

## Summary
PURPOSE
This function will check the keyboard and see if a character is waiting.

## Signature
```
PARTS
1
```

## Details
f/c/e
Normally, this function would just check the keyboard and then return. However, if you
want the program to wait until a key is pressed then ‘W’ must be included here.
RETURN TYPE
I If you have not included the wait (‘W’) option and no character is waiting at the keyboard the value
returned will be 0.
COMMENTS
If a character is waiting or one is entered, the ASCII representation value will be returned.
Once a key is pressed it will stay in the keyboard buffer until it is accessed by some procedure. The
procedure could be a function such as this one or the normal TAS Professional 5.1 process, which
checks the keyboard before each command is executed.
EXAMPLE
? inkey()
0
&& no character was waiting
(This will return as soon as the keyboard is checked.)
? inkey(‘w’)
32
&& space bar was pressed
(This will return after a key was pressed.)
