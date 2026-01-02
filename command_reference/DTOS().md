# DTOS()

## Summary
PURPOSE
This function will return a date value as a string in the form YYYYMMDD.

## Signature
```
PARTS
1
```

## Details
f/c/e
Date value to use.
RETURN TYPE
A An 8 character string is always returned.
EXAMPLE
? dtos(date())
19940623
EDIT(1) (Windows Only)
PURPOSE
This function will edit an existing note or create a new one. It handles word wrap automatically and
provides all the normal Windows editing capabilities including marking a block of text, saving that
block to the clipboard, and inserting a block of text from the clipboard to the note.
PARTS
1
fn/v
Field name that contains the current note or will contain the new one.
RETURN TYPE
L If the user presses the SAVE button (or F10) then .T. will be returned. If the user presses the ESC
key or button then .F. will be returned.
COMMENTS
This function replaces the TASEDIT.RUN program in DOS. It expects to have a field
that contains the text for the entire note. CR/LFs are ok and are preserved by the
function. Before you call this function be sure to create a window where the editing
will take place. If you haven't created a window the function will use the entire base
Function Reference
