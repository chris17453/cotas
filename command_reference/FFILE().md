# FFILE()

## Summary
FFILE(1,2)
PURPOSE
This function searches for and returns the name of a file if found.

## Signature
```
PARTS
1
```

## Details
f/c/e
The file to search for. You may use the standard DOS file search characters of * and ? in
the same manner as they would be used searching at the DOS prompt.
2
f/c/e
Whether to search for the first occurrence of the file or the next. ‘F’ searches for the first,
‘N’ for the next.
RETURN TYPE
A Returns the file name if found. If the file is not found a blank field (“”) will be returned. If the
receiving field is too short the returning value will be truncated.
COMMENTS
You can search for an entire group of files that match a certain pattern. The first time you search part 2
must be set to ‘F’. For any searches after that, part 2 must be set to ‘N’. When no more matches are
found the file name returned will be blank (“”). Part 1 is not consequential when Part 2 is set to ‘N’.
However, a null field (“”) at least must be included as a space keeper.
You must include the path if the file being searched for isn’t in the current path. However, the file
name returned will not include any path value, only the name and extension.
EXAMPLE
? FFILE(‘D*.*’,’F’)
DICTDAT.B or DATABASE.DAT etc.
? FFILE(‘D??DAT.B’,’F’)
DBCDAT.B or D12DAT.B etc.
FFLD(1,2)
PURPOSE
This function will return the pointer (P or F) for a field. The function can also return a logical value to
indicate that the field was not found.
PARTS
1
f/c/e
The name or number of the field to search for.
2
f/c/e
What to return. ‘P’ returns a P type pointer, ‘F’ returns an F type pointer, and ‘L’ returns
a logical value.
