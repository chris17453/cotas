# LOC()

## Summary
COMMENTS
If you are using a field as the search for value (part 1) make sure that you TRIM() the value first. This
can be done within the LOC(). If you don’t, the function will try to match all trailing spaces also.
Obviously, if you are looking for the exact match, spaces and all, don’t TRIM() it.

## Signature
```
EXAMPLE
x = ‘ABCDEFGHIJ’
? loc(‘CD’,x)
3
? loc(‘CD’,x,4,5)
0
define srch_fld type a size 10
srch_fld=’CD’
? loc(trim(srch_fld,’t’),x)
3
```

