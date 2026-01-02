# ALOCARY()

## Summary
The number of elements to allocate. If there isn’t enough available memory for the
number of elements desired the program will allocate as many as possible. The maximum
number of array elements for any single field is 65,535.

## Signature
```
RETURN TYPE
I The actual number of elements allocated.
```

## Details
COMMENTS
The field (part 1) used in this command cannot be a part of a standard TAS Professional 5.1 file. It
must have been created using the DEFINE command.
Once this function has been used to allocate the array the data space originally used by the field is lost
to use. Due to this you should define the field with no elements initially. This keeps the amount of
wasted space to a minimum.
