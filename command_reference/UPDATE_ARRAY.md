# UPDATE ARRAY

## Summary
This command is used to change all or several of the values in an array or group of arrays. It will
quickly and efficiently add or subtract values, or insert or delete a specific element number in an array.
UPDTA array_field_list INS/DEL/ADD/SUB/CLR TIMES times VAL value
array_field_list - fn/v1, fn/v2,..., fn/vx - Required - The names of the array fields to be
changed. You may set the array element spec to the first element number to be
updated, or in the case of INS/DEL, the location to start the insertion/deletion.
INS/DEL/ADD/SUB/CLR - Required - Options include:
ADD - Add a value to an array of values. The type of the value being added needs to
be the same as the type of the array field(s). The value specified will added the
number of times specified, starting with the array element indicated in
array_field_list. For example, if the following were true:
Element #
————
1
2
3
4
5

