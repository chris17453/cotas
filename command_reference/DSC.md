# DSC

## Summary
sort_field_expr - fn/v/e - Required - The field to be sorted. This can also be an expression
containing several fields. The counter_field must be used as the array element
specifier in the field or expression. For example:
cust_state[cntr]+cust_zip[cntr]
or
cust_state[cntr]
number_of_elements - f/c/e - Required - The number of elements to sort. The actual maximum number of array elements must be at least number_of_elements+1.
move_list - fn/v1, fn/v2,..., fn/vx - Required - A list of array fields to be moved along with the
field to be sorted. You must include at least the sort field itself. You may also include
any other array fields that you want to be ordered in the same fashion as the sort field.
The counter_field must be specified as the array element in these fields. For example:
cust_state[cntr],cust_zip[cntr],cust_sales[cntr],etc...
counter_field - fn/v - Required - This is the field that you will use in the command as the array
specifier. It must be of type I.

