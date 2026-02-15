# READ ARRAY

| | |
|---|---|
| **Category** | Command |

## Description



## Parameters

- **`to_field_list`** · `fn/v1, fn/v1,..., fn/vx` · *Required*

  These are the recipient fields. They must be array fields of the same type as the respective from_field. The first from_field_list field is paired with the first to_field_list field, the second to the second, etc. There must be the same number of to_field_list fields as from_field_list fields.

- **`filename/@file_number`** · `file_expr` · *Optional*

  The name or number of the file to be used. If you do not include this option, the program will look for a default search file set in the SRCH (Search File) command. If a default file hasn’t been previously set the program will report an error and the command will be skipped. This entry will override any default value set in the SRCH command.

- **`keyname/@key_number`** · `key_expr` · *Optional*

  Set this to the appropriate value to read the records in the file in a particular order. If you do not include this option, the program will look for a default key set in the SRCH (Search File) command. If a default key hasn’t been previously set the program will report an error and the command will be skipped. This entry will override any default value set in the SRCH command.

- **`start_value`** · `f/c/e1,f/c/e2,...,f/c/ex` · *Optional*

  The value to use as the beginning record. This is similar to doing a FIND before the command. If the index you’re searching on has multiple segments you may list the proper values for each of the segments, separating them with commas. This will allow you to specify the exact type for each of the segments. For example, suppose you’re searching on an index that has two segments: a 5 character alpha and an I type field. Each record you want has the same alpha but the I field will change, and is in order. In the first record the I type field has a value of 0. The following would find the first record for that group, if it exists: start ‘ABCDE’,0! Notice that we separate the values with a comma. The only requirement is that the values be of the same type (and size) as the segments in the key. In this case we put the I type constant specifier (!) after the 0 to make sure that it is passed as an I type field.

- **`scope`** · `unknown` · *Optional*

  This option may help determine how many records are read. For more information about the scope specifier please see the general information at the beginning of this chapter.

- **`scope_value`** · `f/c/e` · *Optional*

  If scope is set to N or F this is the number of records to read.

- **`for_filter_expression`** · `lexpr` · *Optional*

  You would use this option to restrict the records read in this command. If the expression did not resolve to .T. the record would not be read, and therefore would not be added to the array. In this option, the search continues until the program reaches the end of file and then will quit.

- **`while_filter_expression`** · `lexpr` · *Optional*

  This option also restricts the records read. However, the first time the expression resolves to .F., the program stops reading records and continues to the next command. Thus if the program is reading records in a certain order (by a specific key) and the expression returns .F., then the program knows that all the appropriate records have been read and there is no need to continue reading. For example: Suppose you want to read all the invoice records for a specific customer. The first record for that customer is found in the invoice file either by using the start_value option within this command, or through the FIND command before executing this command. Then the while_filter_expression would be: INV_CUST_CODE = CUSTOMER_CODE
  (This assumes that there is a field called INV_CUST_CODE in the invoice file and a similar field in the customer file called CUSTOMER_CODE. The keyname/@key_number option must be set to the proper value so that the records are read in CUSTOMER_CODE order, or you must have set the search key using SRCH previous to this command.) When the first invoice record for the next customer is read the program will stop reading records.
  NOTE: If there is no start_value you must set the scope value to N xxx. If this is not done, the program will find the first record in the file and the while_filter_expression option will probably fail the first time. However, if the start_value option is used you can ignore this requirement.

- **`counter_field`** · `fn/v` · *Optional*

  This is an I type field that will be used for passing the number of records read, and the current array number, to your program. You can also use this to count the number of records read.

- **`DISP`** · `NY` · *Optional*

  Each time a record is read the program will redisplay the screen fields, if you specify this option. This will slow down the operation of this command, with the amount of slow-down depending on how many fields are displayed on the screen. The net effect will probably be negligible unless you are reading through a very large file.

## Comments

If the while_filter_expression option is used you must be careful to set scope and/or the start_value options properly. If you think you've set everything properly and yet no records are being read, make sure that the proper first record is in memory prior to the execution of this command or that correct use has been made of the start_value option. The last record read before exiting the command is still in the record buffer after the program leaves this command.
You must make sure you won’t exceed the number of elements in the array. If there is a chance this might happen, you should use the scope to insure this doesn’t happen. For example:
... scope n 300 ...
This would limit the command to the next 300 records in the file.

## Sample Program

`RDARRAY`

## Program Editor

`fiLe -> Mult Rec Cmds -> Read Array`

## See Also

- [WRTA](WRTA.md)
