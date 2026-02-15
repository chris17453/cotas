# EXPORT

| | |
|---|---|
| **Category** | Command |

## Description



## Parameters

- **`scope`** · `f/c/e` · *Optional*

  This option may help determine how many records are exported. For more information about the scope specifier, please see the general information at the beginning of this chapter.

- **`scope_value`** · `f/c/e` · *Optional*

  If scope is set to N or F this is the number of records to exported.

- **`number`** · `fn/v` · *Required*

  If MEM is set then this is Required - If MEM is set, this is the number of elements in the array to be exported.

- **`counter_field`** · `fn/v` · *Optional*

  This is an I type field that will be used for passing the number of records read, and the current array number, to your program. You can also use this to count the number of records read.

- **`for_filter_expression`** · `lexpr` · *Optional*

  You would use this option to restrict the records exported in this command. If the expression did not resolve to .T. the record would not be exported. In this option, the search continues until the program reaches the end of file and then will quit. You can also use this to perform the same task if the MEM option is set. Use the counter_field as the array specifier in the expression.

- **`while_filter_expression`** · `lexpr` · *Optional*

  This option also restricts the records exported. However, the first time the expression resolves to .F., the program stops reading records and continues to the next command. Therefore, if the program is reading records in a certain order (by a specific key) and the expression returns .F., then the program knows that all the appropriate records have been read and there is no need to continue reading. For example: You want to export all the invoice records for a specific customer. The first record for that customer is found in the invoice file either by using the start_value option within this command, or through the FIND command before executing this command. Then the while_filter_expression would be: INV_CUST_CODE = CUSTOMER_CODE (This assumes that there is a field called INV_CUST_CODE in the invoice file and a similar field in the customer file called CUSTOMER_CODE. The keyname/ @key_number option must be set to the proper value so that the records are read in CUSTOMER_CODE order, or you must have executed the SRCH command previous to this command.) When the first invoice record for the next customer is read the program will stop reading records. NOTE: If there is no start_value you must set the scope value to R or N xxx. If this is not done, the program will find the first record in the file and the while_filter_expression option will probably fail the first time. However, if the start_value option is used you can ignore this requirement.

- **`to_file`** · `f/c/e` · *Required*

  The name of the file you are exporting to. Must include the entire path, if any, and any extension. The program will use the exact name you enter.

- **`file_type`** · `f/c/e` · *Required*

  The type of file you are exporting to. The options are:
  D - dBASE III+
  L - delimited
  F - fixed length/SDF
  Y - SYLK
  I - DIF
  X - text

- **`delimiter_character`** · `f/c/e` · *Optional*

  If you choose the delimited (‘L’) type of file, you can also choose a delimiter character. If you don’t specify this value the program will put commas (,) between each field and surround alpha fields with quotes (“xxx”).

- **`APND`** · `'Y' or 'N'` · *Optional*

  Optional - Normally the EXPORT command will create a new file each time it is run. However, you may choose to add records to an existing file by selecting this option. The full option is APND 'Y' to append records to a current file and APND 'N' to create a new file. NOTE: The program doesn’t check to make sure that the previous records are in the same format.

- **`DISP`** · `flag` · *Optional*

  Optional - Each time a record is read the program will redisplay the screen fields, if you specify this option. This will slow down the operation of this command, the amount of slow down depending on how many fields are displayed on the screen. The net effect will probably be negligible unless you are reading through a very large file.

## Comments

If the while_filter_expression option is used you must watch out for the proper setting of the scope, and/or start_value options also. If you think everything is set properly and yet no records are being exported, make sure that the proper first record is in memory prior to the execution of this command or that correct use has been made of the start_value option. The last record read before exiting the command is still in the record buffer after the program leaves this command.

## Program Editor

`fiLe -> Mult rec cmds -> Export`

## See Also

- [IMPORT](IMPORT.md)
