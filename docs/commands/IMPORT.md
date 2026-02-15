# IMPORT

| | |
|---|---|
| **Category** | Command |

## Description

Imports records from a file or memory into the program. When MEM is used, the command imports into an array in memory instead of a standard file. The MEM option is optional. If MEM is included in the command, number becomes the maximum number of elements in the array to be imported to. A counter_field may be supplied to pass the number of records read and the current array number to your program, and can also be used to count the number of records read. The destination is specified by to_filename/@file_number, which is the name or number of the file to be imported to. An optional scope may help determine how many records are imported; for details on the scope specifier, see the general information at the beginning of this chapter. If scope is set to N or F, scope_value specifies the number of records to be imported. You can use for_filter_expression to restrict the records imported; if the expression does not resolve to .T., the record will not be imported. The search continues until end of file and then will quit. You can perform the same task with MEM if you specify MEM, and use counter_field as the array specifier in the expression. If DISP is specified, each time a record is read the program will redisplay the screen fields; this will slow down the operation, with the slowdown depending on how many fields are displayed on the screen; the net effect will probably be negligible unless you are reading through a very large file.

## Parameters

- **`MEM`** · `flag` · *Optional*

  Optional - If you are importing to an array in memory instead of a standard file include this option in the command.

- **`number`** · `fn/v` · *Optional*

  If MEM option is included, then this is Required - If MEM is included in the command, number is the maximum number of elements in the array to be imported to.

- **`counter_field`** · `fn/v` · *Optional*

  Optional - This is an I type field that will be used for passing the number of records read, and the current array number, to your program. You can also use this to count the number of records read.

- **`to_filename/@file_number`** · `file_expr` · *Required*

  The name or number of the file to be imported to.

- **`scope`** · `option` · *Optional*

  Optional - This option may help determine how many records are imported. For more information about the scope specifier, please see the general information at the beginning of this chapter.

- **`scope_value`** · `f/c/e` · *Optional*

  Optional - If scope is set to N or F this is the number of records to be imported.

- **`for_filter_expression`** · `lexpr` · *Optional*

  Optional - You would use this option to restrict the records imported in this command. If the expression did not resolve to .T., the record would not be imported. In this option, the search continues until the program reaches the end of file and then will quit. You can also use this to perform the same task if MEM is specified. Use the counter_field as the array specifier in the expression.

- **`DISP`** · `flag` · *Optional*

  Optional - Each time a record is read the program will redisplay the screen fields, if you specify this option. This will slow down the operation of this command, with the amount of slow-down depending on how many fields are displayed on the screen. The net effect will probably be negligible unless you are reading through a very large file.

## Program Editor

`fiLe -> Mult rec cmds -> Import`

## See Also

- [EXPORT](EXPORT.md)
