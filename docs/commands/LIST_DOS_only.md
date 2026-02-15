# LIST (DOS only)

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS |

## Description

This command will list a file, or a group of array fields, to the screen, printer, or a disk file.

## Syntax

```text
LIST field_list TTL title_list MEM NUM number CNTR counter_field
FILE filename/@file_number KEY keyname/@key_number
START start_value SCOPE scope scope_value FOR for_filter_expression
WHILE while_filter_expression PTO print_to PFN print_to_file NOFF
```

## Parameters

- **`field_list`** · `f/c/e` · *Required*

  Same as LIST: This is the list of fields to be used in this command.

- **`title_list`** · `f/c/e` · *Optional*

  Same as LIST: Optional - This list will be displayed after any automatic top of form and as the first line printed by the command.

- **`MEM`** · `fn/v` · *Optional*

  Optional - If you are listing from an array in memory instead of a standard file include this option in the command.

- **`number`** · `fn/v` · *Optional*

  If MEM is set, then this is Required - If MEM is specified then this the number of elements in the array to be listed.

- **`CNTR`** · `fn/v` · *Optional*

  Optional - This is an I type field that will be used for passing the number of records read, and the current array number, to your program.

- **`counter_field`** · `fn/v` · *Optional*

  Optional - This is an I type field that will be used for passing the number of records read, and the current array number, to your program.

- **`FILE`** · `f/c/e` · *Optional*

  Optional - The name or number of the file to be used.

- **`filename/@file_number`** · `f/c/e` · *Optional*

  Optional - The name or number of the file to be used.

- **`KEY`** · `f/c/e` · *Optional*

  Optional - Set this to the appropriate value to list the records in the file in the correct order.

- **`keyname/@key_number`** · `f/c/e` · *Optional*

  Optional - Set this to the appropriate value to list the records in the file in the correct order.

- **`START`** · `f/c/e` · *Optional*

  Optional - The value to use as the beginning record.

- **`scope`** · `f/c/e` · *Optional*

  Optional - This option may help determine how many records are listed.

- **`scope_value`** · `f/c/e` · *Optional*

  Optional - If scope is set to N or F this is the number of records to listed.

- **`for_filter_expression`** · `lexpr` · *Optional*

  Optional - You would use this option to restrict the records listed in this command.

- **`while_filter_expression`** · `lexpr` · *Optional*

  Optional - This option also restricts the records listed.

- **`PTO`** · `prt_where` · *Optional*

  Optional - Use this option to direct the output of this command.
