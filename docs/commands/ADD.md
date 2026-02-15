# ADD

| | |
|---|---|
| **Category** | Command |

## Description

This command will add fields to the internal field list. These fields may be a part of a file that has been opened or just for standalone purposes. This file could have been specified in the original program or could have been opened during the execution of the program. Once the field is added you can access it almost as though it had been named in the original program. An ‘F’ type pointer is returned and by using the field redirector (&) you can access that field for any purpose, including ENTER, =, etc.

## Syntax

```text
ADD field_name TYPE type SIZE display_size DEC decimal_chrs
ARRAY num_array_elements UP up_case PICT picture
FILE filename/@file_number KEY key_number OFST offset_in_file FPTR field_ptr
```

## Parameters

- **`field_name`** · `f/c/e` · *Required*

  The name of the field being added. Must conform to standard field name requirements.

- **`TYPE`** · `type` · *Required*

  TYPE type

- **`SIZE`** · `display_size` · *Required*

  display_size

- **`DEC`** · `decimal_chrs` · *Required*

  decimal_chrs

- **`ARRAY`** · `num_array_elements` · *Required*

  num_array_elements

- **`UP`** · `up_case` · *Required*

  up_case

- **`PICT`** · `picture` · *Required*

  picture

- **`FILE`** · `filename/@file_number` · *Required*

  filename/@file_number

- **`KEY`** · `key_number` · *Required*

  key_number

- **`OFST`** · `offset_in_file` · *Required*

  offset_in_file

- **`FPTR`** · `field_ptr` · *Required*

  field_ptr
