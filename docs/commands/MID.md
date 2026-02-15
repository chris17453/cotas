# MID

| | |
|---|---|
| **Category** | Command |

## Description

This command will either overwrite a field or insert a field into another field.

## Syntax

```text
MID receiving_field START starting_character NCHR number_of_characters
FLD characters_to_add MEM_AREA mem_area# INS
```

## Parameters

- **`receiving_field`** · `fn/v` · *Required*

  The receiving field. Must be of type A.

- **`START`** · `f/c/e` · *Optional*

  Where to start adding the new characters. The first character in the receiving_field is character 1. If no value is given the value will default to 1.

- **`NCHR`** · `f/c/e` · *Optional*

  The number of characters to add. If no value is given the program will use the internal size of the characters_to_add field.

- **`characters_to_add`** · `f/c/e` · *Required*

  The characters that will be put into the receiving field. Any type of field may be used; however, the field is not converted to type A. It will be added in its natural form.

- **`mem_area#`** · `f/c/e` · *Optional*

  If you are actually getting the characters from a memory area then this is the number (from 1-4). NOTE: Even if you want to use a memory area you must still specify a characters_to_add field, although it will be ignored during execution of the command.

- **`INS`** · `flag` · *Optional*

  If this option is specified the program will insert the characters, moving the current characters to the right starting with the character at the start location. The characters at the end of the field, if any, will be deleted. The default is to overwrite the characters at the current location.
