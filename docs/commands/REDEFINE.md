# REDEFINE

| | |
|---|---|
| **Category** | Command |
| **Platform** | TAS Professional 5.1 |

## Description



## Syntax

```text
REDEFINE
```

## Parameters

- **`fieldname`** · `fn/v` · *Required*

  The name of the field being redefined. Must be a ‘standard’ TAS Professional 5.1 field name.

- **`type`** · `f/c/e` · *Optional*

  The new type value. May be:
  Name
  —————————
  A Alphanumeric
  N Numeric
  D Date
  T Time
  I Integer
  B Byte
  F F type pointer
  P P type pointer
  R Record number
  L Logical
  O TAS Pro 3.0 BCD
  
  Internal size
  ———————————————
  maximum 4 gbytes
  8 bytes
  4 bytes
  4 bytes
  2 bytes
  1 byte
  5 bytes
  14 bytes
  4 bytes
  1 bytes
  max of 10 bytes

- **`size`** · `f/c/e` · *Optional*

  The new display size. If the type is A then this is required and is both the display and internal size.

- **`decimal_characters`** · `f/c/e` · *Optional*

  If the type is N then you can also define the number of decimal characters. The maximum number is 8 and must be at least 2 less than the size value.

- **`filename/@file_number`** · `file_expr` · *Optional*

  If the field being redefined is part of a TAS Professional 5.1 file then this is the name or number of that file. The file must be opened before a field can be redefined.

- **`offset_in_file`** · `f/c/e` · *Optional*

  If the field being redefined is part of a TAS Professional 5.1 file then you can specify the new offset within the file buffer. The first character of the record is number 0.

- **`key_number`** · `f/c/e` · *Optional*

  If this field is a key you can specify the key number here. Then if this field is used in the ENTER command the user will be able to search for records using the file search keys (F5-F9).
  NOTE: If the SRCH (Set Search File) command has been executed setting a specific search file and key it will override any other key fields.

- **`picture`** · `f/c/e` · *Optional*

  The new picture value to be used with this field. If this is a type ‘A’ field, you can use this option to enter a field into a space that is shorter than the defined length of the field. This is called a slider field. A slider is a field that shows fewer characters on the screen than actually exist in the field. The user can see the other characters by pressing the LEFT and RIGHT ARROW keys and the characters will appear to ‘slide’ back and forth across the available space. This is very useful when you have several large fields and want them all to fit on the same screen. A slider field is specified with an alpha constant of "Sxx", where xx is the number of columns to display.

- **`set_up_case`** · `f/c/e` · *Optional*

  This value must resolve to Y or N. If the value is Y, then any entries made to this field will be forced to upper case characters.

- **`location`** · `fn/v` · *Optional*

  If you specify a fieldname here, the program will use the location (offset) of that field for this one.

## Comments

The original size (internal) of the field being redefined must be enough to cover the size of the new field.
The program does not check that this is true, and you can end up crossing from one field to another.
The program does not check that this is true, and you can end up crossing from one field to another.
The program does not check that this is true, and you can end up crossing from one field to another.
The program does not check that this is true, and you can end up crossing from one field to another.
You should consider using the ADD command before using this one. It is much more flexible and doesn’t have the restrictions on original size.

## Sample Program

`REDEFINE`

## Program Editor

`Field -> Create/chg -> Redefine`

## See Also

- [DEFINE](DEFINE.md)
