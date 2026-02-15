# DEFINE FIELD

| | |
|---|---|
| **Category** | Command |

## Description

This command is used to ‘create’ a field that will be used only within the current program (or subsequent programs that are called from this program). This is not a field that is part of a standard TAS Professional 5.1 file.

## Syntax

```text
DEFINE FIELD
DEFINE field_name_list TYPE type SIZE display_size DEC decimal_chrs
ARRAY num_array_elements PICT picture DUP dup_field UP RESET LOCAL INIT
init_val
```

## Parameters

- **`field_name_list`** · `sac` · *Required*

  The name or names of the field(s) being defined. Must conform to standard field name requirements. You may define a maximum of 10 fields in one DEFINE command. Each of the fields will have the same specifications.

- **`TYPE`** · `sac` · *Optional*

  Optional - The type of the field being added. Must conform to standard field type requirements. If you don’t specify a field type, the default value is N. The field name types are:
  A Alphanumeric
  N Numeric
  D Date
  T Time
  I Integer
  B Byte
  F F type pointer
  P P type pointer
  R Record number

- **`SIZE`** · `display_size` · *Optional*

  The size of the field being defined.

- **`DEC`** · `sac` · *Optional*

  The number of decimal characters to allocate for the field.

- **`decimal_chrs`** · `n` · *Optional*

  The number of decimal characters to display (used with numeric fields).

- **`ARRAY`** · `sac` · *Optional*

  ARRAY num_array_elements - The array feature allowing multiple elements of the field.

- **`num_array_elements`** · `n` · *Optional*

  The number of elements in the array.

- **`PICT`** · `sac` · *Optional*

  PICT picture - The editing picture for the field.

- **`picture`** · `sac` · *Optional*

  The PICTURE specification for the field.

- **`DUP`** · `sac` · *Optional*

  DUP dup_field - Duplicate the field for alignment.

- **`dup_field`** · `sac` · *Optional*

  The field to be duplicated.

- **`UP`** · `sac` · *Optional*

  UP - Indicates the field or operation involves upper-level or upward referencing.

- **`RESET`** · `sac` · *Optional*

  RESET - Cares the reset of the field.

- **`LOCAL`** · `sac` · *Optional*

  LOCAL - Local field scope.

- **`INIT`** · `sac` · *Optional*

  INIT - Initialization for the field.

- **`init_val`** · `sac` · *Optional*

  init_val - The initial value for the field.
