# PARAMETER

| | |
|---|---|
| **Category** | Command |

## Description

This is the command used to ‘receive’ values from a previous program in which the with option was set in the CHAIN command.

## Syntax

```text
PARAM field_list
```

## Parameters

- **`field_list`** · `fn/v` · *Required*

  The names of the fields that will be used to receive the values from the with option in the CHAIN command. If the receiving field type does not match the type of the ‘giving’ field, the value will be converted to the receiving field type. Fields are separated by commas.

## Comments

If this program is run directly or there are no values to pass, the fields in the PARAM command will remain blank.

## Program Editor

`System -> Other programs -> Parameters`

## See Also

- [CHAIN](CHAIN.md)
- [RAP](RAP.md)
