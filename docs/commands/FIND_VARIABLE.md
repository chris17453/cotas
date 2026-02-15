# FIND VARIABLE

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to find a record in any file, either TAS (opened with the OPENV (Open Variable) command) or non-TAS.

## Syntax

```text
FINDV find_type FNUM file_number KEY keyname/@key_number VAL search_value ERR error_label FOR for_filter_expression NLOCK KEYO NOCLR
```

## Parameters

- **`find_type`** · `mgflnp` · *Required*

  mgflnp - Required - This is the type of find you want to execute. The options are: M - matching - Find the exact record based on the search_value option. You can also set the appropriate key fields with the EQUAL command and then the search_value option need not be used.

- **`FNUM`** · `string` · *Required*

  

- **`file_number`** · `n` · *Required*

  

- **`KEY`** · `sac` · *Optional*

  

- **`keyname`** · `sac` · *Optional*

  

- **`@key_number`** · `n` · *Optional*

  

- **`VAL`** · `v` · *Optional*

  

- **`search_value`** · `v` · *Optional*

  

- **`ERR`** · `string` · *Optional*

  

- **`error_label`** · `string` · *Optional*

  

- **`FOR`** · `string` · *Optional*

  

- **`for_filter_expression`** · `lexpr` · *Optional*

  for_filter_expression - lexpr - Optional - You would use this option to restrict the records read in this command. If the expression did not resolve to .T. the record would not be found. In this option, the search continues until the program reaches the end of file (EOF = .T.) and then will quit.

- **`NLOCK`** · `option` · *Optional*

  NLOCK - Optional - If this option is included in the command the program will NOT lock the record upon finding it as it would normally do in a multi-user situation. The default operation is to place a lock on the record upon reading it.

- **`KEYO`** · `option` · *Optional*

  KEYO - Optional - If this option is included in the command the program will search only for the key. The result is that you can use this method for checking if a key value exists without having to find the entire record. This will preserve the values in your record until you are ready to save it. To test whether or not the record exists use the FLERR() function without specifying the file number.

- **`NOCLR`** · `option` · *Optional*

  NOCLR - Optional - Normally, during a M (match) type FIND, if a record is not found the program will clear the record buffer. If you don’t want this to happen then include this option. If a record was active before the FIND it will still be active. If you had data as part of a new record then that data will still be there.
