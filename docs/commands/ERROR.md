# ERROR

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to display an error message at the standard message location. You can also set the internal System Error Number field.

## Syntax

```text
ERR message NUM number
```

## Parameters

- **`message`** · `f/c/e` · *Required*

  You may use this in two ways. It can be set as an A type field and the program will display the field as the error message. It can also be set as a numeric value (any appropriate type) and the program will read the appropriate record in the ERRMSG.B file. The value set by you must relate to the ERROR_NUM field in the ERRMSG record. For more information please see Chapter 3, Main Menu.

- **`number`** · `f/c/e` · *Optional*

  If this value is set it will be saved in the internal System Error Number field. You can access this value elsewhere in the program through the PERR() function.

## Comments

By using the ERRMSG.B file as the storage for error messages you can easily add, change, delete, etc. any appropriate message. You should start any message numbers at 5000 or more. This will assure that no messages will have to change due to interference with records saved by CAS and provided with the system. Obviously, the error number values don’t have to be sequential.

## See Also

- [PERR()](../functions/PERR.md)
