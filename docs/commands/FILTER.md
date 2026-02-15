# FILTER

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS |

## Description

This command will set an expression as a for_filter to be used during a file search unless overridden in the command. This will affect all searches from the keyboard.

## Syntax

```text
FILTER expression
```

## Parameters

- **`expression`** · `lexpr` · *Required*

  This is the expression to test against. It will literally test the expression against each record found. If it resolves to .F., the program will search for the next record and will continue until it reaches the end of the file. Unlike a normal search if the program reaches the beginning or end of the file the record will be cleared.

## Comments

This is a very powerful feature of TAS Professional 5.1. Through the use of this command you are able to change the ‘look’ of the file at runtime, on the fly, with no changes to your program. We use this feature in the Maintain Database and Export and Import utilities. Also, any programs you generate from
