# EQUAL (=)

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to set the receiving field to the value of an expression.

## Syntax

```text
receiving_field = expression
```

## Parameters

- **`receiving_field`** · `fn/v` · *Required*

  The field that is going to be changed.

- **`expression`** · `f/c/e` · *Required*

  The expression that is providing the data. This can be as simple as a numeric constant, 1, or a very complex expression that uses functions and UDFs.

## Comments

If the two field types are not the same the program will attempt to convert the expression to the type of the receiving_field.

## Program Editor

`Field -> Equal`
