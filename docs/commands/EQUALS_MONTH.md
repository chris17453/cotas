# EQUALS MONTH

| | |
|---|---|
| **Category** | Command |

## Description

This is a TAS Professional 3.0 command here for compatibility. The preferred method is to use the =MNTH() function for the month number, or =CMNTH() for alphanumeric month.

## Syntax

```text
EQU_XMT recv_field FLD date_field
```

## Parameters

- **`recv_field`** · `fn/v` · *Required*

  The field that is going to receive the month value. If the receiving field is A type then the result is the character month. If the receiving field is numeric the result is the month number (1-12).

- **`date_field`** · `fn/v` · *Required*

  The field containing the date value to use for calculating the month.

## Comments

This is the equivalent of the TAS Professional 3.0 command Equals Month.

## Program Editor

`3.0 Commands -> Equ Mnth`
