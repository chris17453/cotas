# EQUALS DAY

| | |
|---|---|
| **Category** | Command |

## Description

This is a TAS Professional 3.0 command here for compatibility. The preferred method is to use the =DOW() function for numeric day of week, or =CDOW() for alphanumeric day.

## Syntax

```text
EQU_DAY recv_field FLD date_field
```

## Parameters

- **`recv_field`** · `fn/v` · *Required*

  The field that is going to receive the day value. If the receiving field is A type then the result is the character day of week. If the receiving field is numeric the result is the day ofweek number.

- **`date_field`** · `fn/v` · *Required*

  The field containing the date value to use for calculating the day.

## Comments

This is the equivalent of the TAS Professional 3.0 command Equals Day.

## Program Editor

`3.0 Commands -> Equ Day`
