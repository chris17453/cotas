# TRANSACTION

| | |
|---|---|
| **Category** | Command |

## Description

Using this command you can be assured that an entire set of file updates will be completed. Otherwise, they can be rolled back to an original state.

## Syntax

```text
TRANSX what_to_do ERR error_field
```

## Parameters

- **`what_to_do`** · `BCR` · *Required*

  You must provide a field that will be used to receive any error number provided by the routine. If the command step is successful the value returned is 0. This field must be of type I.

- **`error_field`** · `fn/v` · *Required*

  You must provide a field that will be used to receive any error number provided by the routine. If the command step is successful the value returned is 0. This field must be of type I.

## Comments

The correct transaction process is:
TRANSX B
&& begin
...
all file updates done here
...
TRANSX C
&& commit
However, if an error occurs while the file updates are being accomplished (between the TRANSX b and
TRANSX c steps) you would use the command:
TRANSX R

&& rollback

and any updates that were completed after the TRANSX b but before the TRANSX c would be undone.

## Program Editor

`fiLe -> Transactions`
