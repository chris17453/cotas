# TIME

| | |
|---|---|
| **Category** | Command |

## Description

Get the current system time, or reset it to a new value.

## Syntax

```text
TIME time_field GET/SET
```

## Parameters

- **`time_field`** · `fn/v` · *Required*

  The T type field that will either receive the current time (GET) or contain the value to be used in setting the time (SET). GET/SET - Required - GET means to obtain the current time and put it into time_field. SET means to set the time using the value in time_field.
