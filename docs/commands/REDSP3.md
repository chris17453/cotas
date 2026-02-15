# REDSP3

| | |
|---|---|
| **Category** | Command |

## Description

This is a TAS Professional 3.0 command here for compatibility. The preferred method is to use the REDSP command.

## Syntax

```text
REDSP3 screen_buff_num
```

## Parameters

- **`screen_buff_num`** · `f/c/e` · *Required*

  This is the screen buffer number. This must resolve to one of the following values:
  1-10 - Redisplay entire screen including active windows
  21-30 - Redisplay characters and color attributes only
  41-50 - Redisplay characters only

## Comments

This is the equivalent of the TAS Professional 3.0 command Redisplay Screen.

## Program Editor

`3.0 Commands -> M - Redsp3`
