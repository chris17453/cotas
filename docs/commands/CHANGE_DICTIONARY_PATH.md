# CHANGE DICTIONARY PATH

| | |
|---|---|
| **Category** | Command |

## Description

This command will change the default dictionary path to a different location.

## Syntax

```text
CDPATH location
```

## Parameters

- **`location`** · `f/c/e` · *Required*

  The new path for the dictionary. The value should be terminated with a backslash, e.g.: C:\\NEWPRGS\\ 

- **`location`** · `f/c/e` · *Required*

  The new path for the dictionary. The value should be terminated with a backslash, e.g.: C:\\NEWPRGS\\ 

## Comments

This command will NOT affect the dictionary used during compilation. That value defaults to the path specified in the TAS50.OVL file or to the value set in the command line.

## Program Editor

`System -> Programming -> chG dict path`
