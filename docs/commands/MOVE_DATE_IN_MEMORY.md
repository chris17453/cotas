# MOVE DATE IN MEMORY

| | |
|---|---|
| **Category** | Command |

## Description

This command does not convert any data but just moves raw characters from one location to another without regard for the type. You must specify either fields or memory areas to use. You may use P and F type pointers in this command. NOTE: Don’t forget to calculate number_of_characters based on internal size and not display size.

## Syntax

```text
MOVE DATE IN MEMORY to_mem_area#
```

## Parameters

- **`to_mem_area#`** · `f/c/e` · *Optional*

  The memory area number to transfer to. This must resolve to a value from 1 through 4. If you specify this value then the to_field is evaluated as the offset within this area.

## Comments

This command does not convert any data but just moves raw characters from one location to another
without regard for the type. You must specify either fields or memory areas to use.
You may use P and F type pointers in this command.
NOTE: Don’t forget to calculate number_of_characters based on internal size and not
display size.
