# MEM_PTR

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS only |

## Description

MEMORY POINTER UPDATE (DOS only) This is a TAS Professional 3.0 command here for compatibility. There is no preferred TAS Professional 5.1 command/function.

## Syntax

```text
MEM_PTR mem_area# START start_val OCCURS #of_chrs_before_next SIZE size_of_ptr VAL
chg_value TIMES #_ptrs_to_chg DOwhat_to_do
```

## Parameters

- **`mem_area#`** · `f/c/e` · *Required*

  This is the memory area to use. This must resolve to a value of 1 through 4.

- **`start_val`** · `f/c/e` · *Required*

  The character position of the first pointer to be changed. The first character in the memory area is at position 1.

- **`#_of_chrs_before_next`** · `f/c/e` · *Required*

  The total number of characters before the next pointer. If there are a total of 10 characters per element in the memory area and the pointer is part of that element then this would be 10.

- **`size_of_ptr`** · `f/c/e` · *Required*

  This is the internal or csize of the pointer itself. If the pointer is O type (old form BCD) and has a display size of 5 then this would be 3 (5/2+rounding).

- **`chg_value`** · `f/c/e` · *Required*

  The amount to add to or subtract from each pointer. NOTE: The size and type of this field must be the same as the pointer.

- **`#_ptrs_to_chg`** · `f/c/e` · *Required*

  How many pointers do you want to change.

- **`what_to_do`** · `f/c/e` · *Required*

  This must resolve to either A (add) or S (subtract).

## Comments

This is the equivalent of the TAS Professional 3.0 command Memory Pointer Update.

## Program Editor

`3.0 Commands -> Mem ptr`
