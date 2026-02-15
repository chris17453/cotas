# DISPLAY MEMORY AREA

| | |
|---|---|
| **Category** | Command |

## Description

Displays a portion or all of a single element from a memory area.

## Syntax

```text
DISPLAY MEMORY AREA mem_area# start_val size_of_element offset_in_element col_row_loc #_chrs_to_prt #_lines_to_print
```

## Parameters

- **`mem_area#`** · `f/c/e` · *Required*

  This is the memory area to display. This must resolve to a value of 1 through 4.

- **`start_val`** · `f/c/e` · *Required*

  The starting character in the memory area. The first character is at position 1.

- **`size_of_element`** · `f/c/e` · *Required*

  This is the total width in characters of a single element. This command will be displaying a part or all of a single element.

- **`offset_in_element`** · `f/c/e` · *Required*

  Where do the characters to be displayed begin within the element. The first character position in each element is 1.

- **`col_row_loc`** · `f/c/e,f/c/e` · *Required*

  The beginning column/row location separated with a comma. For example, if the beginning location is column 10, row 10 then this would be:\nAT 10,10

- **`#_chrs_to_prt`** · `f/c/e` · *Required*

  How many characters do you want to print from the element.

- **`#_lines_to_print`** · `f/c/e` · *Required*

  How many lines (elements) do you want to print. The program will start with the position specified in START and print the number of lines specified here.

## Comments

This is the equivalent of the TAS Professional 3.0 command Display Memory.

## Program Editor

`3.0 Commands -> Disp mem`
