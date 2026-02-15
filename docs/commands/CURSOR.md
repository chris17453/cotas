# CURSOR

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to change the size of the cursor on the screen and to turn it on and off.

## Syntax

```text
CURSOR ON/OFF SIZE start_line,stop_line WAIT DFLT
```

## Parameters

- **`ON/OFF`** · `sac` · *Optional*

  Optional - If ON the cursor will be turned on, if it is off. This is the default value.

- **`SIZE`** · `n` · *Optional*

  start_line,stop_line

- **`start_line`** · `f/c/e` · *Optional*

  The beginning scan line for the cursor block. This may be from 0 through 7 for color monitors and 0 through 13 for monochrome.

- **`stop_line`** · `f/c/e` · *Optional*

  The ending scan line for the cursor block. This may be from 0 through 7 for color monitors and 0 through 13 for monochrome.

- **`WAIT`** · `sac` · *Optional*

  Windows Only - Optional - Will change the standard arrow cursor to an hourglass or whatever is the equivalent in Windows 95.

- **`DFLT`** · `sac` · *Optional*

  Windows Only - Options - Will change the cursor back to the standard arrow. Used after setting the WAIT option when whatever process that was taking so long is over.

## Comments

The stop_line value should be the larger number. The default value in TAS Pro 5.1 is 6,7 for color and 11,12 for mono displays.
NOTE: The ON/OFF and SIZE options have no effect in Windows.
