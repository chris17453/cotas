# {...}

| | |
|---|---|
| **Category** | Command |

## Description

These single character commands will allow you to group appropriate code together without worrying about program flow. For example, if you have an ENTER command that has a PRE and POST option and you want to have all appropriate code in one section you could write it as follows:
ENTER CUST_NAME PRE SetupSearch() POST ClearSearch()
{
func SetupSearch
Trap F2 gosub DoCustSearch
ret .t.
func ClearSearch
trap F2 dflt
ret .t.
}
When the program is running it will execute the ENTER command but will skip all other commands after the ENTER until the next command after the }. However, the commands within the braces are not invisible. They are still executed properly by the PRE and POST calls and can even be called by other commands before and after this ENTER. The only purpose for these controls is to keep related code together within your program.

## Syntax

```text
{...} (CODE PROCESS CONTROLS)
```
