# PRINT TOP OF FORM

## Summary
The command will prepare the output device for a new page.
PTOF print_to
print_to - SPD - Optional - You can use this option to specify which output device. The
option ASK (a regular print_to option) is not available. The only options available
are S - screen, P - printer or D - default.

## Signature
```
COMMENTS
If the output device is the screen, the "Press Any Key" message will be displayed. When the user
presses a key, the screen will be cleared and the default row and column will be reset to 1. If the output
device is printer or disk then a form_feed (binary character 12 - 0ch) will be sent to the device. If the
number of lines per page (total_lines) is set to something other than 66, and the device is printer or disk,
the program will use CR/LF's to get to the next page instead of a form_feed.
```

## Details
PROGRAM EDITOR
Reports -> Top of form
