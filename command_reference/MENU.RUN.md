# MENU.RUN

## Summary
InitCaption

## Signature
```
blank
```

## Details
DefaultCompanyCode
blank
AllowGray
False
Entered characters during a LISTF/LISTM
command text.
Background color for raised frame around
windows and menus as described above.
Text (or border) color for raised frame
around windows and menus as described
above.
Number of rows in base form.
Number of columns in base form.
Regular display (non-entry mode) font size.
Entry mode (during ENTER command) font
size.
Whether there is a border around the entry
field during display mode.
Size in pixels of the raised border around
windows and menus as described above.
If this is set to True then in the NMENU
command if TAS finds a & before a character
in the menu line it will remove the & and put
an underscore _ under the character immediately to the right of the &.
This is the initial program that will be run if
there is no program in the executable command line.
This is the caption that will appear in the
caption block at the top of the default form.
This is the company code that will be used as
the default value when the files are opened.
If no value is here TAS will use a blank value
which is the normal default. If you save a
value here it must be two characters. This
option follows the same rules as standard
company codes.
If this is set to True then inactive windows
will be "grayed out" unless the command
GRAY OFF has been executed. For more
information please refer to the command
GRAY in Chapter 5 - Command Reference.
Windows Programming
Along with the categories listed above there will be a category created for each printer you add to the
file. The category name for each printer will be the printer name, for example, the printer HP LaserJet
Series II on LPT1 would be set to: [HP LASERJET SERIES II]. The entries are different if you are
printing direct to the printer or if you are using the Windows driver. These entries are:
Option Name
When printing direct
PrintDriver
Default Value
Description
