# HP2

## Summary
PrintWidth
PrintLines

## Signature
```
80
60
```

## Details
PrintPaper
66
PrintLaser
True
The name of the .CTL (printer control) file on
the disk.
The number of columns wide you can print.
The number of lines you can print on the
page.
The total number of lines possible to print
from top to bottom.
Whether or not this is a laser printer. This
means that PrintPaper for this type of printer
would normally be 60 lines also. To make it
66 you have to send special control characters
before starting to print. At the end of the
report you'll need to send special control
characters to change back to 60 lines.
When printing through a Windows Driver
FontName
Courier New
RegularSize
CompressedSize
Bold
MarginSide
12
7
False
0
MarginTop
0
The name of the font to be used in the report.
You should probably use a fixed pitch font,
although you don't have to. However, if you
don't, you may not be able to line up the
columns as you expect. Also, Courier New is
a True Type font and should be available on
all printers.
The size of the regular pitch font.
The size of the compressed pitch font.
Whether to use bold option or not.
The number of columns to offset the report
from the left side. In general this will be 0,
however, you may find you need to set this
value when printing to a fax machine.
The number of rows to offset the report from
the top of the page. In general this will be 0,
however, you may find you need to set this
value when printing to a fax machine.
Windows Programming
We provide a program to manage this file called TASINI.RUN. This program must be run in Windows
and can be run directly in the command line, i.e., TP5WIN.EXE TASINI. When you execute the
program the first screen displayed is below:
The entry fields on the screen correspond to the TP5WIN.INI listings previous. Two issues that need to
be dicussed here are Display/Entry field sizes and the Enter Field border. Due to the font type we use
on the screens (to maintain compatibility with DOS) there are only a few display sizes available. The
one that fits the screen the best for normal size is 9. The next size down is 6. The default mode of
displaying a field is without a border. However, you can choose to put a border around the display field
by enterying Y at the Enter Field Border On option. This will do the following: When the field is in
normal display mode (not being entered) the program will put a border around the field. This will also
reduce the size of the field so that it will fit within the border to 6 (this can be very small depending on
your monitor and age). When the field is in entry mode the border will be removed and the font will be
increased to the Enter Mode Size value. You should be aware that your only options in size for this font
are 5, 6, 9, 12 and 14. Even if you change these values you will not change the size of the font used for
general text on the screen.
At the bottom of the initial screen are the buttons for entering colors and printers. To enter new colors
click on the Change Colors button or press the F2 key. When you do the following screen will be
displayed.
Windows Programming
Next press the F2 key or click on Change Colors and a list of the color options will be displayed.
Choose the appropriate color. When you do the following color dialog will be displayed.
You can then choose the appropriate color, first for the background and then the text. Note that even
though you can create custom colors that can be used those values will not be saved from one session to
another. Also, the color dialog that is displayed may be different for your system depending on your
video card and your setup.
If you wish to reset all colors back to the default values then press the F3 key or click on the Reset
button. When you're finished choosing colors press the ESC key or click on the EXIT button.
NOTE: When you make changes to either the first screen options or color values you MUST save the
new value to TP5WIN by pressing the F10 key or clicking on the SAVE button when you're on the first
screen. If you don't any changes you have made will be discarded. This, however, does not apply to
printer entries as described below. Also, you have to exit completely out of the current program and
then run TAS Professional for Windows again before the changes will take effect. This also does not
apply to changes made to printer information.
The main option from the first screen is for printers. When you press the F3 key the following screen is
displayed.
Windows Programming
Your first task is to choose a printer or just press the ENTER key for the default. Once you have chosen
a printer click the OK button or press the ENTER key. When you do the printer dialog box will be
removed and the following screen will be displayed.
The first entry is whether or not you're printing direct. If you are then enter Y here. If you enter N then
you will be using the Windows printer driver. If you answer Y to Print Direct you will be entering
values in the middle box. If you answer N you will enter values in the lower box. Each entry field will
default to the appropriate value if a value didn't already exist for it. Further, if you have entered
information for this particular printer previously, those values will appear on the screen. You can also
change from printing direct to printing through the driver and vice-versa without any ill effect. You can
also try printing with your settings and then come back and adjust them any number of times. Each time
you save the printer information the appropriate block in TP5WIN.INI is deleted and the new information is saved.
You can print to a printer without having this information entered for it, however, the program will
assume that you are printing through the Windows driver and will use all appropriate default values for
that printer. If you want to print direct you must have it added to the TP5WIN.INI file. The easiest way
to do that is to enter the information here.
Windows Programming
NOTE: As opposed to the main screen information and colors when you save the printer information
from this screen it is saved immediately to the TP5WIN.INI file. Even if you press the ESC key at the
main screen and answer No to saving changes, any changes you saved about printers will be in the
TP5WIN.INI file.
Field Search Process
When you or your user clicks on a field with the mouse TAS Professional for Windows has a specific
process in deciding not only whether to allow you to make that field active, but which line of code in
your program that field is attached to. This is called the Field Search Process. The rules that apply are
as follows:
1) If the user is not on a field currently they will not be allowed to move to a field by clicking on
it. This means they can't move from a menu to a field just by clicking on the field.
2) The program then checks for a NOCLICKOFF option in the ENTER command. If this exists
they won't be allowed to click on to another field.
3) Next it checks for a for an ENTER command in your code that matches the field. This is
probably the most complicated part of the process and there are some twists here also. First,
normally the process will start checking from the beginning of the program for the appropriate
line. However, if an UPAR command has been executed then the search process will start
from that line instead. Each time an UPAR command is executed (in the normal course of
operating, not during the field search process) this starting point is reset. The search process
will continue from that line until the end of the program or the first CLIK_SRCH_LIMIT
command. Through the use of both of these commands you can easily control where the
search process starts and ends for different parts of your program. This might be necessary if
you have the same field on two or more screens, but you only enter it in one location.
For example: Let's say you have two different entry screens in a program. The first is a
general entry screen, the second for a particular sub-group of fields. You would normally put
an UPAR command after the second MOUNT and before the first ENTER command so that
the user couldn't move towards the beginning of the program by pressing the UP ARROW key
when in the first entry field. If you didn't do this, and the user pressed the UP ARROW key as
they are apt to, your user would get a 'Field Not Mounted' error message and you would get a
support call. In Windows you or your user are more likely to use the mouse to click on a field
you want to move to and the field search process is what needs to be controlled. This time, if
you don't put in the proper controls, the ENTER command for the field being clicked on might
work properly, but when the user presses the ENTER key to move to the next field the lines
being executed are not the ones supposed to be next and then, you'll get the 'Field Not
Mounted' error or worse. The sample code below shows a situation that may occur.
Windows Programming
start:
mount screen1 type s
upar
enter fld1
enter fld2
enter fld3
... other code ...
clik_srch_limit
;put this here to keep the click
;process from checking too far down
;the code also!
start2:
mount screen2 type s
upar
enter fld4
enter fld5
enter fld6
... other code ...
clik_srch_limit
goto start
In this example above assume that that FLD1 is on both screens, however, it's only being
entered in the START group. If the user clicked on the field when SCREEN2 was mounted,
the process would start at the beginning of the program (assuming there were no UPAR
commands) and would find the ENTER that applied to SCREEN1. Then when the user
pressed the ENTER key while in FLD1 the next line to be executed would be ENTER FLD2
which would give the 'Field Not Mounted' error assuming that FLD2 wasn't on the second
screen also. By using the UPAR and CLIK_SRCH_LIMIT commands you can make sure
this won't happen to you or your users.
Another change in the Windows version is that the user can click on the array element of a field
and the program can correctly determine which field this is on the screen. So, you could have,
for example, all twelve months on the screen and have code allowing the user to enter these
values with a single ENTER command and a FOR/NEXT loop as follows:
for(cntr;1;12;1)
enter monthly_amounts[cntr]
next
The field search process will set the array element to the proper value internally for that
ENTER command. However, it is up to you to reset the CNTR value before you leave the
ENTER command (generally in a POST option) so that the user continues from the next field
on the screen if they press the ENTER key. If you don't update the value the entry process will
continue with the field that matches the next CNTR value. For example, if the user is on
element 2 and clicks into element 5. If they press the ENTER key only the entry process will
continue with element 3. However, if you use something similar to the code below, the entry
process will continue with element 6 and everything will look proper to the user:
for(cntr;1;12;1)
enter monthly_amounts[cntr] post reset_cntr()
{
func reset_cntr
if windows() then cntr = get_elem_num()
ret .t.
}
next
Windows Programming
Notice how we use the WINDOWS() function above to make sure we running in Windows.
This allows us to use this code in either DOS or Windows. Because the GET_ELEM_NUM()
function always returns 0 in DOS, if the WINDOWS() function had not been used to prevent
this section of code from being run under DOS, your user would have been permanently stuck
on the first element! This works for any element in the array since it will reset CNTR to the
current element number. CNTR will get incremented properly by the FOR command when the
loop continues. Finally, notice how we used the new code control braces. This allows us to
keep the code that applies to the ENTER command very close. And, since this works in both
DOS and Windows we don't have to worry about those issues when we use them.
4) If the field search process finds the proper ENTER command it now looks for a PRE option in
the new command. If a PRE option exists, the program will evaluate the expression and if it
returns false (.F.) the user will not be able to move to that field.
Don't forget, you can use the GET_ELEM_NUM() function in the PRE option also. The only
extra code you need here is to make sure that the field is being clicked on. If it isn't then the
function will not return the proper element number. So, using similar code to above, you would
do the following:
for(cntr;1;12;1)
enter monthly_amounts[cntr] pre check_fld() post reset_cntr()
{
func check_fld
if clicked_on()
if monthly_amounts[get_elem_num()]=0 then ret .f.
else
if monthly_amounts[cntr]=0 then ret .f.
endif
ret .t.
func reset_cntr
if windows() then cntr = get_elem_num()
ret .t.
}
next
Some things to note about the code above: First, we didn't have to check both WINDOWS()
and CLICKED_ON() functions. In DOS the CLICKED_ON() function will always return
false (.F.). Second, we didn't change the CNTR value if CLICKED_ON() was true. That's
because we're just checking the PRE option and if it returns false we don't want to change
where we are in the array. Finally, notice that we have two different UDFs in the code control
block. There is no limit the number of UDFs or other code lines you have within the braces.
5) Finally, if the PRE option of the clicked on field returns true or if there is no PRE option, the
program checks the VLD (valid) for the current field. If it returns true, or if there is no VLD
for this field, the program will move to the new field.
Sample Program
There is a sample program that demonstrates some of the features of TAS Professional for Windows.
This program is installed in the SAMPLE subdirectory. It is a standard .RUN program, however, it is
meant to be run in Windows only.
