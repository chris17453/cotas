# Windows Programming

Chapter 11
Windows Programming

Windows (and possibly even programming in Windows) for many years. For some of you this is your
first attempt. Our aim with TAS Professional 5 for Windows is not to create the most complete programming language for Windows. It is to allow you to ability to keep one foot in DOS and the other in
Windows as your customers make a slow, but methodical, transition to the Windows world. You will
still be able to program in TAS for DOS and use that same code, with minor modifications, for your
Windows customers. Not only that, but you'll be able to run some users on DOS and others in Windows
at the same site, using the same programs and data! In fact, you'll even be able to run Windows and
DOS versions of the same program for a single user at the same time.
As with our aim to create a first step into Windows with our language, this chapter is not meant to be the
complete and encyclopedic information in actual Windows programming, but the information you need
about the enhancements and differences between the DOS and Windows version of TAS Professional 5.
One major point to remember is that we have made this version of TAS Pro for Windows 16bit instead
of 32bit (the DOS version). This allows you to run on both Win 3.1x and Win95. If we had created a
32bit version you would have been restricted to Win95/WinNT only. The important issue about this is
that the maximum field size is 64k (65535) characters and the maximum number of array elements for
any one field is 64k. What this means to you is that you can't run the development system in Windows
since some of the fields are 500k+. So, you develop in DOS and run in Windows. If you have Win95
(and a 15+ inch monitor) you can put a DOS box on the screen in a small window and move easily
between the development system and the Windows program. Since you can have both DOS and
Windows active at the same time you don't even have to exit out of the development system when
running your program under Windows. The limitations in existing commands and functions, as well as
the new commands and functions, are listed below and in Chapter 5 - Command Reference, and
Chapter 6 - Function Reference.
fine on a Windows 95 but has not been tested thoroughly on Windows NT.
We hope that this program works as well for you as it has for us. Now on to the rest of the information.

The Programs That Make Up TAS Professional 5 for Windows
1) TP5WIN.EXE - This is the actual Windows executable that will run your TAS Professional for
Windows programs. This program does not need to be in the sam sub-directory as the programs you are
running, however, you should be sure it's in a sub-directory specified in your PATH option in your
AUTOEXEC.BAT file. For example, this could be your TAS50 sub-directory. If the TP5WIN.EXE
file is not in the same sub-directory as your .RUN programs then you can either specify the initial run
program in your command line or in the TP5WIN.INI file. In either case be sure to use the entire path
or TAS won't be able to find the correct .RUN program. The path for the initial run program is also
used for the default location of TAS50.OVL and it's where TAS will look for TP5WIN.INI first.
2) TPC50.EXE - This is the standard TAS Professional 5 for DOS programs. Make sure you're running
version 5.1 or newer. Any version prior to this will not have the Windows extensions as part of the
language.
3) TP5WIN.INI - More about this later, however, this file needs to be in your default sub-directory (as
described above) or in your \WINDOWS sub-directory. There is a new program TASINI.RUN that will
maintain this file.



for this program. This entry should look like the following:
[BTRIEVE]
options=/m:64 /p:4096 /b:16 /l:40 /n:12 /f:150
This entry can be made by the installation program or you can make it yourself. If you get error #24
while opening a file you more than likely have a problem in this. You should also make sure you don't
have multiple Btrieve sections in your WIN.INI file if you enter it yourself. For more information on
installing TAS Professional for Windows please refer to the general installation information in the front
of this manual.
Now let's look at the changes between DOS and Windows.

Changes, Enhancements and Limitations
The most important limitation is that this is a 16bit product. This returns us to the 64k limits set for field
and array sizes. Also, since we have not included the compiler in this version you cannot use the
FILTER command or any alpha macro fields, i.e.:
AlphaField = 'X * 2'
Y = &AlphaField
If you include the above in a program you will get an error during runtime. The FILTER command is
just ignored (as are all other commands and functions that are not part of Windows) and does not give
an error. The FOR and WHILE options (used in many commands) are still availabled and work just
like you would expect them to. This restriction on filters applies just to expressions that would be
compiled 'on the fly' as is shown above.
Some commands and functions are not applicable to Windows and are not implemented. This can be for
various reasons but mostly because they don't have any use. All the color commands in DOS are
significantly different in Windows (see section on Color in Windows below). So, many of the standard
DOS color commands are just ignored. All of the commands and functions that have not been implemented either have new forms in Windows or were not used much, if at all, in DOS. Following is a list
of commands and functions that are not implemented in Windows at all, changed, or new to the Windows version:
Commands Not Implemented
AUTORUN, BKG, COLOR (colors are much different in Windows), COMPRG, DISPMEM, FRG,
INT, LIST (not to be confused with LISTF and LISTM), MEMPTR, MEMSPC, MOUSE (it's
always on), PAINT, PEEK, POKE, PORT, PRTNUM, REV, TRACE.
Commands Changed or Enhanced
CHAIN, CURSOR, ENTER, LISTF, LISTM, MSG, NMENU, SOUND, UPAR, WINDEF,
WINDOW.
Commands New for Windows
{} (Process controls - These also work in DOS), BUTTON, CAPTION, CLIK_SRCH_LIMIT ,
GRAY, HOT_SPOT,PICTURE, ROW_COLOR,WIN_COLOR.
Functions Not Implemented
ATAN2, CC, CCE, CCF, CCR



MAX_COLS, MAX_ROWS, PRINT_CANCEL, PRINTER_NAME, WIN_LASER_PRT,
WINDOW_PTR, WINDOWS
Details about changes, and new commands/functions, are in Chapter 5 - Command Reference and
Chapter 6 - Function Reference.
In DOS you could not 'click' on a field that was part of an array, in Windows you can. You can use the
'standard' Windows editing commands in any field. This includes selecting a group of characters with
the mouse (holding down the left button and dragging the mouse pointer over the characters), ^X
(ctrl+X) to delete those chosen characters, ^C (ctrl+C) to copy those characters, or put the mouse
pointer at a certain location and press ^V (ctrl+V) to insert characters copied previously either in a TAS
program or some other that were saved to the clipboard.
The Windows version allows for multiple instances. This means you can bring up TAS Professional for
Windows once and then do it again, and again, etc. until you run out of memory. If you attempt to
access the same record in the same file in two different instances the program will act as though the
record were locked by another user (just what it should do).
One of the major changes in Windows is a window itself. The next section explains the differences
between the DOS and Windows screens.

Windows in Windows
Obviously, Windows is a much different beast than DOS. However, the most important difference is
how it keeps track of what's on your computer screen. Probably the easiest way to explain this is that in
DOS you have a flat screen. What we mean by this is that only one thing can be on the screen at a time.
When you create a window and display it the characters under that window are gone forever, unless you
have previously saved the screen with the SAVES command. But what you're doing, with the SAVES
command, is putting each character and color attribute on the screen into a buffer. In DOS there are
2000 characters and color attributes in a 25 row by 80 column screen. In Windows, things are measured
by the pixel instead of a character and in TAS it takes 128 pixels to make up a character. And by the
way, that's just the text. Each pixel can have its own color attribute also, each color attribute is an R
type field (internally it's a longint if you know what that is), so you can see that the number of bytes to
hold the screen starts becoming very large. Plus, it would take forever, to constantly move those bytes
from buffer to screen, to buffer, etc., even on fast computers. Luckily for us, Windows takes care of
most of the work. So, instead of having a flat screen, we now have things on the screen (I hate to call
them objects). And, even though you can't see it, they are actually piled on top of each other. So, when
you add a new window it doesn't obliterate the characters underneath it, it just lays on top of them.
What we've done here, it to try to make it look as much like a DOS screen as possible without losing the
Windows touch and feel, thereby making your job as easy as possible.
There are some things that are much different, however. When you first run a TAS program, we create
what we call a base form. The size comes from values you set in the TP5WIN.INI file (more about this
later). No window you create can be larger than that base form. Each time you run a program TAS
creates a new base window that is the same size as the client area of the base form (unless you use the
NOBASEWIND option in the CHAIN command). All the windows, entry fields, etc. you create in that
program are then stacked on top of the base window for that program. Each window you create is a
separate entity until you get rid of it, either by exiting the program or using the REDSP command. In
fact, the SAVES and REDSP command work much differently in Windows than in DOS. In DOS when
you run the SAVES command it will save the current screen characters and attributes either to an
internal buffer or to a buffer you supply in the command. In Windows all it does is put a bookmark in
the list of windows that have been created. When you use the REDSP command in DOS it puts
whatever is in the appropriate buffer back on the screen. In Windows it removes all the windows
currently on the screen that have been created since the corresponding SAVES. You can see in just this
one example how different the two systems are. There are many benefits to the process in Windows.



window. Then, when you want to make that window active again, later in the program, you just use the
WINACT command and pass the value received earlier. This assumes that you have not removed the
window previously with a REDSP command. Once it has been removed it cannot be called back
without using the WINDOW or WINDEF commands.
Another thing that works differently in Windows is shadows. In DOS you can put a shadow to the right
or left of the current window (this also works for menus, etc.). In Windows we actually create a raised
border around the window if you specify a shadow of either right or left. This again, gives your program
more of a Windows look and feel.
One command that has changed significantly in Windows is the RSCR (Reset the Screen) command. In
DOS this command will undo the effect of any windows without removing those windows from the
screen. In Windows this command removes all windows (except the base window) and may have
unexpected results. If you find that you are losing windows on the screen when running a program, look
for an RSCR command. In DOS you could alway reactivate a previously defined window after this
command, in Windows you can't.
Hopefully, this will give you some insight into how we run under Windows. The next subject is
logically connected, and that's about color.

Color in Windows
Probably, the thing you hear the most from new computer users is that Windows is much prettier than
DOS. It certainly has more colors! Of course, in most systems today the typical user never even sees
DOS so they might not even know what it used to be like.
In DOS colors are very easy, but also very limited. In Windows things become much more complex. In
DOS a single value determines both the background and text color. In Windows there are two different
values. Further, the values in DOS are a 1 byte value with a range of 0 through 255. In Windows this
becomes a 4 byte value with a range of 0 through 4,294,976,295. Most computers don't allow close
to this many options but many today allow more than 256.
In Windows a color value is split into 4 different pieces or bytes. Three bytes cover the colors red (R),
green (G) and (B) which we all remember as primary colors from our high school science classes. The
fourth byte determines the palette which we won't get into here. For right now, the palette byte (or the
highest byte) will always be 0. The RGB colors can range from 0 to 255 each. The higher the color
value the more intense the color becomes, so a blue value of 255 would be the most intense blue
possible and 0 would be no blue at all. If all three color values are 0 you have the color black (no color
at all) and if all three are at full intensity you would have the color value of 16,777,215 or white. To
determine the color value use the following equation:
Color value = (BLUE*65536)+(GREEN*256)+RED
Or, you can use the standard colors provided by Windows. These are the colors you (or your system)
have determined as appropriate for the different parts of the screen, or standard colors. You can also
use colors that have been setup in the TP5WIN.INI file. You should note that you can't use these colors
directly, you must use them as the parameter in the GET_WIN_COLOR function. Also, due to the
major difference in colors between DOS and Windows all commands that would normally allow you to
set specific colors ignore any values unless you use the USE_COLORS option that is a part of that
command. This applies to NMENU, LISTM, LISTF, WINDOW and WINDEF. If you do not use the
USE_COLORS option these commands will get the appropriate colors from the TP5WIN.INI file or
from the defaults for those colors (see below). A typical command might be:



The following are the different Windows color names:
Standard Windows Part Names (these colors are set by the system)
ScrollBar, Background, ActiveCaption, InactiveCaption, Menu, Window, WindowFrame,
MenuText, WindowText, CaptionText, ActiveBorder, InactiveBorder, AppWorkSpace, Highlight,
HighlightText, BtnFace, BtnShadow, GrayText, BtnText, InactiveCaptionText, BtnHighlight.
Standard Windows Color Names
Black, Maroon, Green, Olive, Navy, Purple, Teal, Gray, Silver, Red, Lime, Yellow, Blue, Fuchsia,
Aqua, LtGray, DkGray, White.
Color Names in TP5WIN.INI
NormalBkg, NormalText, BoxBkg, BoxText, EnterBkg, EnterText, MsgBkg, MsgText,
WindowBkg, WindowTextSet, MenuBkg, MenuTextSet, ButtonBkg, ButtonText, ChoiceBkg,
ChoiceText, EcolorBkg, EcolorText.
Another process that can be significantly different in Windows is printing. Let's look at that now.

Printing in Windows
For those of you who have had the joys of trying to print under Windows you may know that it's not
quite as simple as they would like you to believe. Also, no matter how hard you try, your output is never
going to match up to what you had under DOS. So, we here at Business Tools, trying to always give
you the best of both worlds, believe we've really done it this time. Not only can you use the appropriate
Windows printer driver, where necessary, but you can bypass the driver and print direct where possible.
When printing to non-HP compatible laser or IBM dot matrix compatible printers you are forced to use
the Windows printer drivers. This includes Postscript type printers, fax machines, etc. However, if you
can print to the printer using TAS Professional 5 for DOS you can print direct to the printer in Windows.
Why would you want to do this? By printing direct you get the exact same output you would under
DOS. This means that all the formatting you have already done will still work. Further, if you want to
use control characters to create boxes, shading, etc. all that will still work. When printing through the
Windows drivers the only option, at this time, is to print in condensed format.
If you can print direct you will want to. You lose no capabililties; you still choose your printer from the
list provided by Windows and you can print across the network without having to capture a printer port.
Any printer you can access in Windows in other programs you can now access the same way in TAS
Professional for Windows. The only difference will be whether your program is using the Windows
printer driver to communicate with the printer or uses the internal controls built into the printer.
The way TAS knows whether it is to print direct or not is by the entry in the TP5WIN.INI file for the
printer. That is described below. Right now we want to show you what the printing dialog looks like.

The print dialog box above is the first one that will appear if your program allows the user to choose
where to send the output from the report. If the user chooses List (the default button) the report will be
printed to disk first. Then a screen similar to the one below will be displayed.



report and what page you are on. The first two buttons let you move towards the beginning of the report
(left button) or the end (right button) one page at a time. The next two buttons will take you to the first
or last page of the report. The following two buttons will increase or decrease the size of the font. By
decreasing the size of the font you will get more characters to fit in the window, however, it may get so
small that you won't be able to read it. The next button will let you print the report to a printer. You
will get the standard choose print dialog, as described below, however you will also be able to choose
the page range to be printed. The last button lets you exit from the report list. You can also press the
ESC key and get the same result.
If you press the PRINTER button the following screen will be displayed.




You can then click on the drop box for Specific Printer to get a list of printers for your system. Once
you choose a printer you will return to the choose printer dialog. Press the OK button or enter O and the
program will start printing. When you choose a printer the program will get the appropriate information
from the TP5WIN.INI file for that printer. This information will tell it whether to print directly to the
printer or use the Windows driver (more about this later).
If you want to print the report to disk just click on the DISK button (or press the D key) from the
original dialog box and the dialog below will be displayed.

You can either enter a file name directly, or click on the SEARCH button. If you enter a name for a file
that already exists the program will alert you to that and you will be given the chance to choose whether
or not to overwrite the existing file (if any). When you click on the OK button (or enter O) the report
will be written out to disk.
All of the choices, including the original where to print choices, offer a cancel option. If you click on
the CANCEL button once you are in the Printer or Disk option you will return to the original options. If
you choose CANCEL again the program will exit from the print where dialog. You can tell, in your
program, that the CANCEL option has been chosen by checking the CANCEL_PRT() function. For
more information on this function please refer to Chapter 6 - Function Reference.



Professional for Windows is no exception. This file plays a role similar to the TAS50.OVL file but it
has different values. TAS Professional for Windows still uses the standard TAS50.OVL file to get
information such as the default data dictionary, date and time type, etc.
This file can be in two different locations. The program first looks in the user's default path. This is
where the first program is executed. If it doesn't exist there the program looks in the user's \WINDOWS
sub-directory. If the file doesn't exist there it is created in that sub-directory. By using this procedure
you can create different TP5WIN.INI files for different applications, users, etc. If you ever feel that
you've messed up the beyond repair, just delete it. You can always specify the initial run program in the
command line for TP5WIN.EXE. If the INI file is not found and there is no program in the command
tail TAS will look for the programs BKMENU.RUN and MENU.RUN. If neither of those are found
you will get an error message. However, once the program is running you can then go back and reset the
values, as appropriate, using the maintenance program described below. You can also edit the file
directly with a text editor if you wish.
The installation program automatically puts a copy of TP5WIN.INI into the same subdirectory where
you install TP5WIN.EXE. You may end up installing TAS Professional for Windows several times in
separate subdirectories. Each time you will put a new copy of TP5WIN.INI in that same subdirectory.
Since you may have already changed colors, added printers, etc. and don't want to go through that work
again, you can copy previously edited versions of the TP5WIN.INI file to the new subdirectory. Also,
you can keep a single copy in the \WINDOWS subdirectory. Then when you install new copies of TAS
Professional for Windows just delete the copy of TP5WIN.INI in the new subdirectory. This will force
the program to look for the file in the \WINDOWS subdirectory. NOTE: This will also work if you are
loading Windows from a network drive and have all the programs in a single directory on the same or
different network drive. By setting the InitialProgramName value in TP5WIN.INI you can control
where the default drive is for each user.
TP5WIN.INI contains, at a minimum, three different categories. If you aren't familiar with Windows
INI files each category starts with a name surrounded by square brackets. These three categories are
[Color], [Video] and [Misc]. Following each category is one or more names for different values used
within TAS. When TAS is initially loaded (or when a printer is chosen) the program looks at the INI
file for the values for each of the options. If a value is not provided the internal value is set to a default.
The lists below give each of the options, their default value a short description of the item. Please note,
when a value is given as True or False it is actually 1 or 0 in the TP5WIN.INI file.


Option Name
[Color] category
NormalBkg
NormalText
EnterBkg
EnterText
WindowBkg

Default Value

Description

BtnFace
BtnText
Window
Black
Window

WindowText
MenuBkg
MenuText
ButtonBkg
ButtonText
ChoiceBkg

Black
Window
Black
BtnFace
BtnText
Highlight

ChoiceText
EColorBkg

HightlightText
Yellow

Base window color.
Base window text color.
Entry field background color.
Entry field text color.
Created window (WINDOW, WINDEF)
background color.
Created window text color.
Menu (MENU, NMENU) background color.
Menu text color.
Button background color.
Button text color.
Choice bar background color (Menus and
LISTF/LISTM).
Choice bar text color.
Entered characters during a LISTF/LISTM
command background.


Black

ShadowBkg

WindowFrame

ShadowText

WindowText

[Video] category
VideoRows
VideoCols
FontSizeReg
FontSizeInp

26
80
-9
-9

BorderOn

False

ShadowWidth

4

[Misc] category
UnderScoreMenuChr

True

InitRunPrg

MENU.RUN

InitCaption

blank

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


Series II on LPT1 would be set to: [HP LASERJET SERIES II]. The entries are different if you are
printing direct to the printer or if you are using the Windows driver. These entries are:
Option Name
When printing direct
PrintDriver

Default Value

Description

HP2

PrintWidth
PrintLines

80
60

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

