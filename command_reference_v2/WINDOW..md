# WINDOW.

## Summary
Commands New for Windows
{} (Process controls - These also work in DOS), BUTTON, CAPTION, CLIK_SRCH_LIMIT ,
GRAY, HOT_SPOT,PICTURE, ROW_COLOR,WIN_COLOR.
Functions Not Implemented
ATAN2, CC, CCE, CCF, CCR

## Details
Windows Programming
Functions New for Windows
CLICKED_ON, COPY_FILE, EDIT, GET_ELEM_NUM, GET_WIN_COLOR, MAKE_DIR,
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
Windows Programming
One of the most important is the ability to get a handle for any window currently on the screen. All you
have to do is use the WINDOW_PTR function to get the handle or pointer for the currently active
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
Windows Programming
Window at .... wcolor get_win_color('red') bcolor
get_win_color('blue') ...
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
Windows Programming
At the top of the screen are 8 buttons and a line of text that tells you how many pages there are in the
report and what page you are on. The first two buttons let you move towards the beginning of the report
(left button) or the end (right button) one page at a time. The next two buttons will take you to the first
or last page of the report. The following two buttons will increase or decrease the size of the font. By
decreasing the size of the font you will get more characters to fit in the window, however, it may get so
small that you won't be able to read it. The next button will let you print the report to a printer. You
will get the standard choose print dialog, as described below, however you will also be able to choose
the page range to be printed. The last button lets you exit from the report list. You can also press the
ESC key and get the same result.
If you press the PRINTER button the following screen will be displayed.
Windows Programming
You first option is to choose the default printer, which is displayed in the box. You can also choose the
SELECT option which will allow you to display the standard Windows printer dialog below.
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
Windows Programming
TP5WIN.INI and How to Access It.
Most Windows programs have what's called an INI file (because of the .INI extension) and TAS
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
