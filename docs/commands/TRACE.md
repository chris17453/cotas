# TRACE

| | |
|---|---|
| **Category** | Command |
| **Platform** | DOS |

## Description

Use this command to execute the trace routine when you are debugging a program.

## Syntax

```text
TRACE what_to_do VALUE prg_fld BRK break_routine_label
```

## Parameters

- **`what_to_do`** · `sac` · *Required*

  What you want the trace routine to do. The options are: STEP turn on the single line step option. This will cause the program to stop at each line and
  if you have compiled the program with the -D (debug) option the actual line from the
  source file will be displayed at the bottom of the screen. You can then look at field
  values, set other break points, etc. PRG - Set the next break point for a specific
  location in the program. This is better done using the option in the actual trace routine
  once it has been executed since you need the actual location in the program of the line
  you want to break on. FIELD - By naming a field the program will break at the line
  the field is accessed, either read or updated. OFF - Turn off the trace routine. You
  can also use this at the beginning to just set up the routine. Then during program
  operation you can press the ^B (ctrl+B) key and execute the trace routine at any point
  in the program.

- **`prg_fld`** · `f/c/e` · *Required*

  If you have specified PRG or FIELD you must put in the appropriate value here.

- **`break_routine_label`** · `label` · *Required*

  The first time you execute this command you must tell the program the location of the trace routine. You do this by putting the line label name here. This will be BREAK_ROUTINE if you are using the standard trace routine (BREAK.SRC) from Business Tools Incorporated. You need only do this once; however, it won’t harm anything if you include it with every command.

## Comments

The trace routine is actually a separate program that must be included in any program you wish to debug
using this command. The standard program provided with this version of TAS Professional 5.1 is
BREAK.SRC. You include it in any program by putting the following line in the main program:
#inc break
If you plan to distribute your program, make sure you remove this line, and not just because it takes up
extra room. You may not include this routine in any program not being run at the same site that
holds the license for the compiler.
When this command is executed the program will display the trace window at the top of the screen and,
if you have compiled the program with the -D (debug) flag, it will also display the actual source code
line at the bottom. You can enter any expression and the program will display the result immediately
below the entry line. Press the F2 key and a list of fields, along with their current values, will be
displayed. Press the ^R key and the full screen will be displayed without any trace info; press any key to
return to the trace box.
