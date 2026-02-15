# COMPILER DIRECTIVE

| | |
|---|---|
| **Category** | Command |

## Description

This command will give the compiler instructions that are necessary only during compilation of a program.

## Syntax

```text
COMPILER DIRECTIVE type_value
```

## Parameters

- **`type_value`** · `sac` · *Required*

  This is another form of the special alpha constant. It may or may not also have a numeric constant (value) as a part. The options are: ADD_FLDS - Adds the number of field slots specified to the internal field list. To be used during operation to add fields on the fly. ALL_LOC - Specify that all fields defined are LOCAL. Still requires the PROC and ENDP compiler directives before this directive takes effect. Once the ENDP directive is encountered the ALL_LOC is turned off and must be specified again if you wish to make another set of DEFINE fields LOCAL to a routine. CHK_UP_VLD - Normally, the VALID option in the ENTER command is always checked unless the user presses the ESC or Up Arrow keys. If you want to check the valid when the Up Arrow key is pressed also then include this directive. ENDP - Specifies the end of a Procedure that is started with the PROC directive. EXT_FMT - In TAS Professional 5.1 all screen/report formats are normally included as part of the source code. However, if you wish to have them as separate files on the disk use this compiler directive. If this is set then the program expects all formats (no matter what type) to be separate for this program. You can also use the EXTERN option in the MOUNT command if you want to keep just a few formats separate. This would apply when you have a format you use in several different programs. FMT - If you are using 'old type' screen/report formats then this option must be set. If the PRO3 compiler directive is also set then the compiler expects the screen/report formats to be in TAS Professional 3.0 form. If PRO3 is not set then you must use the TAS Professional 4.0 method. INC - A named source file to be compiled and included in the run program. LIB - Specify a library of commonly used routines. At the end of the compilation process if the compiler is still missing pieces it will look here. If an extension is not given for the library file name it is assumed to be ‘.LIB’. If no path is given it is assumed to be in the user’s current directory. If the name is surrounded by angle brackets, e.g., <TASRTNS.LIB>, it is to be found in the TAS50 path. With TAS Professional 5.1, you also have the ability to use a SET TASLIB= to set a path in your environment (generally in your AUTOEXEC.BAT file). If you use a SET TASLIB= statement, the compiler will look for all libraries in the path given. This means you can have one set of libraries for all programs you're developing. OLD_MATH - Use the TAS Pro 3.0 method of resolving expressions, left to right, regardless of operator precedence. PRO3 - Specifies to both the compiler and the runtime that the program is a direct conversion from TAS Professional 3.0. A more complete listing of the effects of this directive is in Chapter 4, Compiler Info.

## Comments

For more information on Compiler Directives please refer to Chapter 4, Compiler Info.
