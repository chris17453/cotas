# FILELOC

## Summary
1

## Signature
```
This is actually no longer a field but just a key. If you try to set this equal to something
you will get a field not found error during compilation. You need to set the
DICT_FIELD_NAME and DICT_BUFF_NAME equal to their individual parts and then
just use the key.
2
```

## Details
With segmented keys the whole key structure has been separated from the fields. There
really is no need anymore for key names to exist in the field structure so these are a little
more difficult. You can either use these fields, which are 24 element arrays, or use the
fields in FILEKNUM. A single record exists in this file for each key. The key name is in
KNUM_KEY_NAME and the number is in KNUM_KEY_NUMB.
3
Overlays are now just a separate type of V.
12) The conversion process is now complete. All that's left is to run the programs. If you desire
you can slowly (or quickly) add the new features of TAS Professional 5.1 or you can just stick
with the 3.0 code. In fact, we have converted the 3.0 editor/screen painter/report writer so that
you can continue to edit program in 3.0 .EDT format. You will have to convert them to 5.1
before they can be compiled but that takes just a few moments, as you can see. The decision is
yours.
Conversion from Previous Versions
Converting from TAS Professional 4.0 to 5.1
There are several new commands in 5.1 and some new features. These include larger source code sizes,
virtually no limits on compiled segment sizes, full use of extended memory, and many more. We have
also returned to the 3.0 method of combining screen and report formats in the source code. However,
we've also given you the option of keeping some or all as separate files on the disk.
There are three commands that no longer exist. These are LOAD, CALL and RELEASE. All three
dealt with .BIN files and there are better ways getting the same results. Either use the INT command or
run an external program. With extended memory the programs would not have run and would have shut
down your system. Also, we have eliminated the source code library management routines since we've
made the other changes. There's no longer any need to limit your source files to 64k maximum nor keep
all those different .SCP/.SRP/.SRB files. If you have source code in one of these maintenance files it
will have to be exported before it can be converted to 5.1
NOTE: This process assumes that you have created a new sub-directory separate from where TAS50 is
installed. If you haven't done so yet then please do that now. The TAS50 sub-directory should only
hold the programs provided by CAS. Through the use of the SET TAS50= and putting the TAS50 subdirectory in your path (both in your environment) you will be able to access the Pro 5.1 programs from
any other drive or sub-directory on your computer. This process assumes your TAS Professional 4.0
programs and data are located at C:\APP40\
The following are the steps to converting to TAS Professional 5.1:
1) Create and log into the new sub-directory. Again, we're going to assume that you've created a
sub-directory called NEWPRGS on drive C. When this is complete you should be at the DOS
prompt in the new sub-directory:
C:\NEWPRGS>
2) Copy the OVL files:
COPY C:\TAS50\*.OVL
This will copy 3 files; TAS.OVL, TAS50.OVL and TASCOLOR.OVL. You can delete or just
ignore the TAS.OVL file. We have included it in the distribution set for the program
TASEDP30.RUN only.
3) Run the Set Configuration program by typing at the DOS prompt:
R50 C:\TAS50\TASCINFO
This should bring up the Set Configuration program as explained in Chapter 3, Main Menu
Programs. Move the cursor to the Default Dictionary Path near the bottom of the screen. Do
this by pressing the ENTER key until the cursor is in that field. If you haven't made any
changes in the TAS50 sub-directory then this field should be blank until you reach it. Then
your current sub-directory should be automatically filled in. Press the F10 key and answer Y to
the save question. The program will exit and return you to the DOS prompt.
4) Copy the data dictionary and files from the old sub-directory to the new:
COPY C:\APP40\*.B*
Your data and dictionaries are now converted!
Conversion from Previous Versions
Converting 4.0 Programs
NOTE: The conversion routine creates files with the same name and extension as those being
converted. Because of this you will want to make sure that the 4.0 source files are in a
different sub-directory from the sub-directory where you plan to put the converted files. If
you don't you will end up with files that are 2 bytes long! The conversion routine doesn't
change the original file so you can safely use the original source files in their original location
and just make sure you're saving the converted files to a new sub-directory. This would
already be the case if you followed the instructions at the beginning of this section.
5) We're now ready to convert the programs. To start the conversion program type:
R50 C:\TAS50\CNVTPALL
The first question it asks is what type of program you're converting. Since this is the 4.0
conversion you will enter 4. Next enter the file path and name, wildcards are acceptable. So to
convert all the BK*.SRC programs in the APP40 sub-directory you would enter 4 for the
program type and then the following for the file name(s):
C:\APP40\BK*
The program will then ask you if you wish to append any #INC (include) files that are referred
to in the source code. It will also ask if you wish to add screen and report formats. If you
answer Y here and the format is NOT found the MOUNT command in the source code will be
changed to MOUNT fmt_name xxxx EXTERN. This signifies that the format for this particular MOUNT is external (or outside) of the source code. If you answer N here then the compiler
directive #EXT_FMT will be added to the beginning of the program. This tells the compiler
that all formats are external to the source code.
Once you enter the file name(s) the program will start the conversion process. The conversion
process from 4.0 to 5.1 is actually very simple. If you don't include screen/report formats and
you don't append #INC files all you need to do is copy all the *.S* files from the C:\APP40\
sub-directory and then add the #EXT_FMT compiler directive to the beginning of each main
source file. You also need to run the CHKZEROS program to make sure there are no
lingering binary 0s in any of the source files. If you run the conversion routine it will do
this for you.
When this process is complete you should have a file in the new sub-directory with the same
name as the program in the old. These will all have the extension .SRC. The conversion utility
doesn't know whether or not the source code is part of another program or separate so all .SRC
files will be converted and copied over. If you've appended the #INC files you can delete all
those not necessary since they've been included already.
NOTE: This process does not effect library files. Those will still be separate.
6) The next step is to compile the converted programs. You should get no errors that didn't
appear in the 4.0 programs. You may need to change the records in COMPINFO.B. We've
found that just a single DEFAULT record will probably suffice since you can set the default
values so high.
7) Your conversion from 4.0 to 5.1 is now complete. We hope you enjoy the new features and all
that memory!
Conversion from Previous Versions
INTENTIONALLY BLANK
