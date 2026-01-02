# RECORD NUMBER

## Summary
This command will either return the record number (position) of an active record, or will use the value
supplied to read a record.
RCN filename/@file_number RCN record_number_field GET/SET NLOCK
filename/@file_number - file_expr - Required - The name or number of the file to use.
record_number_field - fn/v - Required - The field that will either hold the results of a GET or
provide the value for a SET. This must be an R type field.
GET/SET - Required - GET the current record number, or SET the value in the
record_number_field into the appropriate location and read the record.
NLOCK - Optional - Normally, in multi-user mode, when a record is read it is automatically
locked so that other users cannot read it until you are finished with it. If this is a SET
operation and you don’t want to lock the record upon reading it then include this
option in the command.
NOTE: If you include this option and another user reads the record and then you try
to save the record back to disk you will get an error message telling you that this has
occurred. You need to read the record again before saving it back to disk.

## Signature
```
COMMENTS
This command will work with both TAS Pro 5.1 and non-TAS files.
The screen will not be automatically refreshed when this command executes. If you need to refresh data
on the screen you should execute the SCRN R command immediately after a GET operation.
```

## Details
PROGRAM EDITOR
fiLe -> Find -> Record number
