# COMPANY CODE

## Summary
This command will get or set the three letter extension on files to be used as company codes (e.g., for
use with Business Tools' accounting products).
CO co_code GET/SET
co_code - f/c/e - Required - The new three letter extension. If this is a GET operation then this
must be a fieldname or variable field (fn/v).
GET/SET - Optional - If SET then the program will set the company code default value to that
in co_code. If GET then the program will get the current company code value and put
it into the co_code field.
COMMENTS
Accounting software products from Business Tools can handle multiple companies, and use a three
character extension on files to indicate different companies' data. For example, the master inventory file
for company #57 might be BKICMSTR.B57.
Once you set this value it is used as the extension for all TAS files opened unless otherwise overridden.
You should have a set of records in FILELOC.B with the proper company code value for each company
you have active. For more information about this topic, please refer to Chapter 3, Main Menu Utilities.

## Details
PROGRAM EDITOR
System -> Company code
