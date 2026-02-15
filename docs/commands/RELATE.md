# RELATE

| | |
|---|---|
| **Category** | Command |

## Description

REL invoice KEY inv_cust_code MSTR customer FLDLST cust_code (This assumes that inv_cust_code is a keyname; that there is only one field in the index; and that the index is the same size and type as cust_code.) This command would need to be executed only once, after the appropriate files have been opened. Each time a new customer is found the program will attempt to find the related record in the invoice file. If a record is not found, the appropriate error will be saved in the invoice file information buffer and can be checked, if needed, by the program through the use of the FLERR() function. You could also find a matching slave record explicitly by setting an equality between indexes and then doing a FIND. For example, to find an invoice record given the customer code key value, you might do the following:
inv_cust_code = cust_code
FIND record in INVOICE file
The major difference is that in using the REL command, the first find is event driven and can occur anytime within the program, regardless of how the master record is found.

## Syntax

```text
REL invoice KEY inv_cust_code MSTR customer FLDLST cust_code
```

## Parameters

- **`invoice`** · `f` · *Required*

  The file to be used as the Invoice file.

- **`inv_cust_code`** · `sac` · *Required*

  The keyname inv_cust_code. This is the value used to locate related records in the invoice file.

- **`MSTR`** · `sac` · *Required*

  Keyword indicating the master relationship. (Reserved word in the syntax.)

- **`customer`** · `sac` · *Required*

  Master file name (customer) to relate to.

- **`FLDLST`** · `sac` · *Required*

  Keyword introducing the field list to be retrieved from the related/master file.

- **`cust_code`** · `sac` · *Required*

  Field list value specifying the field from the master file to use for the relation.

## Example

```tas
inv_cust_code = cust_code
FIND record in INVOICE file
```

## Program Editor

`fiLe -> Find -> rElate`
