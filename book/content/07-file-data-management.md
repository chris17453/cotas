## Opening and Closing Files

TAS programs access data through numbered file handles. Files are opened with `OPENV` and linked to their data definitions through Data Dictionary Files (DDFs).

```tas
OPENV customers 1       ; Open customer file as file #1
OPENV invoices 2        ; Open invoice file as file #2

; ... work with the files ...

CLOSE 1                 ; Close file #1
```

The `StorageEngine` resolves the TAS file name to a `.int` DDF file, reads the schema (table name, fields, indexes), and establishes a database connection. CoTAS connects directly to your database using open-source ADO.NET providers - no paid drivers, no middleware, no per-seat licensing.

<div class="callout new">
<div class="callout-title">⚡ No Paid Drivers Required</div>
Unlike legacy TAS which required expensive Btrieve/Pervasive licenses, CoTAS talks directly to your database engine using free, open-source .NET data providers:
<ul>
<li><strong>MSSQL</strong> - <code>Microsoft.Data.SqlClient</code> (fully supported now)</li>
<li><strong>MySQL</strong> - <code>MySqlConnector</code> (in progress)</li>
<li><strong>PostgreSQL</strong> - <code>Npgsql</code> (in progress)</li>
</ul>
Your existing TAS programs work unchanged - just point the connection string at your database. No per-seat fees, no pay-as-you-go metering.
</div>

### The MOUNT Command

`MOUNT` sets the data path - the directory where CoTAS looks for DDF files and data:

```tas
MOUNT data_path
```

The mount path is stored in the `__MOUNT_PATH` system variable and used by `DPATH()` and `CPATH()` functions.

<div class="callout new">
<div class="callout-title">⚡ CoTAS Storage Architecture</div>
CoTAS uses a layered storage architecture:
<ul>
<li><strong>DdfParser</strong> - Reads <code>.int</code> files (key-value format) to produce <code>TableSchema</code> objects</li>
<li><strong>StorageEngine</strong> - Translates TAS file operations to parameterized SQL queries</li>
<li><strong>FileHandle</strong> - Tracks per-file state: record buffer, EOF/BOF flags, lock mode, current record ID</li>
<li><strong>RecordBuffer</strong> - In-memory cache of current record fields with dirty tracking</li>
</ul>
When no <code>StorageEngine</code> is available, file commands degrade gracefully to no-ops - so you can test UI logic without a database.
</div>

## Finding Records

### FINDV - Keyed Access

`FINDV` locates a record by key value. The `StorageEngine` translates this to a parameterized SQL query:

```tas
cust_id = "ACME"
FINDV M 1 @1 cust_id      ; Find match by primary key in file #1

IF .NOT. EOF(1)
  SAY 1,1 cust_name
ELSE
  SAY 1,1 "Customer not found"
ENDIF
```

Find modes and their SQL translations:

| Mode | TAS Syntax | SQL Generated |
|------|-----------|---------------|
| **F** (First) | `FINDV F 1` | `SELECT TOP 1 * FROM [tbl] ORDER BY (SELECT NULL)` |
| **L** (Last) | `FINDV L 1` | `SELECT TOP 1 * FROM [tbl] ORDER BY key DESC` |
| **M** (Match) | `FINDV M 1 @1 val` | `SELECT TOP 1 * FROM [tbl] WHERE [key] = @key0` |
| **G** (Greater/Equal) | `FINDV G 1 @1 val` | `SELECT TOP 1 * FROM [tbl] WHERE [key] >= @key0 ORDER BY [key]` |

### SCAN - Sequential Access with Filtering

`SCAN` reads records sequentially. The `StorageEngine` opens a SQL cursor:

```tas
FINDV G 1 @1 "A"         ; Position to start

SCAN @1 KEY custid START "A" WHILE custid < "Z"
  SAY ROW(),1 cust_name
  SAY ROW(),35 STR(balance,10,2)
ENDS
```

SQL generated for SCAN cursor:
```sql
SELECT * FROM [dbo].[CUSTOMERS]
WHERE [CUSTID] >= @key0
ORDER BY [CUSTID], [CUST_NAME]
```

## Modifying Records

### SAVE - Insert or Update

```tas
cust_id = "NEWCO"
cust_name = "New Company Inc."
balance = 0
SAVE 1                    ; INSERT if new, UPDATE if existing
```

SQL generated:
```sql
-- New record:
INSERT INTO [dbo].[CUSTOMERS] ([CUSTID], [CUST_NAME], [BALANCE])
VALUES (@p0, @p1, @p2)

-- Existing record:
UPDATE [dbo].[CUSTOMERS]
SET [CUST_NAME] = @s0, [BALANCE] = @s1
WHERE [CUSTID] = @w0
```

### DEL - Delete Current Record

```tas
FINDV M 1 @1 "OLDCO"
DEL 1
```

SQL: `DELETE FROM [dbo].[CUSTOMERS] WHERE [CUSTID] = @p0`

### CLR - Clear Record Buffer

`CLR` resets all fields in the file's record buffer to defaults without touching the database.

## Record Locking

When multiple users access the same data simultaneously, CoTAS uses database-native row-level locking:

```tas
REOPEN 1 L                ; Reopen file #1 with lock mode
READ 1                    ; Read and lock current record
balance = balance + amount
SAVE 1                    ; Update and release lock
```

Lock-related commands: `RLCK` (record lock), `ULKALL` (unlock all), `REOPEN` (change lock mode).

## Arrays

TAS supports arrays for in-memory data collections:

```tas
DEFINE FIELD prices N 10 2 ARRAY 100   ; 100-element numeric array (1-based)
DEFINE FIELD names A 30 ARRAY 50       ; 50-element string array

prices[1] = 29.99
prices[2] = 49.99
names[1] = "Widget"
```

Array indexing is **1-based** in TAS. Both `ARRAY[n]` and `ARRAY(n)` syntax are supported.

### Array Commands

| Command | Handler | Description |
|---------|---------|-------------|
| `RDA` | `ArrayReadCommand` | Read array data from a file |
| `WRA` | `ArrayWriteCommand` | Write array data to a file |
| `UDA` | `ArrayUpdateCommand` | Update array element |
| `SORT` | `ArraySortCommand` | Sort array elements (ascending or descending) |
| `RMVA` | `ArrayRemoveCommand` | Remove element and shift remaining down |
| `DSPA` | `ArrayDisplayCommand` | Display array contents |
| `DLCA` | `ArrayDeallocateCommand` | Free array memory |

### Dynamic Field & Array Allocation

CoTAS supports runtime field creation:

```tas
fptr = ALC_FLD("DYNAMIC_FIELD", "A", 20)   ; Allocate field
aptr = ALOCARY("DYN_ARRAY", "N", 10, 50)   ; Allocate 50-element array
```

Functions: `ALC_FLD()`, `ALOC()`, `ALOCARY()`, `GET_ELEM_NUM()`, `AEV()`.

## DDF Schema Format

Data Dictionary Files (`.int` files) use a simple key-value format:

```
DATABASE_SPACE_NAME GPacific
TABLE_NAME BKACMOD
SCHEMA_NAME dbo
NUMBER_DF_FIELDS 12
PAGE_SIZE 4096
LOGICAL_RECORD_LENGTH 256
FIELD_NUMBER 1
FIELD_NAME CUSTID
FIELD_NATIVE_TYPE 1
FIELD_NATIVE_LENGTH 8
FIELD_NATIVE_OFFSET 0
INDEX_NUMBER 1
INDEX_SEGMENT_FIELD 1
INDEX_SEGMENT_FLAG 0
```

Native type mapping:

| Code | NativeType | TAS Type |
|------|-----------|----------|
| 0 | String | A (Alpha) |
| 1 | Integer | I (Integer) |
| 2 | Numeric | N (Numeric/Float) |
| 3 | Date | D (Date) |
| 13 | Variable | A (Variable-length) |
| 14 | SmallInt | I (Small integer) |

### Multi-Database Support

CoTAS supports multiple database backends and multiple databases per application via the `DatabaseMap` configuration:

```json
{
  "Storage": {
    "Provider": "mssql",
    "BaseConnectionString": "Server=your-server;TrustServerCertificate=true",
    "DdfDirectory": "db",
    "DefaultSchema": "dbo",
    "DatabaseMap": {
      "MYDATA": "mydata_db",
      "ARCHIVE": "archive_db"
    }
  }
}
```

Supported providers:

- **`mssql`** - Microsoft SQL Server via `Microsoft.Data.SqlClient` (fully supported)
- **`mysql`** - MySQL / MariaDB via `MySqlConnector` (in progress)
- **`postgres`** - PostgreSQL via `Npgsql` (in progress)

All providers use free, open-source ADO.NET libraries - no paid driver licenses required.

The `StorageEngine` preloads all `.int` files from the DDF directory at startup. If no `.int` file exists for a table, it falls back to dynamic schema discovery via `INFORMATION_SCHEMA.COLUMNS`.
