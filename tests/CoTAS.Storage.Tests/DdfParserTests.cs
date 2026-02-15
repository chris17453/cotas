using CoTAS.Storage;

namespace CoTAS.Storage.Tests;

public class DdfParserTests
{
    private const string SampleDdf = """
        DATABASE_SPACE_NAME GPacific
        TABLE_NAME BKACMOD
        SCHEMA_NAME dbo
        NUMBER_DF_FIELDS 3
        PERMANENT_INT NO
        LOCAL_CACHE YES
        FILE_FLAGS 512
        PAGE_SIZE 1024
        LOGICAL_RECORD_LENGTH 62
        IGNORE_NULL_VALUES 1
        TRIM_STRING_FIELDS 1

        FIELD_NUMBER 1
        FIELD_NAME BKRAD_MOD_MAKE
        FIELD_NATIVE_TYPE 0
        FIELD_NATIVE_LENGTH 30
        FIELD_NATIVE_OFFSET 0
        FIELD_INDEX 1

        FIELD_NUMBER 2
        FIELD_NAME BKRAD_MOD_MODEL
        FIELD_NATIVE_TYPE 0
        FIELD_NATIVE_LENGTH 30
        FIELD_NATIVE_OFFSET 30
        FIELD_INDEX 1

        FIELD_NUMBER 3
        FIELD_NAME BKRAD_MOD_YEAR
        FIELD_NATIVE_TYPE 1
        FIELD_NATIVE_LENGTH 2
        FIELD_NATIVE_OFFSET 60
        FIELD_DEFAULT_VALUE 0
        FIELD_INDEX 2

        INDEX_NUMBER 1
        INDEX_NUMBER_SEGMENTS 3
        INDEX_SEGMENT_FIELD 1
        INDEX_SEGMENT_FLAG 275
        INDEX_SEGMENT_FIELD 2
        INDEX_SEGMENT_FLAG 259
        INDEX_SEGMENT_FIELD 0
        INDEX_SEGMENT_FLAG -1
        INDEX_SEGMENT_NULL_VALUE 0

        INDEX_NUMBER 2
        INDEX_NUMBER_SEGMENTS 4
        INDEX_SEGMENT_FIELD 1
        INDEX_SEGMENT_FLAG 275
        INDEX_SEGMENT_FIELD 3
        INDEX_SEGMENT_FLAG 279
        INDEX_SEGMENT_FIELD 2
        INDEX_SEGMENT_FLAG 259
        INDEX_SEGMENT_FIELD 0
        INDEX_SEGMENT_FLAG -1
        INDEX_SEGMENT_NULL_VALUE 0
        """;

    [Fact]
    public void Parse_TableMetadata()
    {
        var schema = DdfParser.ParseText(SampleDdf);

        Assert.Equal("GPacific", schema.DatabaseSpace);
        Assert.Equal("BKACMOD", schema.TableName);
        Assert.Equal("dbo", schema.SchemaName);
        Assert.Equal(3, schema.NumberOfFields);
        Assert.Equal(1024, schema.PageSize);
        Assert.Equal(62, schema.LogicalRecordLength);
    }

    [Fact]
    public void Parse_FieldDefinitions()
    {
        var schema = DdfParser.ParseText(SampleDdf);

        Assert.Equal(3, schema.Fields.Count);

        var f1 = schema.Fields[0];
        Assert.Equal(1, f1.FieldNumber);
        Assert.Equal("BKRAD_MOD_MAKE", f1.Name);
        Assert.Equal(NativeType.String, f1.NativeType);
        Assert.Equal(30, f1.NativeLength);
        Assert.Equal(0, f1.NativeOffset);
        Assert.Equal(1, f1.IndexRef);
        Assert.Equal("TEXT", f1.SqliteType);
        Assert.Equal("A", f1.TasTypeCode);

        var f3 = schema.Fields[2];
        Assert.Equal("BKRAD_MOD_YEAR", f3.Name);
        Assert.Equal(NativeType.Integer, f3.NativeType);
        Assert.Equal(2, f3.NativeLength);
        Assert.Equal("0", f3.DefaultValue);
        Assert.Equal("I", f3.TasTypeCode);
    }

    [Fact]
    public void Parse_IndexDefinitions()
    {
        var schema = DdfParser.ParseText(SampleDdf);

        Assert.Equal(2, schema.Indexes.Count);

        // Index 1: fields 1, 2 (terminator field 0 excluded)
        var idx1 = schema.Indexes[0];
        Assert.Equal(1, idx1.IndexNumber);
        Assert.Equal(2, idx1.Segments.Count);
        Assert.Equal(1, idx1.Segments[0].FieldNumber);
        Assert.Equal(275, idx1.Segments[0].Flag);
        Assert.Equal(2, idx1.Segments[1].FieldNumber);
        Assert.Equal(259, idx1.Segments[1].Flag);

        // Index 2: fields 1, 3, 2 (terminator excluded)
        var idx2 = schema.Indexes[1];
        Assert.Equal(2, idx2.IndexNumber);
        Assert.Equal(3, idx2.Segments.Count);
    }

    [Fact]
    public void Parse_NumericField()
    {
        string ddf = """
            DATABASE_SPACE_NAME Test
            TABLE_NAME TESTFILE
            SCHEMA_NAME dbo
            NUMBER_DF_FIELDS 2
            PAGE_SIZE 4096
            LOGICAL_RECORD_LENGTH 18

            FIELD_NUMBER 1
            FIELD_NAME TEST_NAME
            FIELD_NATIVE_TYPE 0
            FIELD_NATIVE_LENGTH 10
            FIELD_NATIVE_OFFSET 0

            FIELD_NUMBER 2
            FIELD_NAME TEST_AMOUNT
            FIELD_NATIVE_TYPE 2
            FIELD_NATIVE_LENGTH 8
            FIELD_NATIVE_OFFSET 10
            FIELD_DEFAULT_VALUE 0
            """;

        var schema = DdfParser.ParseText(ddf);
        Assert.Equal(2, schema.Fields.Count);

        var numField = schema.Fields[1];
        Assert.Equal(NativeType.Numeric, numField.NativeType);
        Assert.Equal("REAL", numField.SqliteType);
        Assert.Equal("N", numField.TasTypeCode);
    }

    [Fact]
    public void Parse_DateField()
    {
        string ddf = """
            DATABASE_SPACE_NAME Test
            TABLE_NAME TESTDATE
            SCHEMA_NAME dbo
            NUMBER_DF_FIELDS 1
            PAGE_SIZE 4096
            LOGICAL_RECORD_LENGTH 4

            FIELD_NUMBER 1
            FIELD_NAME MY_DATE
            FIELD_NATIVE_TYPE 3
            FIELD_NATIVE_LENGTH 4
            FIELD_NATIVE_OFFSET 0
            FIELD_DEFAULT_VALUE 0001-01-01
            """;

        var schema = DdfParser.ParseText(ddf);
        var dateField = schema.Fields[0];
        Assert.Equal(NativeType.Date, dateField.NativeType);
        Assert.Equal("D", dateField.TasTypeCode);
        Assert.Equal("0001-01-01", dateField.DefaultValue);
    }

    [Fact]
    public void Parse_RealDdfFile()
    {
        // Test with a real .int file from the db directory
        string ddfDir = Path.Combine(FindRepoRoot(), "db", "GPACIFIC");
        if (!Directory.Exists(ddfDir))
            return; // Skip if not available

        string? intFile = DdfParser.FindIntFile(ddfDir, "BKACMOD");
        Assert.NotNull(intFile);

        var parser = new DdfParser();
        var schema = parser.Parse(intFile);
        Assert.Equal("BKACMOD", schema.TableName);
        Assert.Equal(3, schema.Fields.Count);
        Assert.True(schema.Indexes.Count >= 1);
    }

    [Fact]
    public void FindIntFile_ExactMatch()
    {
        string ddfDir = Path.Combine(FindRepoRoot(), "db", "GPACIFIC");
        if (!Directory.Exists(ddfDir))
            return;

        // Should find by TABLE_NAME scan since file is "bkacmod_.int"
        string? found = DdfParser.FindIntFile(ddfDir, "BKACMOD");
        Assert.NotNull(found);
    }

    [Fact]
    public void ParseText_NoIndexes()
    {
        string ddf = """
            DATABASE_SPACE_NAME Test
            TABLE_NAME SIMPLE
            SCHEMA_NAME dbo
            NUMBER_DF_FIELDS 1
            PAGE_SIZE 4096
            LOGICAL_RECORD_LENGTH 10

            FIELD_NUMBER 1
            FIELD_NAME MY_FIELD
            FIELD_NATIVE_TYPE 0
            FIELD_NATIVE_LENGTH 10
            FIELD_NATIVE_OFFSET 0
            """;

        var schema = DdfParser.ParseText(ddf);
        Assert.Single(schema.Fields);
        Assert.Empty(schema.Indexes);
    }

    private static string FindRepoRoot()
    {
        string dir = AppContext.BaseDirectory;
        while (dir != null)
        {
            if (File.Exists(Path.Combine(dir, "CoTAS.sln")))
                return dir;
            dir = Path.GetDirectoryName(dir)!;
        }
        return AppContext.BaseDirectory;
    }
}
