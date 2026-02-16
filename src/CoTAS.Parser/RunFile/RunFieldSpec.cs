namespace CoTAS.Parser.RunFile;

/// <summary>
/// A field definition from a compiled .RUN file.
/// Maps to TFldSpec/TFldSpecOld in the TAS compiler.
/// </summary>
public sealed class RunFieldSpec
{
    /// <summary>Field name (up to 14 characters).</summary>
    public string Name { get; set; } = "";

    /// <summary>Memory offset in the defined data segment.</summary>
    public int Offset { get; set; }

    /// <summary>
    /// Field type character:
    /// A=Alpha, I=Integer, N=Numeric, D=Date, T=Time, L=Logical,
    /// R=Real/Report, B=Byte, P=Packed, S=String(temp), F=Float, O=Object
    /// </summary>
    public char FieldType { get; set; }

    /// <summary>Number of decimal places.</summary>
    public int Decimals { get; set; }

    /// <summary>Display size (characters).</summary>
    public int DisplaySize { get; set; }

    /// <summary>Number of array elements (0 = not an array).</summary>
    public int ArrayCount { get; set; }

    /// <summary>Whether this field comes from an opened data file.</summary>
    public bool IsFileField { get; set; }

    /// <summary>Raw FileFld byte value: 0=memory, N=buffer index+1 (1-based).</summary>
    public byte FileFieldIndex { get; set; }

    /// <summary>Picture format type character.</summary>
    public char PictureType { get; set; }

    /// <summary>Offset to picture format in constant segment.</summary>
    public int PictureLocation { get; set; }

    /// <summary>Whether this field was inherited from a parent program (via CHAIN).</summary>
    public bool IsReset { get; set; }

    /// <summary>Whether to force uppercase input.</summary>
    public bool ForceUpperCase { get; set; }

    /// <summary>Allocation flag.</summary>
    public byte AllocFlag { get; set; }

    /// <summary>Internal storage size in bytes.</summary>
    public int InternalSize { get; set; }

    /// <summary>Key/index number for file fields.</summary>
    public byte KeyNumber { get; set; }

    /// <summary>File buffer number (0-99).</summary>
    public byte FileBufferNumber { get; set; }

    /// <summary>Whether the field is ready for use.</summary>
    public bool IsReady { get; set; }

    /// <summary>Whether the field has an initial VALUE expression.</summary>
    public bool HasInitialValue { get; set; }

    /// <summary>File handle number.</summary>
    public ushort FileHandle { get; set; }

    /// <summary>Whether this is a temp field (TEMP0-TEMP29).</summary>
    public bool IsTempField => Name.StartsWith("TEMP", StringComparison.OrdinalIgnoreCase)
                               && Name.Length <= 6;

    /// <summary>Map the TAS type char to a TAS type string.</summary>
    public string TypeName => FieldType switch
    {
        'A' => "Alpha",
        'I' => "Integer",
        'N' => "Numeric",
        'D' => "Date",
        'T' => "Time",
        'L' => "Logical",
        'R' => "Real",
        'B' => "Byte",
        'P' => "Packed",
        'S' => "String",
        'F' => "Float",
        'O' => "Object",
        _ => $"Unknown({FieldType})"
    };

    public override string ToString()
    {
        string arr = ArrayCount > 0 ? $"[{ArrayCount}]" : "";
        string dec = Decimals > 0 ? $".{Decimals}" : "";
        string file = IsFileField ? $" FILE(buf={FileBufferNumber})" : "";
        string reset = IsReset ? " RESET" : "";
        return $"{Name}{arr} TYPE {TypeName} SIZE {DisplaySize}{dec} ISIZE {InternalSize}{file}{reset}";
    }
}
