namespace CoTAS.Parser.RunFile;

/// <summary>
/// TAS .RUN file header (TPci). 128 bytes at the start of every .RUN file.
/// All multi-byte values are little-endian (x86).
/// </summary>
public sealed class RunFileHeader
{
    public int CodeSize { get; set; }         // offset 0: run code segment size
    public int ConstSize { get; set; }        // offset 4: constant segment size
    public int SpecSize { get; set; }         // offset 8: spec segment size
    public int LabelSize { get; set; }        // offset 12: label segment size
    public int ScrnFldNum { get; set; }       // offset 16: number of screen fields
    public int NumFlds { get; set; }          // offset 20: total number of fields
    public int TempFlds { get; set; }         // offset 24: temp field data area end offset
    public int NumTempFlds { get; set; }      // offset 28: number of temp fields
    public int FldNameSize { get; set; }      // offset 32: field spec segment size (= NumFlds * 48)
    public int TempFldSize { get; set; }      // offset 36: temp field data area size
    public int DefFldSegSize { get; set; }    // offset 40: defined field data segment size
    public int NumExtraFlds { get; set; }     // offset 44: extra field slots available at runtime
    public int PrgNames { get; set; }         // offset 48: program name buffer size
    public bool DebugFlg { get; set; }        // offset 52: debug mode flag
    public string ProType { get; set; } = ""; // offset 53: signature ("TAS32" or "TASWN")
    public int NumLabels { get; set; }        // offset 58: number of labels
    public bool NewFldSpec { get; set; }      // offset 62: uses new TFldSpec format (60 bytes vs 48)
    public bool ChkUpVld { get; set; }        // offset 63: check up arrow validation
    public bool IncLabels { get; set; }       // offset 64: include label names in run file
    public bool ObjUsed { get; set; }         // offset 78: compiled with OBJ file (overlay spec/const are combined)

    /// <summary>Size of the header in bytes.</summary>
    public const int Size = 128;

    /// <summary>Size of the file buffer list (100 entries × 16 bytes).</summary>
    public const int BufferListSize = 1600;

    /// <summary>Field spec size for old format (TAS 5.1 DOS).</summary>
    public const int OldFieldSpecSize = 48;

    /// <summary>Field spec size for new format (TAS 6.0 Windows).</summary>
    public const int NewFieldSpecSize = 60;

    /// <summary>Instruction size for TAS 5.1 (TAS32): 7 bytes (word CmdNum + byte SLSize + int32 SLPtr).</summary>
    public const int Tas51InstructionSize = 7;

    /// <summary>Instruction size for TAS 6.0 (TASWN): 8 bytes (word CmdNum + byte Exit + byte SLSize + int32 SLPtr).</summary>
    public const int Tas60InstructionSize = 8;

    /// <summary>Actual field spec size based on NewFldSpec flag.</summary>
    public int FieldSpecSize => NewFldSpec ? NewFieldSpecSize : OldFieldSpecSize;

    /// <summary>Offset where the run code segment starts.</summary>
    public int CodeOffset => Size + BufferListSize;

    /// <summary>Offset where the constant segment starts.</summary>
    public int ConstOffset => CodeOffset + CodeSize;

    /// <summary>Offset where the spec segment starts.</summary>
    public int SpecOffset => ConstOffset + ConstSize;

    /// <summary>Offset where the label segment starts.</summary>
    public int LabelOffset => SpecOffset + SpecSize;

    /// <summary>Offset where the field spec segment starts.</summary>
    public int FieldSpecOffset => LabelOffset + LabelSize;
}
