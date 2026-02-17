// AUTO-GENERATED from Pascal source by generate_compiler_tables.py
// DO NOT EDIT MANUALLY

namespace CoTAS.Compiler;

/// <summary>Compilation primitive actions from the Pascal compiler.</summary>
public enum CompileAction
{
    None = 0,
    PE = 1,
    PF = 2,
    CF = 3,
    LL = 4,
    CFile = 5,
    CFile2 = 6,
    ChkYn = 7,
    ChkNy = 8,
    ChkChr = 9,
    CFAll = 10,
    CSlvRel = 11,
    CFAllRfo = 12,
    ChkMem = 13,
    CTrace = 14,
    ChkTrap = 15,
    IfLL = 16,
    ChkFFld = 17,
    ChkRelFld = 18,
    ChkElbl = 19,
    CKey = 20,
    CFMount = 21,
    CSchema = 22,
    ChkChr1 = 23,
    CAt = 24,
    SetY = 25,
    SetN = 26,
    ChkChr2 = 27,
    CScope = 28,
    CUfc = 29,
    CTops = 30,
    CUpdaVal = 31,
    CUfcCf = 32,
    LUdf = 33,
    CFAllNoExp = 34,
    CCaVal = 35,
    Cc1PE = 36,
    ChkChrFt = 37,

    // Special compilation routines (control flow)
    DefineFld = 101,
    CompIf = 102,
    CompElse = 103,
    CompElseIf = 104,
    CompWhile = 105,
    CompEndWhile = 106,
    CompWhileLoop = 107,
    CompWhileExit = 108,
    CompWhileLoopIf = 109,
    CompWhileExitIf = 110,
    CompSelect = 111,
    CompCase = 112,
    CompOtherwise = 113,
    CompEndCase = 114,
    CompFor = 115,
    CompNext = 116,
    CompFloop = 117,
    CompFexit = 118,
    CompFloopIf = 119,
    CompFexitIf = 120,
    CompUdf = 121,
    CompUdc = 122,
    CompScan = 123,
    CompScanExit = 124,
    CompScanExitIf = 125,
    CompScanLoop = 126,
    CompScanLoopIf = 127,
    CompScanEnd = 128,
    CompIfdup = 129,
    CompIfna = 130,
    StartBrace = 131,
    StopBrace = 132,
    SetupMount = 133,
    SetupRemount = 134,
    ParseOnGoto = 135,
    GetScrnFormat = 136,
}

/// <summary>One option spec for a command (maps keyword to compile action + spec offset).</summary>
public readonly record struct OptionDef(
    int OptionIndex,       // Index into OptionTable.Keywords (0 = positional/first arg)
    CompileAction Action,  // What compilation primitive to use
    int SpecOffset,        // Byte offset in spec line to write result
    string? CharList = null // Valid characters for chk_chr-type actions
);

/// <summary>Default value spec for unset options.</summary>
public readonly record struct DefaultDef(
    CompileAction Action,  // Default action (chk_yn, chk_ny, chk_chr, 0=none)
    int SpecOffset,        // Byte offset in spec line
    string? CharList = null // Default char for chk_chr
);

/// <summary>Complete command definition (mirrors Pascal TCmdStruct).</summary>
public sealed class CommandDef
{
    public required string Name { get; init; }
    public required int Opcode { get; init; }
    public required int SpecSize { get; init; }
    public OptionDef[] Options { get; init; } = [];
    public DefaultDef[] Defaults { get; init; } = [];
    public bool IsSpecial { get; init; }
}