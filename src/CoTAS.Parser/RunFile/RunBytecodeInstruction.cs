namespace CoTAS.Parser.RunFile;

/// <summary>
/// A single bytecode instruction from the .RUN file code segment.
/// 8 bytes each. Maps to TPrgPtr.
/// </summary>
public sealed class RunBytecodeInstruction
{
    /// <summary>Command number (opcode).</summary>
    public ushort CommandNumber { get; set; }

    /// <summary>Exit flag.</summary>
    public byte Exit { get; set; }

    /// <summary>Spec line size (number of bytes of parameters in the spec segment).</summary>
    public byte SpecLineSize { get; set; }

    /// <summary>Pointer/offset into the spec segment for this instruction's parameters.</summary>
    public int SpecLinePtr { get; set; }

    /// <summary>Command name from the opcode table.</summary>
    public string CommandName => TasOpcode.GetName(CommandNumber);

    public override string ToString() =>
        $"{CommandName,-12} (#{CommandNumber:D3}) Exit={Exit} SpecSize={SpecLineSize} SpecPtr=0x{SpecLinePtr:X}";
}
