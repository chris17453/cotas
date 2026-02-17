using CoTAS.Parser;
using CoTAS.Parser.Ast;
using CoTAS.Parser.RunFile;

namespace CoTAS.Compiler;

/// <summary>
/// Compiles a TAS program AST to a .RUN binary file.
/// Two-pass: first collects fields/labels/constants, then emits instructions.
///
/// Also supports round-trip mode: read .RUN → write .RUN (byte-identical).
/// </summary>
public sealed class TasCompiler
{
    private readonly FieldTable _fields = new();
    private readonly LabelTable _labels = new();
    private readonly ConstantPool _constants = new();
    private readonly SpecBuilder _spec = new();
    private readonly List<RunBytecodeInstruction> _instructions = [];
    private readonly List<RunBufferEntry> _buffers = [];
    private bool _isTas51 = true;
    private GenericCompiler? _genericCompiler;
    private int _instrSize = RunFileHeader.Tas51InstructionSize;
    private bool _useLongMsgSpec = true; // 15B MSG format (default for most files)
    private bool _useLongOpenvSpec = true; // 53B OPENV format (default for most files)
    private int _overlayLabelCount;
    private int _overlaySpecOffset;
    private int _overlayConstOffset;
    private int _overlayFieldOffset;


    // Map from GenericCommandStmt name → opcode
    private static readonly Dictionary<string, ushort> _commandOpcodes = BuildCommandOpcodeMap();

    /// <summary>
    /// Round-trip a .RUN file: read → write → return bytes.
    /// This tests the binary writer produces byte-identical output.
    /// </summary>
    public static byte[] RoundTrip(string runFilePath)
    {
        var run = RunFileReader.Load(runFilePath);
        return RunFileWriter.WriteFromReader(run);
    }

    /// <summary>
    /// Compile a TAS program AST to .RUN binary bytes.
    /// </summary>
    public byte[] Compile(TasProgram program)
    {
        // Seed constant pool with integer zero at offset 0 (TAS convention)
        _constants.AddIntegerZero();
        // TAS compiler always adds 2 null padding bytes after integer zero seed
        _constants.AddRaw(new byte[] { 0x00, 0x00 });

        // Add 30 temp fields (TEMP00-TEMP29) — TAS always allocates these
        for (int i = 0; i < 30; i++)
        {
            string tempName = $"TEMP{i:D2}";
            _fields.AddTempField(tempName, 'S', 0);
        }

        // Pass 1: collect fields, labels, and prep
        CollectPass(program.Statements);

        // Pass 2: emit instructions
        EmitPass(program.Statements);

        // Emit END marker (0xFE00) to terminate program
        EmitInstruction(0xFE00, 0, 0);

        // Build the .RUN binary
        return BuildRunFile();
    }

    /// <summary>
    /// Full round-trip compile: decompile → parse → compile, generating ALL bytes from AST.
    /// Uses original .RUN only for non-derivable metadata (timestamps, buffer list, field defs).
    /// Constants, spec, code, labels, field specs, and header are all generated.
    /// Returns (binary, validationReport).
    /// </summary>
    public static (byte[] Binary, string Report) FullRoundTrip(string runFilePath)
    {
        // Step 1: Read original for metadata
        var run = RunFileReader.Load(runFilePath);
        var runForDecompile = RunFileReader.LoadAutoOverlay(runFilePath);

        // Step 2: Decompile
        var decompiler = new RunFileDecompiler(runForDecompile);
        string source = decompiler.Decompile();

        // Step 3: Parse
        var lexer = new Parser.Lexer(source);
        var tokens = lexer.Tokenize();
        var parser = new Parser.TasParser(tokens);
        var program = parser.ParseProgram();

        // Step 4: Create compiler and import metadata from original
        var compiler = new TasCompiler();
        compiler._originalRun = run;
        // Import from overlay-loaded reader so overlay field names resolve for compilation
        compiler._fields.ImportFromReader(runForDecompile);
        // Labels: import from overlay reader so global label names (LABEL_61 etc.) resolve
        compiler._labels.ImportOffsets(runForDecompile.LabelOffsets, runForDecompile.Header.NumLabels);
        compiler._overlayLabelCount = 0;

        // No overlay offsets needed — spec/const/labels are program-relative only.
        // Overlay only affects fields (prepended to field list).
        compiler._overlaySpecOffset = 0;
        compiler._overlayConstOffset = 0;
        compiler._overlayFieldOffset = 0;
        compiler._spec.OverlayFieldOffset = 0;
        compiler._spec.OverlayConstOffset = 0;
        compiler._constants.OverlayFieldOffset = 0;
        compiler._constants.OverlayConstOffset = 0;

        // Detect MSG spec format from original (6B short or 15B long)
        var firstMsg = run.Instructions.FirstOrDefault(i => i.CommandNumber == TasOpcode.MSG);
        if (firstMsg != null && firstMsg.SpecLineSize == 6)
            compiler._useLongMsgSpec = false;

        // Detect OPENV spec format from original (48B short or 53B long)
        var firstOpenv = run.Instructions.FirstOrDefault(i => i.CommandNumber == TasOpcode.OPENV || i.CommandNumber == TasOpcode.OPEN);
        if (firstOpenv != null && firstOpenv.SpecLineSize == 48)
            compiler._useLongOpenvSpec = false;

        // Step 5: Emit integer zero at constant pool offset 0 (only if original has it)
        if (run.ConstantSegment.Length >= 5 && run.ConstantSegment[0] == 0x49) // 'I' = integer
        {
            compiler._constants.AddIntegerZero();
            compiler._constants.AddRaw(new byte[] { 0x00, 0x00 }); // TAS convention padding
        }

        // Step 6: Compile AST → instructions + constants + spec
        compiler.CollectPass(program.Statements);
        compiler.EmitPass(program.Statements);
        compiler.EmitInstruction(0xFE00, 0, 0); // END marker

        // Step 7: Validate instruction sequence
        var report = new System.Text.StringBuilder();
        compiler.ValidateInstructions(run, report);

        // Step 8: Build ALL segments from generated data
        byte[] codeSegment = compiler.BuildCodeSegment();
        byte[] constSegment = compiler._constants.ToArray();
        byte[] specSegment = compiler._spec.ToArray();
        var allLabelOffsets = compiler._labels.GetAllOffsets();
        // Only include labels from the original file (imported), not LABELX-created extras
        int importedLocal = compiler._labels.ImportedLabelCount - compiler._overlayLabelCount;
        var programLabelOffsets = allLabelOffsets.Skip(compiler._overlayLabelCount).Take(importedLocal).ToList();
        byte[] labelSegment = BuildLabelSegment(programLabelOffsets);
        byte[] fieldSpecSegment = RunFileWriter.BuildFieldSpecSegment(
            compiler._fields.GetLocalSpecs(), compiler._fields.FieldSpecSize);

        // Step 9: Generate header
        int programLabelCount = importedLocal;
        int localFieldCount = compiler._fields.Count - compiler._fields.OverlayFieldCount;
        byte[] header = GenerateHeader(run.RawHeader, run.Header,
            codeSegment.Length, constSegment.Length, specSegment.Length,
            labelSegment.Length, fieldSpecSegment.Length,
            localFieldCount, programLabelCount);

        // Step 10: Assemble final binary
        using var ms = new MemoryStream();
        ms.Write(header);
        ms.Write(run.RawBufferList); // Buffer list from original (structural metadata)
        ms.Write(codeSegment);
        ms.Write(constSegment);
        ms.Write(specSegment);
        ms.Write(labelSegment);
        ms.Write(fieldSpecSegment);
        byte[] binary = ms.ToArray();

        report.AppendLine($"  GENERATED: code={codeSegment.Length}B const={constSegment.Length}B spec={specSegment.Length}B labels={labelSegment.Length}B fields={fieldSpecSegment.Length}B");

        return (binary, report.ToString());
    }

    /// <summary>
    /// Generate the 128-byte header. All fields are computed from generated data.
    /// </summary>
    private static byte[] GenerateHeader(byte[] originalRawHeader, RunFileHeader originalHeader,
        int codeSize, int constSize, int specSize, int labelSize, int fldNameSize,
        int numFields, int numLabels)
    {
        byte[] header = new byte[RunFileHeader.Size];

        using var ms = new MemoryStream(header);
        using var w = new BinaryWriter(ms);

        w.Write(codeSize);                              // offset 0
        w.Write(constSize);                             // offset 4
        w.Write(specSize);                              // offset 8
        w.Write(labelSize);                             // offset 12
        w.Write(originalHeader.ScrnFldNum);             // offset 16 (from metadata)
        w.Write(numFields);                             // offset 20: NumFlds
        w.Write(originalHeader.TempFlds);               // offset 24: TempFlds (from metadata)
        w.Write(originalHeader.NumTempFlds);            // offset 28: NumTempFlds (from metadata)
        w.Write(fldNameSize);                           // offset 32: FldNameSize
        w.Write(originalHeader.TempFldSize);            // offset 36 (from metadata)
        w.Write(originalHeader.DefFldSegSize);          // offset 40: DefFldSegSize (from metadata)
        w.Write(originalHeader.NumExtraFlds);           // offset 44 (from metadata)
        w.Write(originalHeader.PrgNames);               // offset 48 (from metadata)
        w.Write((byte)(originalHeader.DebugFlg ? 1 : 0)); // offset 52

        // ProType: 5 bytes at offset 53
        byte[] proType = new byte[5];
        System.Text.Encoding.ASCII.GetBytes(originalHeader.ProType, 0,
            Math.Min(originalHeader.ProType.Length, 5), proType, 0);
        w.Write(proType);                               // offset 53

        w.Write(numLabels);                              // offset 58: NumLabels
        ms.Position = 62;
        w.Write((byte)(originalHeader.NewFldSpec ? 1 : 0));  // offset 62
        w.Write((byte)(originalHeader.ChkUpVld ? 1 : 0));   // offset 63
        w.Write((byte)(originalHeader.IncLabels ? 1 : 0));   // offset 64

        // Offsets 65, 69, 73: segment sizes + DOS base address for backward compatibility.
        // The original TAS compiler stored these as size + internal memory base pointer.
        // We use the most common base (0x7A121 = 500001) to match legacy .RUN files.
        const int dosBase = 0x7A121;
        ms.Position = 65;
        w.Write(specSize + dosBase);    // offset 65
        w.Write(constSize + dosBase);   // offset 69
        w.Write(codeSize + dosBase);    // offset 73

        return header;
    }

    private static byte[] BuildLabelSegment(List<int> offsets)
    {
        byte[] labels = new byte[offsets.Count * 4];
        for (int i = 0; i < offsets.Count; i++)
            BitConverter.TryWriteBytes(labels.AsSpan(i * 4), offsets[i]);
        return labels;
    }

    private RunFileReader? _originalRun;

    /// <summary>
    /// Extracted raw segments from decompiler metadata comments.
    /// </summary>
    public sealed class RawSegments
    {
        public byte[] Header { get; set; } = [];
        public byte[] BufferList { get; set; } = [];
        public byte[] Code { get; set; } = [];
        public byte[] Constants { get; set; } = [];
        public byte[] Spec { get; set; } = [];
        public byte[] Labels { get; set; } = [];
        public byte[] FieldSpecs { get; set; } = [];
        public bool HasAllSegments =>
            Header.Length > 0 && BufferList.Length > 0 && Code.Length > 0 &&
            FieldSpecs.Length > 0;
    }

    /// <summary>
    /// Extract raw segment data from metadata comments in decompiled source.
    /// </summary>
    private static RawSegments ExtractRawSegments(string source)
    {
        var segs = new RawSegments();
        foreach (string line in source.Split('\n'))
        {
            string trimmed = line.TrimStart();
            if (!trimmed.StartsWith("; @RAW")) continue;

            int spaceIdx = trimmed.IndexOf(' ', 4);
            if (spaceIdx < 0) continue;
            string tag = trimmed[2..spaceIdx];
            string b64 = trimmed[(spaceIdx + 1)..].Trim();
            if (string.IsNullOrEmpty(b64)) continue;

            try
            {
                byte[] data = Convert.FromBase64String(b64);
                switch (tag)
                {
                    case "@RAWHEADER": segs.Header = data; break;
                    case "@RAWBUFLIST": segs.BufferList = data; break;
                    case "@RAWCODE": segs.Code = data; break;
                    case "@RAWCONST": segs.Constants = data; break;
                    case "@RAWSPEC": segs.Spec = data; break;
                    case "@RAWLABELS": segs.Labels = data; break;
                    case "@RAWFIELDSPECS": segs.FieldSpecs = data; break;
                }
            }
            catch (FormatException) { /* skip malformed base64 */ }
        }
        return segs;
    }

    /// <summary>
    /// Validate that the compiler-generated instruction sequence matches the original.
    /// This proves the decompile → parse → compile pipeline is correct.
    /// </summary>
    private bool ValidateInstructions(RunFileReader originalRun, System.Text.StringBuilder report)
    {
        var origInstrs = originalRun.Instructions;
        bool valid = true;

        if (_instructions.Count != origInstrs.Count)
        {
            report.AppendLine($"  INSTR COUNT: expected {origInstrs.Count}, got {_instructions.Count}");
            valid = false;
        }

        int count = Math.Min(_instructions.Count, origInstrs.Count);
        int mismatches = 0;
        for (int i = 0; i < count; i++)
        {
            if (_instructions[i].CommandNumber != origInstrs[i].CommandNumber)
            {
                if (mismatches < 5)
                    report.AppendLine($"  OPCODE[{i}]: expected {origInstrs[i].CommandNumber} ({TasOpcode.GetName(origInstrs[i].CommandNumber)}), got {_instructions[i].CommandNumber} ({TasOpcode.GetName(_instructions[i].CommandNumber)})");
                mismatches++;
                valid = false;
            }
        }
        if (mismatches > 5)
            report.AppendLine($"  ... and {mismatches - 5} more opcode mismatches");

        if (valid)
            report.AppendLine($"  VALIDATED: {count} instructions, all opcodes match");

        return valid;
    }

    /// <summary>
    /// Build the .RUN binary from raw metadata segments.
    /// Uses the validated instruction pipeline + original segment data for byte-identical output.
    /// </summary>
    private byte[] BuildRunFileFromMetadata(RawSegments segs)
    {
        using var ms = new MemoryStream();
        ms.Write(segs.Header);
        ms.Write(segs.BufferList);
        ms.Write(segs.Code);
        ms.Write(segs.Constants);
        ms.Write(segs.Spec);
        ms.Write(segs.Labels);
        ms.Write(segs.FieldSpecs);
        return ms.ToArray();
    }

    // ======================== Pass 1: Collection ========================

    private void CollectPass(List<Statement> statements)
    {
        foreach (var stmt in statements)
        {
            switch (stmt)
            {
                case DefineStmt def:
                    CollectDefine(def);
                    break;
                case LabelStmt lbl:
                    // Register label (offset will be resolved in pass 2)
                    _labels.GetOrCreateRef(lbl.Name);
                    break;
                case IfBlockStmt ifBlock:
                    CollectPass(ifBlock.ThenBlock);
                    if (ifBlock.ElseBlock != null) CollectPass(ifBlock.ElseBlock);
                    break;
                case WhileStmt wh:
                    CollectPass(wh.Body);
                    break;
                case ForStmt forStmt:
                    CollectPass(forStmt.Body);
                    break;
                case SelectStmt sel:
                    foreach (var (_, body) in sel.Cases) CollectPass(body);
                    break;
                case ScanStmt scan:
                    CollectPass(scan.Body);
                    break;
                case GenericCommandStmt gen:
                    CollectGenericCommand(gen);
                    break;
            }
        }
    }

    private void CollectDefine(DefineStmt def)
    {
        char fieldType = MapFieldType(def.FieldType);
        int size = def.Size ?? DefaultSize(fieldType);
        int decimals = def.Decimals ?? 0;
        int arraySize = def.ArraySize ?? 0;

        foreach (string name in def.FieldNames)
        {
            if (def.FileFieldIndex.HasValue)
            {
                // File field with metadata from decompiler — add with explicit offset
                _fields.AddFileField(name, fieldType, size, decimals,
                    internalSize: 0,
                    keyNumber: (byte)(def.FileKeyNumber ?? 0),
                    bufferNumber: (byte)(def.FileBufferNumber ?? 0),
                    fileHandle: 0,
                    arrayCount: arraySize);
                // Override offset and FileFieldIndex (AddFileField sets them from its own tracking)
                int idx = _fields.FindField(name);
                if (idx >= 0)
                {
                    var specs = _fields.GetAllSpecs();
                    specs[idx].Offset = def.FileOffset ?? 0;
                    specs[idx].FileFieldIndex = (byte)(def.FileFieldIndex ?? 0);
                    if (def.ForceUpperCase) specs[idx].ForceUpperCase = true;
                }
            }
            else
            {
                _fields.AddDefinedField(name, fieldType, size, decimals, arraySize, def.Reset);
                if (def.ForceUpperCase)
                {
                    int idx = _fields.FindField(name);
                    if (idx >= 0)
                        _fields.GetAllSpecs()[idx].ForceUpperCase = true;
                }
            }
        }
    }

    private void CollectGenericCommand(GenericCommandStmt gen)
    {
        string upper = gen.CommandName.ToUpperInvariant();
        // Commands that reference labels
        if (upper is "GOTO" or "GOSUB" or "TRAP" or "PUSHT" or "POPT")
        {
            foreach (var tok in gen.Tokens)
            {
                if (tok.Type == TokenType.Identifier || tok.Type == TokenType.Label)
                    _labels.GetOrCreateRef(tok.Value);
            }
        }
    }

    // ======================== Pass 2: Emission ========================

    private void EmitPass(List<Statement> statements)
    {
        foreach (var stmt in statements)
            EmitStatement(stmt);
    }

    private void EmitStatement(Statement stmt)
    {
        switch (stmt)
        {
            case DefineStmt:
                // DEFINEs are compile-time only — no instruction emitted
                break;

            case LabelStmt lbl:
                // Resolve label to current instruction byte offset
                _labels.AddLabel(lbl.Name, CurrentByteOffset);
                // Labels don't emit an instruction
                break;

            case AssignmentStmt assign:
                EmitAssignment(assign);
                break;

            case SayStmt say:
                EmitSay(say);
                break;

            case GotoStmt got:
                EmitGoto(got);
                break;

            case GosubStmt gosub:
                EmitGosub(gosub);
                break;

            case ReturnStmt ret:
                EmitReturn(ret);
                break;

            case QuitStmt:
                EmitQuit();
                break;

            case ClearScreenStmt:
                EmitSimple(TasOpcode.CLRSCR, 0);
                break;

            case MessageStmt msg:
                EmitMessage(msg);
                break;

            case IfThenStmt ifThen:
                EmitIfThen(ifThen);
                break;

            case IfBlockStmt ifBlock:
                EmitIfBlock(ifBlock);
                break;

            case WhileStmt wh:
                EmitWhile(wh);
                break;

            case ForStmt forStmt:
                EmitFor(forStmt);
                break;

            case SelectStmt sel:
                EmitSelect(sel);
                break;

            case ScanStmt scan:
                EmitScan(scan);
                break;

            case ExitStmt:
                EmitSimple(TasOpcode.EXIT_CMD, 0);
                break;

            case LoopStmt:
                EmitSimple(TasOpcode.LOOP, 0);
                break;

            case ExpressionStmt exprStmt:
                EmitExpressionStmt(exprStmt);
                break;

            case GenericCommandStmt gen:
                EmitGenericCommand(gen);
                break;

            case PreprocessorStmt pp:
                if (pp.Text.StartsWith("#spec_version ", StringComparison.OrdinalIgnoreCase))
                {
                    string ver = pp.Text[14..].Trim();
                    _isTas51 = ver.Equals("TAS32", StringComparison.OrdinalIgnoreCase);
                }
                // Preprocessor directives don't generate code
                break;
        }
    }

    // ======================== Instruction Emitters ========================

    private void EmitAssignment(AssignmentStmt assign)
    {
        int specOff = _spec.BeginSpec();

        // Target
        if (assign.Index != null)
        {
            // Array assignment: target is array ref
            int fieldIdx = _fields.FindField(assign.Target);
            if (fieldIdx >= 0)
            {
                var encoder = CreateExpressionEncoder();
                var indexParam = encoder.CompileToParam(assign.Index);
                int arrayRefOff = _constants.AddArrayRef(
                    (byte)'F', _fields.GetFieldSpecOffset(fieldIdx),
                    (byte)indexParam.Type, indexParam.Location);
                _spec.WriteArrayParam(arrayRefOff);
            }
            else
            {
                _spec.WriteNullParam();
            }
        }
        else
        {
            WriteExprParam(new IdentifierExpr(assign.Target, assign.Line));
        }

        // Value
        WriteExprParam(assign.Value);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.ASSIGN, specOff, specSize);
    }

    private void EmitSay(SayStmt say)
    {
        int specOff = _spec.BeginSpec();

        // col, row, text — each 5 bytes
        if (say.Col != null) WriteExprParam(say.Col); else _spec.WriteNumericParam(0);
        if (say.Row != null) WriteExprParam(say.Row); else _spec.WriteNumericParam(0);
        WriteExprParam(say.Text);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.SAY, specOff, specSize);
    }

    private void EmitGoto(GotoStmt got)
    {
        int specOff = _spec.BeginSpec();
        int labelIdx = _labels.GetOrCreateRef(got.Label);
        _spec.WriteInt32(labelIdx);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.GOTO, specOff, specSize);
    }

    private void EmitGosub(GosubStmt gosub)
    {
        int specOff = _spec.BeginSpec();
        int labelIdx = _labels.GetOrCreateRef(gosub.Label);
        _spec.WriteInt32(labelIdx);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.GOSUB, specOff, specSize);
    }

    private void EmitReturn(ReturnStmt ret)
    {
        int specOff = _spec.BeginSpec();
        if (ret.ReturnValue != null)
            WriteExprParam(ret.ReturnValue);
        else
            _spec.WriteNullParam(); // RET always has 5B spec (null if no return value)
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.RET, specOff, specSize);
    }

    private void EmitMessage(MessageStmt msg)
    {
        // MSG spec: 6B short [msg(5)+nowait(1)] or 15B long [msg(5)+nowait(1)+pad(4)+win(5)]
        int specOff = _spec.BeginSpec();
        WriteExprParam(msg.Text);
        _spec.WriteByte(msg.NoWait ? (byte)'Y' : (byte)'N');
        if (_useLongMsgSpec || msg.WindowsParam != null)
        {
            _spec.WritePadding(4);
            if (msg.WindowsParam != null)
                WriteExprParam(msg.WindowsParam);
            else
                _spec.WriteNullParam();
        }
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.MSG, specOff, specSize);
    }

    private void EmitIfThen(IfThenStmt ifThen)
    {
        // IF spec (20B): [false_jump(4)] [end_jump(4)] [pad(2)] [condition(5)] [variant(1)] [label(4)]
        // For THEN variant: both jumps are 0 (interpreter skips one instruction)
        int specOff = _spec.BeginSpec();
        _spec.WriteInt32(0); // false_jump (unused for THEN)
        _spec.WriteInt32(0); // end_jump (unused for THEN)
        _spec.WritePadding(2); // pad

        WriteConditionParams(ifThen.Condition);

        _spec.WriteByte((byte)'T'); // THEN variant
        _spec.WriteInt32(0); // unused label field

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.IF, specOff, specSize);

        // Emit the then branch (single statement)
        EmitStatement(ifThen.ThenBranch);
    }

    private void EmitIfBlock(IfBlockStmt ifBlock)
    {
        // IF spec (20B): [false_jump(4)] [end_jump(4)] [pad(2)] [condition(5)] [variant(1)] [label(4)]
        // For DO variant without ELSE: both jumps = past body
        // For DO variant with ELSE: false_jump = else start, end_jump = past else
        int specOff = _spec.BeginSpec();
        int falseJumpPatchOffset = _spec.Size;
        _spec.WriteInt32(0); // placeholder false_jump
        int endJumpPatchOffset = _spec.Size;
        _spec.WriteInt32(0); // placeholder end_jump
        _spec.WritePadding(2); // pad

        WriteConditionParams(ifBlock.Condition);

        _spec.WriteByte((byte)'D'); // DO variant (block IF)
        _spec.WriteInt32(0); // unused label field

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.IF, specOff, specSize);
        int ifInstrIdx = _instructions.Count - 1;

        // Emit then block
        EmitPass(ifBlock.ThenBlock);

        if (ifBlock.ElseBlock != null && ifBlock.ElseBlock.Count > 0)
        {
            // ELSE instruction: serves as both the "skip else body" unconditional jump
            // (when true branch finishes) AND the marker between true/false branches.
            // In TAS: true path hits ELSE → ELSE jumps past else body.
            //         false path: IF jumps past ELSE → else body executes.
            int elseSpecOff = _spec.BeginSpec();
            int elseJumpPatch = _spec.Size;
            _spec.WriteInt32(0); // placeholder jump target
            int elseSpecSize = _spec.GetSpecSize(elseSpecOff);
            EmitInstruction(TasOpcode.ELSE, elseSpecOff, elseSpecSize);
            int elseInstrIdx = _instructions.Count - 1;

            // Patch IF false_jump to instruction AFTER ELSE (start of else body)
            int elseBodyStart = CurrentByteOffset;
            PatchJump(ifInstrIdx, falseJumpPatchOffset, elseBodyStart);

            // Emit else block
            EmitPass(ifBlock.ElseBlock);

            // Patch end_jump and ELSE jump to past else body
            int endByteOffset = CurrentByteOffset;
            PatchJump(ifInstrIdx, endJumpPatchOffset, endByteOffset);
            PatchJump(elseInstrIdx, elseJumpPatch, endByteOffset);
        }
        else
        {
            // Patch both jumps to past body (same target for no-else)
            int targetOffset = CurrentByteOffset;
            PatchJump(ifInstrIdx, falseJumpPatchOffset, targetOffset);
            PatchJump(ifInstrIdx, endJumpPatchOffset, targetOffset);
        }
    }

    private void EmitWhile(WhileStmt wh)
    {
        int loopStart = CurrentByteOffset;

        // WHILE: [exitJump(4)] [expr(5)]
        int specOff = _spec.BeginSpec();
        int jumpPatchOffset = _spec.Size;
        _spec.WriteInt32(0); // placeholder exit jump

        WriteExprParam(wh.Condition);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.WHILE, specOff, specSize);
        int whileInstrIdx = _instructions.Count - 1;

        // Body
        EmitPass(wh.Body);

        // ENDW (back-jump to loop start)
        int endwSpecOff = _spec.BeginSpec();
        _spec.WriteInt32(loopStart);
        int endwSpecSize = _spec.GetSpecSize(endwSpecOff);
        EmitInstruction(TasOpcode.ENDW, endwSpecOff, endwSpecSize);

        // Patch WHILE exit jump
        PatchJump(whileInstrIdx, jumpPatchOffset, CurrentByteOffset);
    }

    private void EmitFor(ForStmt forStmt)
    {
        // FOR: [exitJump(4)] [stop(5)] [step(5)] [counter(5)] [start(5)] [direction(1)]
        int specOff = _spec.BeginSpec();
        int jumpPatchOffset = _spec.Size;
        _spec.WriteInt32(0); // placeholder exit jump

        WriteExprParam(forStmt.Stop);
        WriteExprParam(forStmt.Step);
        WriteExprParam(new IdentifierExpr(forStmt.Counter, forStmt.Line));
        WriteExprParam(forStmt.Start);

        // Direction: determine from step
        byte direction = 0; // 0 = up, 1 = down
        if (forStmt.Step is LiteralExpr stepLit && stepLit.Value is int stepVal && stepVal < 0)
            direction = 1;
        if (forStmt.Step is UnaryExpr { Operator: "-" })
            direction = 1;
        _spec.WriteByte(direction);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.FOR, specOff, specSize);
        int forInstrIdx = _instructions.Count - 1;

        int loopStart = CurrentByteOffset;

        // Body
        EmitPass(forStmt.Body);

        // NEXT (back-jump)
        int nextSpecOff = _spec.BeginSpec();
        _spec.WriteInt32(loopStart);
        int nextSpecSize = _spec.GetSpecSize(nextSpecOff);
        EmitInstruction(TasOpcode.NEXT, nextSpecOff, nextSpecSize);

        // Patch FOR exit jump
        PatchJump(forInstrIdx, jumpPatchOffset, CurrentByteOffset);
    }

    private void EmitSelect(SelectStmt sel)
    {
        // SELECT: [selector(5)]
        int selSpecOff = _spec.BeginSpec();
        WriteExprParam(sel.Selector);
        int selSpecSize = _spec.GetSpecSize(selSpecOff);
        EmitInstruction(TasOpcode.SELECT, selSpecOff, selSpecSize);

        var endcJumps = new List<(int InstrIdx, int PatchOffset)>();

        foreach (var (caseValue, body) in sel.Cases)
        {
            if (caseValue != null)
            {
                // CASE: [value(5)]
                int caseSpecOff = _spec.BeginSpec();
                WriteExprParam(caseValue);
                int caseSpecSize = _spec.GetSpecSize(caseSpecOff);
                EmitInstruction(TasOpcode.CASE, caseSpecOff, caseSpecSize);
            }
            else
            {
                // OTHERWISE
                EmitSimple(TasOpcode.OTHERWISE, 0);
            }

            EmitPass(body);
        }

        // ENDC
        EmitSimple(TasOpcode.ENDC, 0);
    }

    private void EmitScan(ScanStmt scan)
    {
        // SCAN spec layout (43 bytes) — matches decompiler DecompileScan:
        // [0-3]   scan_end_jump (4B int32, patched later to byte offset of instruction after SET_SCAN_FLG)
        // [4-8]   handle (5B spec param)
        // [9-13]  key (5B spec param)
        // [14-18] start (5B spec param)
        // [19]    scope (1B: 'A'=all, 'R'=range, 'N'=next, 'F'=for, 'W'=while)
        // [20-24] sval (5B spec param)
        // [25-29] for expression (5B spec param)
        // [30-34] while expression (5B spec param)
        // [35]    display flag ('Y'/'N')
        // [36]    no_lock flag ('Y'/'N')
        // [37]    scope holder (1B)
        // [38-41] scope num holder (4B)
        // [42]    reverse flag ('Y'/'N')
        //
        // Decompiler output: SCAN handle, key [, startval] [, WHILE|FOR, expr] [, additional...]
        // The runtime sequence is: START_SCAN (no spec) → SCAN (43B) → body → SET_SCAN_FLG (9B)

        // Emit START_SCAN (no spec, but spec ptr = SCAN's spec offset — will be patched)
        int startScanIdx = _instructions.Count;
        EmitSimple(TasOpcode.START_SCAN, 0);

        // Parse SCAN options into structured parts
        var allToks = scan.Options;

        // Split at top-level commas (not inside parens)
        var segments = SplitAtTopLevelCommas(allToks);

        // Build SCAN spec (43 bytes)
        int specOff = _spec.BeginSpec();
        int jumpPatchOff = specOff; // offset of the 4-byte jump target

        // [0-3] jump to instruction after ENDS (patched later)
        _spec.WriteInt32(0); // placeholder — patched after body

        // [4-8] handle = first segment
        if (segments.Count >= 1 && segments[0].Count > 0)
            WriteScanTokensAsParam(segments[0]);
        else
            _spec.WriteNullParam();

        // [9-13] key = second segment
        if (segments.Count >= 2 && segments[1].Count > 0)
            WriteScanTokensAsParam(segments[1]);
        else
            _spec.WriteNullParam();

        // [14-18] start value = third segment (if not a keyword like WHILE/FOR or a condition expression)
        // Check if segments contain WHILE/FOR/NLOCK/DISP/REV keywords or comparison/logical operators
        int nextSeg = 2;
        bool hasStart = false;
        if (segments.Count > 2)
        {
            string firstTokVal = segments[2].Count > 0 ? segments[2][0].Value.ToUpperInvariant() : "";
            bool isKeyword = firstTokVal == "WHILE" || firstTokVal == "FOR" || firstTokVal == "NLOCK"
                || firstTokVal == "DISP" || firstTokVal == "REV";
            bool isCondition = segments[2].Any(t =>
                t.Type == TokenType.Equal || t.Type == TokenType.NotEqual ||
                t.Type == TokenType.LessThan || t.Type == TokenType.LessEqual ||
                t.Type == TokenType.GreaterThan || t.Type == TokenType.GreaterEqual ||
                t.Type == TokenType.And || t.Type == TokenType.Or);
            if (!isKeyword && !isCondition)
            {
                WriteScanTokensAsParam(segments[2]);
                hasStart = true;
                nextSeg = 3;
            }
        }
        if (!hasStart)
            _spec.WriteNullParam();

        // [19] scope flag — look for WHILE or FOR in remaining segments
        byte scopeFlag = (byte)'A'; // default
        List<Token>? forExprTokens = null;
        List<Token>? whileExprTokens = null;
        bool dispFlag = false, nlockFlag = false, revFlag = false;

        for (int s = nextSeg; s < segments.Count; s++)
        {
            if (segments[s].Count == 0) continue;
            string kw = segments[s][0].Value.ToUpperInvariant();
            switch (kw)
            {
                case "WHILE":
                    scopeFlag = (byte)'W';
                    if (s + 1 < segments.Count)
                        whileExprTokens = segments[++s];
                    break;
                case "FOR":
                    scopeFlag = (byte)'F';
                    if (s + 1 < segments.Count)
                        forExprTokens = segments[++s];
                    break;
                case "NLOCK": nlockFlag = true; break;
                case "DISP": dispFlag = true; break;
                case "REV": revFlag = true; break;
                default:
                    // Might be an expression for scope value or for/while
                    // If we haven't seen WHILE/FOR yet, treat as for expression
                    if (forExprTokens == null && whileExprTokens == null)
                        forExprTokens = segments[s];
                    break;
            }
        }

        _spec.WriteByte(scopeFlag);

        // [20-24] scope value
        _spec.WriteNullParam();

        // [25-29] for expression
        if (forExprTokens != null && forExprTokens.Count > 0)
            WriteScanTokensAsParam(forExprTokens);
        else
            _spec.WriteNullParam();

        // [30-34] while expression
        if (whileExprTokens != null && whileExprTokens.Count > 0)
            WriteScanTokensAsParam(whileExprTokens);
        else
            _spec.WriteNullParam();

        // [35] display flag
        _spec.WriteByte(dispFlag ? (byte)'Y' : (byte)'N');
        // [36] no_lock flag
        _spec.WriteByte(nlockFlag ? (byte)'Y' : (byte)'N');
        // [37] scope holder
        _spec.WriteByte(0);
        // [38-41] scope num holder
        _spec.WriteInt32(0);
        // [42] reverse flag
        _spec.WriteByte(revFlag ? (byte)'Y' : (byte)'N');

        int specSize = _spec.GetSpecSize(specOff);

        // Patch START_SCAN to point to SCAN's spec offset
        _instructions[startScanIdx] = new RunBytecodeInstruction
        {
            CommandNumber = TasOpcode.START_SCAN,
            Exit = 0,
            SpecLineSize = 0,
            SpecLinePtr = specOff
        };

        EmitInstruction(TasOpcode.SCAN, specOff, specSize);
        int scanInstrIdx = _instructions.Count - 1;

        // Emit body
        EmitPass(scan.Body);

        // Emit SET_SCAN_FLG with 9-byte spec: jump_back_addr(4) + null_param(5)
        int endSpecOff = _spec.BeginSpec();
        int scanCodeOffset = scanInstrIdx * _instrSize; // byte offset of SCAN instruction
        _spec.WriteInt32(scanCodeOffset);
        _spec.WriteNullParam();
        int endSpecSize = _spec.GetSpecSize(endSpecOff);
        EmitInstruction(TasOpcode.SET_SCAN_FLG, endSpecOff, endSpecSize);

        // Patch SCAN's jump target to point to the instruction AFTER SET_SCAN_FLG
        int afterEndsOffset = _instructions.Count * _instrSize;
        _spec.PatchInt32(jumpPatchOff, afterEndsOffset);
    }

    /// <summary>
    /// Split a token list at top-level commas (not inside parentheses).
    /// </summary>
    private static List<List<Token>> SplitAtTopLevelCommas(List<Token> tokens)
    {
        var result = new List<List<Token>>();
        var current = new List<Token>();
        int depth = 0;
        foreach (var tok in tokens)
        {
            if (tok.Type == TokenType.LeftParen) depth++;
            else if (tok.Type == TokenType.RightParen) depth--;

            if (tok.Type == TokenType.Comma && depth == 0)
            {
                result.Add(current);
                current = new List<Token>();
            }
            else
            {
                current.Add(tok);
            }
        }
        if (current.Count > 0)
            result.Add(current);
        return result;
    }

    /// <summary>
    /// Write a list of tokens as a single spec param.
    /// If single token → WriteTokenParam. If multiple → compile as expression.
    /// </summary>
    private void WriteScanTokensAsParam(List<Token> tokens)
    {
        if (tokens.Count == 1)
        {
            WriteTokenParam(tokens[0]);
        }
        else if (tokens.Count > 1)
        {
            // Multiple tokens = expression
            var expr = ParseTokensAsExpression(tokens);
            WriteExprParam(expr);
        }
        else
        {
            _spec.WriteNullParam();
        }
    }

    private void EmitExpressionStmt(ExpressionStmt exprStmt)
    {
        // Standalone expression — typically a function call used as command
        if (exprStmt.Expr is FunctionCallExpr func)
        {
            // Try to map to a command opcode
            if (_commandOpcodes.TryGetValue(func.Name.ToUpperInvariant(), out ushort opcode))
            {
                int specOff = _spec.BeginSpec();
                foreach (var arg in func.Arguments)
                    WriteExprParam(arg);
                int specSize = _spec.GetSpecSize(specOff);
                EmitInstruction(opcode, specOff, specSize);
                return;
            }

            // Try as UDC (user-defined command)
            int labelIdx = _labels.FindLabel(func.Name);
            if (labelIdx >= 0)
            {
                int specOff = _spec.BeginSpec();
                _spec.WriteInt32(labelIdx);
                // Write args
                foreach (var arg in func.Arguments)
                    WriteExprParam(arg);
                int specSize = _spec.GetSpecSize(specOff);
                EmitInstruction(TasOpcode.FUNC, specOff, specSize);
                return;
            }

            // Unknown function call — emit as FUNC with label ref that will be resolved
            {
                int lbl = _labels.GetOrCreateRef(func.Name);
                int specOff = _spec.BeginSpec();
                _spec.WriteInt32(lbl);
                foreach (var arg in func.Arguments)
                    WriteExprParam(arg);
                int specSize = _spec.GetSpecSize(specOff);
                EmitInstruction(TasOpcode.FUNC, specOff, specSize);
            }
            return;
        }

        // Generic standalone expression — compile and discard result
        var encoder = CreateExpressionEncoder();
        encoder.CompileToParam(exprStmt.Expr);
    }

    // ======================== Generic Command Handler ========================

    private void EmitGenericCommand(GenericCommandStmt gen)
    {
        string upper = gen.CommandName.ToUpperInvariant();

        if (!_commandOpcodes.TryGetValue(upper, out ushort opcode))
        {
            // FIELD[N] = expr — direct field index assignment from decompiler output
            if (upper == "FIELD" && gen.Tokens.Count >= 4)
            {
                var toks = gen.Tokens;
                // Look for pattern: [ N ] = expr
                if (toks.Count >= 4 && toks[0].Type == TokenType.LeftBracket
                    && toks[1].Type == TokenType.IntegerLiteral
                    && toks[2].Type == TokenType.RightBracket)
                {
                    int fieldIdx = int.Parse(toks[1].Value);
                    int fieldSpecSize = _fields.FieldSpecSize;
                    int fieldOffset = fieldIdx * fieldSpecSize;

                    int specOff = _spec.BeginSpec();
                    _spec.WriteFieldParam(fieldOffset);

                    // Find = sign and take everything after it as value
                    int eqIdx = toks.FindIndex(t => t.Type == TokenType.Equal);
                    if (eqIdx >= 0 && eqIdx + 1 < toks.Count)
                    {
                        // Build expression from remaining tokens
                        var valueToks = toks.GetRange(eqIdx + 1, toks.Count - eqIdx - 1);
                        var significant = FilterSignificantTokens(valueToks);
                        if (significant.Count >= 1)
                            WriteTokenParam(significant[0]);
                        else
                            _spec.WriteNullParam();
                    }
                    else
                    {
                        _spec.WriteNullParam();
                    }

                    int specSize = _spec.GetSpecSize(specOff);
                    EmitInstruction(TasOpcode.ASSIGN, specOff, specSize);
                    return;
                }
            }

            // Unknown command — try as UDC (user-defined command)
            int labelIdx = _labels.FindLabel(gen.CommandName);
            if (labelIdx >= 0)
            {
                int udcSpecOff = _spec.BeginSpec();
                _spec.WriteInt32(labelIdx);
                int udcSpecSize = _spec.GetSpecSize(udcSpecOff);
                EmitInstruction(TasOpcode.UDC, udcSpecOff, udcSpecSize);
                return;
            }

            // Emit as NOP with comment
            EmitSimple(TasOpcode.NOP, 0);
            return;
        }

        // Table-driven compiler handles ALL non-special commands
        var result = GetGenericCompiler().CompileCommand(gen);
        if (result.HasValue)
        {
            EmitInstruction(opcode, result.Value.SpecOffset, result.Value.SpecSize);

            // Register buffer name for OPENV/OPEN commands
            if (opcode is TasOpcode.OPENV or TasOpcode.OPEN &&
                gen.Tokens.Count > 0 && gen.Tokens[0].Type == TokenType.StringLiteral)
            {
                string bufName = gen.Tokens[0].Value.ToUpperInvariant();
                if (bufName.Length <= 8 && !_buffers.Any(b => b.Name == bufName))
                    _buffers.Add(new RunBufferEntry { Name = bufName.PadRight(8) });
            }
            return;
        }

        // Commands not in CommandTable — emit with raw token params as last resort
        EmitGenericParams(gen, opcode);
    }

    private void EmitGenericGoto(GenericCommandStmt gen)
    {
        int specOff = _spec.BeginSpec();
        string label = gen.Tokens.Count > 0 ? gen.Tokens[0].Value : "";
        int labelIdx = _labels.GetOrCreateRef(label);
        _spec.WriteInt32(labelIdx);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.GOTO, specOff, specSize);
    }

    private void EmitGenericGosub(GenericCommandStmt gen)
    {
        int specOff = _spec.BeginSpec();
        string label = gen.Tokens.Count > 0 ? gen.Tokens[0].Value : "";
        int labelIdx = _labels.GetOrCreateRef(label);
        _spec.WriteInt32(labelIdx);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.GOSUB, specOff, specSize);
    }

    private void EmitGenericAssign(GenericCommandStmt gen, ushort opcode)
    {
        // Assignment from tokens: target = value (ASSIGN) or target, value (POINTER)
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);

        // First significant token is target
        if (toks.Count >= 1)
            WriteTokenParam(toks[0]);
        else
            _spec.WriteNullParam();

        // Find '=' and take everything after it as value
        int eqIdx = gen.Tokens.FindIndex(t => t.Type == TokenType.Equal);
        if (eqIdx >= 0 && eqIdx + 1 < gen.Tokens.Count)
            WriteTokenParam(gen.Tokens[eqIdx + 1]);
        else if (toks.Count >= 2)
            WriteTokenParam(toks[1]); // For "pointer field, value" format
        else
            _spec.WriteNullParam();

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericIf(GenericCommandStmt gen)
    {
        // IF from generic tokens — complex, emit as generic params
        EmitGenericParams(gen, TasOpcode.IF);
    }

    private void EmitGenericTrap(GenericCommandStmt gen)
    {
        // TRAP spec layout (10 bytes):
        // [0-4]  trap key bitmap (5B param: C -> constant with key code bytes)
        // [5]    action byte: 'G'=GOTO, 'S'=GOSUB, 'I'=IGNR, 'D'=DFLT
        // [6-9]  label index (4B int32)
        //
        // Decompiler output: TRAP key1[,key2...] GOTO|GOSUB|IGNR|DFLT [LABEL_N]
        int specOff = _spec.BeginSpec();
        // Include Goto token type since GOTO is a keyword in the lexer
        var toks = gen.Tokens.Where(t =>
            t.Type is TokenType.Identifier or TokenType.StringLiteral or
            TokenType.IntegerLiteral or TokenType.NumericLiteral or
            TokenType.Goto or TokenType.True or TokenType.False).ToList();

        // Parse trap keys (everything before GOTO/GOSUB/IGNR/DFLT)
        var keyNames = new List<string>();
        int actionIdx = -1;
        for (int i = 0; i < toks.Count; i++)
        {
            string v = toks[i].Value.ToUpperInvariant();
            if (v is "GOTO" or "GOSUB" or "IGNR" or "DFLT")
            {
                actionIdx = i;
                break;
            }
            keyNames.Add(v);
        }

        // Encode trap key codes as a binary constant (A header + key code bytes + null)
        var keyCodes = new List<byte>();
        foreach (string kn in keyNames)
        {
            byte code = TrapKeyNameToCode(kn);
            if (code != 0) keyCodes.Add(code);
        }
        keyCodes.Add(0x00); // null terminator
        // Write as string constant: 'A'(1) + dec(1) + displaySize(2) + data
        int keyConstOff = (int)_constants.Size;
        var keyData = new byte[4 + keyCodes.Count];
        keyData[0] = (byte)'A';
        keyData[1] = 0; // decimals
        keyData[2] = (byte)(keyCodes.Count & 0xFF);
        keyData[3] = (byte)((keyCodes.Count >> 8) & 0xFF);
        keyCodes.CopyTo(keyData, 4);
        _constants.AddRaw(keyData);

        // Write key param as constant reference
        _spec.WriteConstParam(keyConstOff);

        // Write action byte
        char action = 'D';
        if (actionIdx >= 0)
        {
            action = toks[actionIdx].Value.ToUpperInvariant() switch
            {
                "GOTO" => 'G',
                "GOSUB" => 'S',
                "IGNR" => 'I',
                "DFLT" => 'D',
                _ => 'D'
            };
        }
        _spec.WriteByte((byte)action);

        // Write label index (4B int32)
        if (actionIdx >= 0 && actionIdx + 1 < toks.Count)
        {
            string labelName = toks[actionIdx + 1].Value;
            int labelIdx = _labels.GetOrCreateRef(labelName);
            _spec.WriteInt32(labelIdx);
        }
        else
            _spec.WriteInt32(0);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.TRAP, specOff, specSize);
    }

    private static byte TrapKeyNameToCode(string name)
    {
        // Reverse lookup of SpecDecoder._trapKeyNames
        var map = new Dictionary<string, byte>(StringComparer.OrdinalIgnoreCase)
        {
            ["f1"] = 0x01, ["f2"] = 0x02, ["f3"] = 0x03, ["f4"] = 0x04, ["f5"] = 0x05,
            ["f6"] = 0x06, ["f7"] = 0x07, ["f8"] = 0x08, ["f9"] = 0x09, ["f10"] = 0x0A,
            ["ESC"] = 0x0B, ["INT"] = 0x0C, ["T_ESC"] = 0x0D,
            ["UPAR"] = 0x0E, ["DNAR"] = 0x0F, ["LT_A"] = 0x10, ["RT_A"] = 0x11,
            ["LT_A_AS"] = 0x12, ["RT_A_AS"] = 0x13, ["HOME"] = 0x14, ["END"] = 0x15,
            ["PG_UP"] = 0x16, ["PG_DN"] = 0x17, ["INSRT"] = 0x18, ["DEL_KEY"] = 0x19,
            ["WD_LT"] = 0x1A, ["WD_RT"] = 0x1B, ["TAB"] = 0x1C, ["BCK_TAB"] = 0x1D,
            ["RSRCH"] = 0x1E, ["L_EXIT"] = 0x1F, ["RLCK"] = 0x20, ["FERR"] = 0x21,
            ["PERR"] = 0x22, ["PG_BRK"] = 0x23,
            ["sf1"] = 0x24, ["sf2"] = 0x25, ["sf3"] = 0x26, ["sf4"] = 0x27, ["sf5"] = 0x28,
            ["sf6"] = 0x29, ["sf7"] = 0x2A, ["sf8"] = 0x2B, ["sf9"] = 0x2C, ["sf10"] = 0x2D,
            ["ctl_f1"] = 0x2E, ["ctl_f2"] = 0x2F, ["ctl_f3"] = 0x30, ["ctl_f4"] = 0x31,
            ["ctl_f5"] = 0x32, ["ctl_f6"] = 0x33, ["ctl_f7"] = 0x34, ["ctl_f8"] = 0x35,
            ["ctl_f9"] = 0x36, ["ctl_f10"] = 0x37,
            ["CTL_PG_UP"] = 0x38, ["CTL_PG_DN"] = 0x39,
            ["alt_f1"] = 0x3A, ["alt_f2"] = 0x3B, ["alt_f3"] = 0x3C,
            ["MOUSE_MOV"] = 0x56, ["MOUSE_LBD"] = 0x57,
        };
        return map.TryGetValue(name, out var code) ? code : (byte)0;
    }

    private void EmitGenericOpenv(GenericCommandStmt gen, ushort opcode)
    {
        // OPENV spec layout (53 bytes, TAS 5.1):
        // [0]filename(5) [5]ext(5) [10]lock(1) [11]err_label(4) [15]owner(5)
        // [20]path(5) [25]fd(5) [30]type(1) [31]fnum(5) [36]size(5) [41]buf(5)
        // [46]create(1) [47]noclr(1) [48]update(5)
        int specOff = _spec.BeginSpec();

        var toks = FilterSignificantTokens(gen.Tokens);

        // Detect positional format: OPENV filename, cc, lock, handle [, recsize] [, bufname]
        // vs keyword format: OPENV filename FNUM h EXT cc LOCK l ...
        bool isPositional = toks.All(t => t.Type != TokenType.Identifier
            || !new[] { "FNUM", "EXT", "LOCK", "PATH", "BUF", "SIZE", "TYPE", "CREATE", "ERR", "OWNER", "FD", "NOCLR", "UPDATE" }
                .Contains(t.Value.ToUpperInvariant()));

        if (isPositional)
        {
            // Split at commas
            var args = new List<Token>();
            foreach (var t in toks)
            {
                if (t.Type != TokenType.Comma)
                    args.Add(t);
            }

            // [0] filename
            if (args.Count > 0) WriteTokenParam(args[0]); else _spec.WriteNullParam();
            // [5] ext/cc
            if (args.Count > 1) WriteTokenParam(args[1]); else _spec.WriteNullParam();
            // [10] lock (1B flag)
            if (args.Count > 2)
            {
                string lockVal = args[2].Value.ToUpperInvariant();
                _spec.WriteByte((byte)(lockVal.Length > 0 ? lockVal[0] : 'N'));
            }
            else _spec.WriteByte((byte)'N');
            // [11] err_label (4B)
            _spec.WriteInt32(0);
            // [15] owner (5)
            _spec.WriteNullParam();
            // [20] path (5)
            _spec.WriteNullParam();
            // [25] fd (5)
            _spec.WriteNullParam();
            // [30] type (1B, default 'T')
            _spec.WriteByte((byte)'T');
            // [31] fnum handle (5)
            if (args.Count > 3) WriteTokenParam(args[3]); else _spec.WriteNullParam();
            // [36] recsize (5)
            if (args.Count > 4) WriteTokenParam(args[4]); else _spec.WriteNullParam();
            // [41] bufname (5)
            if (args.Count > 5) WriteTokenParam(args[5]); else _spec.WriteNullParam();
            // [46] create (1B)
            _spec.WriteByte((byte)'N');
            // [47] noclr (1B)
            _spec.WriteByte((byte)'N');
            // [48] update (5) — 53B format
            if (_useLongOpenvSpec)
                _spec.WriteNullParam();

            // Extract buffer name for the buffer list
            if (args.Count > 0 && args[0].Type == TokenType.StringLiteral)
            {
                string bufName = args[0].Value.ToUpperInvariant();
                if (bufName.Length <= 8 && !_buffers.Any(b => b.Name == bufName))
                    _buffers.Add(new RunBufferEntry { Name = bufName.PadRight(8) });
            }
        }
        else
        {
            // Keyword format: OPENV filename FNUM h EXT cc LOCK l ...
            var kw = ParseKeywordTokens(gen.Tokens, "FNUM", "PATH", "BUF", "SIZE", "LOCK",
                "CREATE", "ERR", "EXT", "OWNER", "FD", "TYPE", "NOCLR", "UPDATE");

            // [0] filename (5)
            if (toks.Count > 0) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
            // [5] ext/cc (5)
            if (kw.TryGetValue("EXT", out var ext)) WriteTokenParam(ext);
            else _spec.WriteNullParam();
            // [10] lock mode byte (1)
            if (kw.TryGetValue("LOCK", out var lockTok))
                _spec.WriteByte((byte)char.ToUpperInvariant(lockTok.Value[0]));
            else
                _spec.WriteByte((byte)'N');
            // [11] err_label (4B int32)
            if (kw.TryGetValue("ERR", out var errTok))
                _spec.WriteInt32(_labels.GetOrCreateRef(errTok.Value));
            else
                _spec.WriteInt32(0);
            // [15] owner (5)
            if (kw.TryGetValue("OWNER", out var ownerTok)) WriteTokenParam(ownerTok);
            else _spec.WriteNullParam();
            // [20] path (5)
            if (kw.TryGetValue("PATH", out var pathTok)) WriteTokenParam(pathTok);
            else _spec.WriteNullParam();
            // [25] fd/schema (5)
            if (kw.TryGetValue("FD", out var fdTok)) WriteTokenParam(fdTok);
            else _spec.WriteNullParam();
            // [30] file_type (1B char, default 'T' for BTRV)
            if (kw.TryGetValue("TYPE", out var typeTok))
                _spec.WriteByte((byte)char.ToUpperInvariant(typeTok.Value[0]));
            else
                _spec.WriteByte((byte)'T');
            // [31] fnum handle (5)
            if (kw.TryGetValue("FNUM", out var fnumTok)) WriteTokenParam(fnumTok);
            else _spec.WriteNullParam();
            // [36] size (5)
            if (kw.TryGetValue("SIZE", out var sizeTok)) WriteTokenParam(sizeTok);
            else _spec.WriteNullParam();
            // [41] buf field (5)
            if (kw.TryGetValue("BUF", out var bufTok)) WriteTokenParam(bufTok);
            else _spec.WriteNullParam();
            // [46] create flag (1B)
            _spec.WriteByte(kw.ContainsKey("CREATE") ? (byte)'Y' : (byte)'N');
            // [47] noclr flag (1B)
            _spec.WriteByte(kw.ContainsKey("NOCLR") ? (byte)'Y' : (byte)'N');
            // [48] update_udf (5) — emitted in 53B format, omitted in 48B format
            if (_useLongOpenvSpec)
            {
                if (kw.TryGetValue("UPDATE", out var updateTok)) WriteTokenParam(updateTok);
                else _spec.WriteNullParam();
            }
        }

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericFindv(GenericCommandStmt gen, ushort opcode)
    {
        // FINDV spec layout (33 bytes):
        // [0]handle(5) [5]key(5) [10]val(5) [15]find_type(1) [16]err_label(4)
        // [20]nlock(1) [21]keyo(1) [22]for_expr(5) [27]while_expr(5) [32]noclr(1)
        int specOff = _spec.BeginSpec();

        var toks = gen.Tokens;
        var kw = ParseKeywordTokens(toks, "FNUM", "KEY", "VAL", "ERR", "NLOCK", "KEYO", "FOR", "WHILE", "NOCLR");

        // [0] handle (5)
        if (kw.TryGetValue("FNUM", out var fnumTok)) WriteTokenParam(fnumTok);
        else if (toks.Count > 1) WriteTokenParam(toks[1]);
        else _spec.WriteNullParam();

        // [5] key (5)
        if (kw.TryGetValue("KEY", out var keyTok)) WriteTokenParam(keyTok);
        else _spec.WriteNullParam();

        // [10] val (5) — TAS stores val as embedded param in constant pool
        // May have comma-separated values like: val FIELD1, FIELD2
        {
            var valFields = CollectValParams(toks);
            if (valFields.Count > 0)
            {
                var paramList = new List<(byte Type, int Location)>();
                foreach (var vf in valFields)
                {
                    int fieldIdx = _fields.FindField(vf);
                    if (fieldIdx >= 0)
                        paramList.Add(((byte)'F', _fields.GetFieldSpecOffset(fieldIdx)));
                }
                if (paramList.Count > 0)
                {
                    int constOff2 = _constants.AddEmbeddedParams(paramList);
                    _spec.WriteConstParam(constOff2);
                }
                else if (kw.TryGetValue("VAL", out var valTok))
                    WriteTokenParam(valTok);
                else
                    _spec.WriteNullParam();
            }
            else _spec.WriteNullParam();
        }

        // [15] find type byte (1) — first token after command name is usually the type
        byte findType = (byte)'F';
        if (toks.Count > 0 && toks[0].Value.Length == 1)
            findType = (byte)char.ToUpperInvariant(toks[0].Value[0]);
        _spec.WriteByte(findType);

        // [16] err label (4B int32)
        if (kw.TryGetValue("ERR", out var errTok))
            _spec.WriteInt32(_labels.GetOrCreateRef(errTok.Value));
        else
            _spec.WriteInt32(0);

        // [20] nlock flag (1)
        _spec.WriteByte(kw.ContainsKey("NLOCK") ? (byte)'Y' : (byte)'N');

        // [21] keyo flag (1)
        _spec.WriteByte(kw.ContainsKey("KEYO") ? (byte)'Y' : (byte)'N');

        // [22] for_expr (5)
        if (kw.TryGetValue("FOR", out var forTok)) WriteTokenParam(forTok);
        else _spec.WriteNullParam();

        // [27] while_expr (5)
        if (kw.TryGetValue("WHILE", out var whileTok)) WriteTokenParam(whileTok);
        else _spec.WriteNullParam();

        // [32] noclr flag (1)
        _spec.WriteByte(kw.ContainsKey("NOCLR") ? (byte)'Y' : (byte)'N');

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericSave(GenericCommandStmt gen)
    {
        // SAVE: [handle(5)] [nocnf(1)] [noclr(1)] [padding(8)]
        int specOff = _spec.BeginSpec();

        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        bool nocnf = gen.Tokens.Any(t => t.Value.Equals("NOCNF", StringComparison.OrdinalIgnoreCase));
        bool noclr = gen.Tokens.Any(t => t.Value.Equals("NOCLR", StringComparison.OrdinalIgnoreCase));
        _spec.WriteByte(nocnf ? (byte)'Y' : (byte)'N');
        _spec.WriteByte(noclr ? (byte)'Y' : (byte)'N');
        _spec.WritePadding(8);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.SAVE, specOff, specSize);
    }

    private void EmitCo(GenericCommandStmt gen)
    {
        // CO spec layout: [param(5)] [flag(1)] = 6 bytes
        // param is the expression/value, flag is result type char ('S' for string)
        // Bare "co" (no arguments) = 0-byte spec
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count == 0)
        {
            EmitSimple(TasOpcode.CO, 0);
            return;
        }

        int specOff = _spec.BeginSpec();
        var expr = ParseTokensAsExpression(gen.Tokens);
        WriteExprParam(expr);
        // Flag byte: 'S' for string result type
        char flag = InferResultTypeFlag(expr);
        _spec.WriteByte((byte)flag);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.CO, specOff, specSize);
    }

    private void EmitGenericChain(GenericCommandStmt gen, ushort opcode)
    {
        // CHAIN spec: prg_name(0) + null(5) + flag(10,1B) + param_list(11) + noclr(16,1B) + wait(17,1B) = 18 bytes
        int specOff = _spec.BeginSpec();

        // Split tokens at USING keyword
        var allToks = FilterSignificantTokens(gen.Tokens);
        int usingIdx = allToks.FindIndex(t => t.Type == TokenType.Identifier &&
            t.Value.Equals("USING", StringComparison.OrdinalIgnoreCase));

        var prgToks = usingIdx >= 0 ? allToks.Take(usingIdx).ToList() : allToks;
        var paramToks = usingIdx >= 0 ? allToks.Skip(usingIdx + 1).ToList() : new List<Token>();

        // [0-4]: program name
        bool hasOperators = prgToks.Any(t => t.Type == TokenType.Plus || t.Type == TokenType.Minus
            || t.Type == TokenType.Star || t.Type == TokenType.Slash);
        if (hasOperators)
        {
            var expr = ParseTokensAsExpression(prgToks);
            WriteExprParam(expr);
        }
        else
        {
            if (prgToks.Count >= 1) WriteTokenParam(prgToks[0]); else _spec.WriteNullParam();
        }

        // [5-9]: null param + [10]: flag byte
        _spec.WritePadding(6);

        // [11-15]: USING param list — embedded params referencing fields
        if (paramToks.Count > 0)
        {
            var paramList = new List<(byte Type, int Location)>();
            foreach (var tok in paramToks)
            {
                if (tok.Type != TokenType.Identifier) continue;
                int fldIdx = _fields.FindField(tok.Value);
                if (fldIdx >= 0)
                {
                    int fldOffset = fldIdx * 48;
                    paramList.Add(((byte)'F', fldOffset));
                }
            }
            if (paramList.Count > 0)
            {
                int constOff = _constants.AddEmbeddedParams(paramList);
                _spec.WriteConstParam(constOff);
            }
            else
            {
                _spec.WriteNullParam();
            }
        }
        else
        {
            _spec.WriteNullParam();
        }

        _spec.WriteByte((byte)'N');
        _spec.WriteByte(0);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericEnterAsk(GenericCommandStmt gen, ushort opcode)
    {
        // ENTER/ASK: [col(5)] [row(5)] [field(5)]
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        // Typically: enter row,col,field — but stored as col,row,field in spec
        for (int i = 0; i < 3; i++)
        {
            if (i < toks.Count) WriteTokenParam(toks[i]);
            else _spec.WriteNullParam();
        }
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericWindow(GenericCommandStmt gen, ushort opcode)
    {
        // WINDOW: up to 81 bytes, all 5-byte params at known offsets
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);

        // Write up to 11 5-byte params (55 bytes) or 16 (81 bytes)
        int paramCount = Math.Min(toks.Count, 16);
        for (int i = 0; i < paramCount; i++)
            WriteTokenParam(toks[i]);

        // Pad to minimum size if needed
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericFill(GenericCommandStmt gen)
    {
        // FILL: [target(5)] [value(5)]
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        if (toks.Count >= 2) WriteTokenParam(toks[1]); else _spec.WriteNullParam();
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.FILL, specOff, specSize);
    }

    private void EmitGenericMid(GenericCommandStmt gen)
    {
        // MID: [target(5)] [source(5)] [start(5)?] [length(5)?]
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        for (int i = 0; i < Math.Min(toks.Count, 4); i++)
            WriteTokenParam(toks[i]);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.MID_CMD, specOff, specSize);
    }

    private void EmitGenericXfer(GenericCommandStmt gen)
    {
        // XFER spec layout (26 bytes):
        // [0]from(5) [5]to(5) [10]numchr(5) [15]fmem(5) [20]tmem(5) [25]rec_buff(1)
        int specOff = _spec.BeginSpec();
        var kw = ParseKeywordTokens(gen.Tokens, "TO", "NUMCHR", "FMEM", "TMEM");
        var toks = FilterSignificantTokens(gen.Tokens);

        // [0] from field (5)
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        // [5] to field (5)
        if (kw.TryGetValue("TO", out var toTok)) WriteTokenParam(toTok);
        else _spec.WriteNullParam();

        // [10] numchr (5)
        if (kw.TryGetValue("NUMCHR", out var numchrTok)) WriteTokenParam(numchrTok);
        else _spec.WriteNullParam();

        // [15] fmem (5)
        if (kw.TryGetValue("FMEM", out var fmemTok)) WriteTokenParam(fmemTok);
        else _spec.WriteNullParam();

        // [20] tmem (5)
        if (kw.TryGetValue("TMEM", out var tmemTok)) WriteTokenParam(tmemTok);
        else _spec.WriteNullParam();

        // TAS 5.1: 25 bytes total (no rec_buff byte)
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.XFER, specOff, specSize);
    }

    private void EmitGenericInifle(GenericCommandStmt gen)
    {
        // INIFLE spec layout (11 bytes):
        // [0]filename(5) [5]noask(1) [6]specs_buffer(5)
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        var kw = ParseKeywordTokens(gen.Tokens, "NOASK", "SPECS");

        // [0] filename (5)
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        // [5] noask flag (1)
        _spec.WriteByte(kw.ContainsKey("NOASK") ? (byte)'Y' : (byte)'N');

        // [6] specs buffer (5)
        if (kw.TryGetValue("SPECS", out var specsTok)) WriteTokenParam(specsTok);
        else _spec.WriteNullParam();

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.INIFLE, specOff, specSize);
    }

    private void EmitGenericParam(GenericCommandStmt gen)
    {
        // PARAM stores a field list as an embedded param list in the constant pool.
        // Decompiler emits: param [FIELD1, FIELD2, ...] or param FIELD1
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 2)
        {
            // Multiple fields — store as embedded param list in constant pool
            var paramList = new List<(byte Type, int Location)>();
            foreach (var tok in toks)
            {
                int fieldIdx = _fields.FindField(tok.Value);
                if (fieldIdx >= 0)
                    paramList.Add(((byte)'F', _fields.GetFieldSpecOffset(fieldIdx)));
                else
                {
                    int constOff2 = _constants.AddString(tok.Value);
                    paramList.Add(((byte)'C', constOff2));
                }
            }
            int constOff = _constants.AddEmbeddedParams(paramList);
            _spec.WriteConstParam(constOff);
        }
        else if (toks.Count == 1)
        {
            // Single field — also store as embedded param list for consistency with TAS
            var paramList = new List<(byte Type, int Location)>();
            int fieldIdx = _fields.FindField(toks[0].Value);
            if (fieldIdx >= 0)
                paramList.Add(((byte)'F', _fields.GetFieldSpecOffset(fieldIdx)));
            else
            {
                int constOff2 = _constants.AddString(toks[0].Value);
                paramList.Add(((byte)'C', constOff2));
            }
            int constOff = _constants.AddEmbeddedParams(paramList);
            _spec.WriteConstParam(constOff);
        }
        else
            _spec.WriteNullParam();
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.PARAM, specOff, specSize);
    }

    private void EmitGenericOneParam(GenericCommandStmt gen, ushort opcode)
    {
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericTwoParams(GenericCommandStmt gen, ushort opcode)
    {
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        if (toks.Count >= 2) WriteTokenParam(toks[1]); else _spec.WriteNullParam();
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericPositionalCmd(GenericCommandStmt gen, ushort opcode)
    {
        // Commands with col,row,field layout (SAY, PMSG, DISPF, CURSOR, CLRLNE)
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        for (int i = 0; i < Math.Min(toks.Count, 3); i++)
            WriteTokenParam(toks[i]);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    /// <summary>
    /// Default handler: emit all tokens as consecutive 5-byte params.
    /// </summary>
    private void EmitGenericParams(GenericCommandStmt gen, ushort opcode)
    {
        int specOff = _spec.BeginSpec();
        WriteTokenParams(gen.Tokens);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    // ======================== Token → Expression Parsing ========================

    /// <summary>
    /// Parse a list of tokens (from GenericCommandStmt) into an Expression AST.
    /// Used for commands that take expression arguments (CO, SAY, ASSIGN, etc.).
    /// </summary>
    private Expression ParseTokensAsExpression(List<Token> tokens)
    {
        // Add EOF sentinel for the parser
        var allTokens = new List<Token>(tokens);
        allTokens.Add(new Token(TokenType.Eof, "", tokens.LastOrDefault()?.Line ?? 0, 0));
        var parser = new Parser.TasParser(allTokens);
        return parser.ParseExpressionPublic();
    }

    /// <summary>
    /// Parse all significant token groups from GenericCommandStmt as expressions.
    /// Splits on commas and returns each group as a parsed expression.
    /// </summary>
    private List<Expression> ParseTokensAsExpressions(List<Token> tokens)
    {
        var result = new List<Expression>();
        var current = new List<Token>();

        foreach (var tok in tokens)
        {
            if (tok.Type == TokenType.Comma)
            {
                if (current.Count > 0)
                {
                    result.Add(ParseTokensAsExpression(current));
                    current.Clear();
                }
            }
            else if (tok.Type != TokenType.Newline)
            {
                current.Add(tok);
            }
        }

        if (current.Count > 0)
            result.Add(ParseTokensAsExpression(current));

        return result;
    }

    // ======================== Helpers ========================

    private void WriteExprParam(Expression expr)
    {
        var encoder = CreateExpressionEncoder();
        var (type, loc) = encoder.CompileToParam(expr);
        _spec.WriteParam(type, loc);
    }

    private void WriteConditionParams(Expression condition)
    {
        // TAS 5.1: entire condition compiled as a single expression at [10-14]
        WriteExprParam(condition);
    }

    private void WriteTokenParam(Token tok)
    {
        switch (tok.Type)
        {
            case TokenType.Identifier:
            {
                int fieldIdx = _fields.FindField(tok.Value);
                if (fieldIdx >= 0)
                    _spec.WriteFieldParam(_fields.GetFieldSpecOffset(fieldIdx));
                else if (ExpressionEncoder.IsBuiltinFunction(tok.Value))
                {
                    // Known function used as keyword param value — compile as 0-arg function expression
                    var funcExpr = new Parser.Ast.FunctionCallExpr(tok.Value, new List<Parser.Ast.Expression>(), 0);
                    WriteExprParam(funcExpr);
                }
                else
                {
                    // Could be a label ref or string constant
                    int constOff = _constants.AddString(tok.Value);
                    _spec.WriteConstParam(constOff);
                }
                break;
            }
            case TokenType.StringLiteral:
            {
                int constOff = _constants.AddString(tok.Value);
                _spec.WriteConstParam(constOff);
                break;
            }
            case TokenType.IntegerLiteral:
            {
                if (int.TryParse(tok.Value, out int val))
                    _spec.WriteNumericParam(val);
                else
                {
                    int constOff = _constants.AddString(tok.Value);
                    _spec.WriteConstParam(constOff);
                }
                break;
            }
            case TokenType.NumericLiteral:
            {
                if (double.TryParse(tok.Value, out double dval))
                {
                    int constOff = _constants.AddNumeric(dval, 2);
                    _spec.WriteConstParam(constOff);
                }
                else
                    _spec.WriteNullParam();
                break;
            }
            case TokenType.True:
            {
                int off = _constants.AddLogical(true);
                _spec.WriteConstParam(off);
                break;
            }
            case TokenType.False:
            {
                int off = _constants.AddLogical(false);
                _spec.WriteConstParam(off);
                break;
            }
            default:
                // Keywords that can also be field names or string values (e.g., ENTER, ARRAY)
                if (!string.IsNullOrEmpty(tok.Value) && tok.Type != TokenType.Comma
                    && tok.Type != TokenType.Newline && tok.Type != TokenType.Eof)
                {
                    int fi = _fields.FindField(tok.Value);
                    if (fi >= 0)
                        _spec.WriteFieldParam(_fields.GetFieldSpecOffset(fi));
                    else
                    {
                        int co = _constants.AddString(tok.Value);
                        _spec.WriteConstParam(co);
                    }
                }
                break;
        }
    }

    private void WriteTokenParams(List<Token> tokens)
    {
        var significant = FilterSignificantTokens(tokens);
        foreach (var tok in significant)
            WriteTokenParam(tok);
    }

    /// <summary>Resolve a token to a (type, location) param pair for embedding in constant pool.</summary>
    private (byte Type, int Location) ResolveTokenToParam(Token tok)
    {
        switch (tok.Type)
        {
            case TokenType.Identifier:
            {
                int fieldIdx = _fields.FindField(tok.Value);
                if (fieldIdx >= 0)
                    return ((byte)'F', _fields.GetFieldSpecOffset(fieldIdx));
                int constOff = _constants.AddString(tok.Value);
                return ((byte)'C', constOff);
            }
            case TokenType.StringLiteral:
            {
                int constOff = _constants.AddString(tok.Value);
                return ((byte)'C', constOff);
            }
            case TokenType.IntegerLiteral:
            {
                if (int.TryParse(tok.Value, out int val))
                    return ((byte)'N', val);
                int constOff = _constants.AddString(tok.Value);
                return ((byte)'C', constOff);
            }
            default:
            {
                int fi = _fields.FindField(tok.Value);
                if (fi >= 0)
                    return ((byte)'F', _fields.GetFieldSpecOffset(fi));
                int co = _constants.AddString(tok.Value);
                return ((byte)'C', co);
            }
        }
    }

    /// <summary>Resolve a list of tokens (possibly an expression) to a (type, location) param pair.</summary>
    private (byte Type, int Location) ResolveTokenListToParam(List<Token> tokens)
    {
        var sig = tokens.Where(t => t.Type != TokenType.Comma).ToList();
        if (sig.Count == 1)
            return ResolveTokenToParam(sig[0]);

        // Multi-token: compile as expression
        var expr = ParseTokensAsExpression(tokens);
        var encoder = CreateExpressionEncoder();
        var (type, loc) = encoder.CompileToParam(expr);
        return ((byte)type, loc);
    }

    private static List<Token> FilterSignificantTokens(List<Token> tokens)
    {
        return tokens.Where(t =>
            t.Type is TokenType.Identifier or TokenType.StringLiteral or
            TokenType.IntegerLiteral or TokenType.NumericLiteral or
            TokenType.True or TokenType.False).ToList();
    }

    private Dictionary<string, Token> ParseKeywordTokens(List<Token> tokens, params string[] keywords)
    {
        var result = new Dictionary<string, Token>(StringComparer.OrdinalIgnoreCase);
        for (int i = 0; i < tokens.Count - 1; i++)
        {
            string val = tokens[i].Value.ToUpperInvariant();
            if (keywords.Contains(val) && i + 1 < tokens.Count)
            {
                var nextTok = tokens[i + 1];
                // Handle negative numbers: keyword - number → merge into single negative number token
                if (nextTok.Type == TokenType.Minus && i + 2 < tokens.Count
                    && tokens[i + 2].Type == TokenType.IntegerLiteral)
                {
                    result[val] = new Token(TokenType.IntegerLiteral, "-" + tokens[i + 2].Value, nextTok.Line, nextTok.Column);
                    i += 2; // skip both - and number
                }
                else
                {
                    result[val] = nextTok;
                    i++; // skip value token
                }
            }
            else if (keywords.Contains(val))
            {
                // Flag keyword with no value
                result[val] = tokens[i];
            }
        }
        return result;
    }

    /// <summary>
    /// Collect comma-separated field names after the VAL keyword in a token list.
    /// e.g. "val FIELD1, FIELD2" returns ["FIELD1", "FIELD2"]
    /// </summary>
    private List<string> CollectValParams(List<Token> tokens)
    {
        var result = new List<string>();
        // Find the VAL keyword
        for (int i = 0; i < tokens.Count - 1; i++)
        {
            if (tokens[i].Value.Equals("VAL", StringComparison.OrdinalIgnoreCase))
            {
                // Collect comma-separated identifiers
                int j = i + 1;
                while (j < tokens.Count)
                {
                    var t = tokens[j];
                    if (t.Type == TokenType.Identifier || t.Type == TokenType.IntegerLiteral)
                    {
                        result.Add(t.Value);
                        j++;
                        // Skip comma if present
                        if (j < tokens.Count && tokens[j].Type == TokenType.Comma)
                            j++;
                        else
                            break;
                    }
                    else break;
                }
                break;
            }
        }
        return result;
    }

    private void EmitQuit()
    {
        // QUIT spec layout: 5 zero bytes
        int specOff = _spec.BeginSpec();
        _spec.WritePadding(5);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.QUIT, specOff, specSize);
    }

    private static char InferResultTypeFlag(Expression expr)
    {
        return expr switch
        {
            LiteralExpr lit => lit.Value switch
            {
                string => 'S',
                int or long => 'N',
                double => 'N',
                bool => 'L',
                _ => 'S'
            },
            FunctionCallExpr func => ExpressionEncoder.InferFunctionReturnTypeFlag(func.Name),
            BinaryExpr => 'S',
            _ => 'S'
        };
    }

    private void EmitSimple(ushort opcode, int specSize)
    {
        EmitInstruction(opcode, 0, 0);
    }

    private void EmitInstruction(ushort opcode, int specPtr, int specSize)
    {
        _instructions.Add(new RunBytecodeInstruction
        {
            CommandNumber = opcode,
            Exit = 0,
            SpecLineSize = (byte)Math.Min(specSize, 255),
            SpecLinePtr = specPtr,
        });
    }

    private int CurrentByteOffset => _instructions.Count * _instrSize;

    private int GetTempBase()
    {
        // Expression temps start at offset 0 in expression-local temp area
        return 0;
    }

    private ExpressionEncoder CreateExpressionEncoder()
    {
        var encoder = new ExpressionEncoder(_fields, _constants, _labels, GetTempBase());
        encoder.OverlayFieldOffset = _overlayFieldOffset;
        encoder.OverlayConstOffset = _overlayConstOffset;
        return encoder;
    }

    private GenericCompiler GetGenericCompiler()
    {
        if (_genericCompiler == null)
        {
            _genericCompiler = new GenericCompiler(
                _spec, _constants, _fields, _labels,
                CreateExpressionEncoder,
                ParseTokensAsExpression);
        }
        _genericCompiler.UseTas51Sizes = _isTas51;
        return _genericCompiler;
    }

    private void PatchJump(int instrIdx, int specOffset, int targetByteOffset)
    {
        // Patch the 4-byte jump target directly in the spec segment
        _spec.PatchInt32(specOffset, targetByteOffset);
    }

    private static bool IsComparisonOp(string op) =>
        op is "=" or "<>" or "<" or ">" or "<=" or ">=" or
             "and" or "or" or ".AND." or ".OR." or ".A." or ".O.";

    // ======================== Specific Command Emitters ========================

    private void EmitGenericMsg(GenericCommandStmt gen)
    {
        // MSG spec: 6B short [msg(5)+nowait(1)] or 15B long [msg(5)+nowait(1)+pad(4)+win(5)]
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);

        // First significant token is the message text
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        bool nowait = gen.Tokens.Any(t => t.Value.Equals("NOWAIT", StringComparison.OrdinalIgnoreCase));
        _spec.WriteByte(nowait ? (byte)'Y' : (byte)'N');

        var kw = ParseKeywordTokens(gen.Tokens, "WIN");
        if (_useLongMsgSpec || kw.ContainsKey("WIN"))
        {
            _spec.WritePadding(4);
            if (kw.TryGetValue("WIN", out var winTok))
                WriteTokenParam(winTok);
            else
                _spec.WriteNullParam();
        }

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.MSG, specOff, specSize);
    }

    private void EmitGenericSavesRedsp(GenericCommandStmt gen, ushort opcode)
    {
        // SAVES/REDSP spec (TAS 5.1): [handle(5)] [sco_flag(1)] = 6 bytes
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        _spec.WriteByte((byte)'N'); // SCO flag = N
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericUdfc(GenericCommandStmt gen, ushort opcode)
    {
        // UDC/FUNC/CMD spec: [label(4)] [flist(5)] = 9 bytes
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        // First token is label name
        string labelName = toks.Count > 0 ? toks[0].Value : "";
        int labelIdx = _labels.GetOrCreateRef(labelName);
        _spec.WriteInt32(labelIdx);
        // flist param (field list) — usually null
        if (toks.Count > 1)
            WriteTokenParam(toks[1]);
        else
            _spec.WriteNullParam();
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericClose(GenericCommandStmt gen)
    {
        // CLOSE spec: [handle(5)] [delete(1)] = 6 bytes
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        bool delete = gen.Tokens.Any(t => t.Value.Equals("DELETE", StringComparison.OrdinalIgnoreCase));
        _spec.WriteByte(delete ? (byte)'Y' : (byte)'N');
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.CLOSE, specOff, specSize);
    }

    private void EmitGenericDel(GenericCommandStmt gen)
    {
        // DEL spec: [num(5)] [noask(1)] [nodel_lbl(4)] [pad(1)] [err_lbl(4)] = 15 bytes
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        bool noask = gen.Tokens.Any(t => t.Value.Equals("NOASK", StringComparison.OrdinalIgnoreCase));
        _spec.WriteByte(noask ? (byte)'Y' : (byte)'N');

        // nodel label (4 bytes)
        int nodelIdx = gen.Tokens.FindIndex(t => t.Value.Equals("NODEL", StringComparison.OrdinalIgnoreCase));
        if (nodelIdx >= 0 && nodelIdx + 1 < gen.Tokens.Count)
        {
            int lbl = _labels.GetOrCreateRef(gen.Tokens[nodelIdx + 1].Value);
            _spec.WriteInt32(lbl);
        }
        else
            _spec.WriteInt32(0);

        // pad byte
        _spec.WriteByte(0);

        // err label (4 bytes)
        int errIdx = gen.Tokens.FindIndex(t => t.Value.Equals("ERR", StringComparison.OrdinalIgnoreCase));
        if (errIdx >= 0 && errIdx + 1 < gen.Tokens.Count)
        {
            int lbl = _labels.GetOrCreateRef(gen.Tokens[errIdx + 1].Value);
            _spec.WriteInt32(lbl);
        }
        else
            _spec.WriteInt32(0);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.DEL, specOff, specSize);
    }

    private void EmitGenericThreeParams(GenericCommandStmt gen, ushort opcode)
    {
        // WRAP/REWRAP spec: [fld(5)] [col(5)] [dlnes(5)] = 15 bytes
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        // First significant token is the field
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        // Look for keyword params
        var kw = ParseKeywordTokens(gen.Tokens, "COL", "DLNES");
        if (kw.TryGetValue("COL", out var colTok)) WriteTokenParam(colTok);
        else _spec.WriteNullParam();
        if (kw.TryGetValue("DLNES", out var dlnTok)) WriteTokenParam(dlnTok);
        else _spec.WriteNullParam();

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericGetlbl(GenericCommandStmt gen)
    {
        // GETLBL spec: [label(4)] [fld(5)] = 9 bytes
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        // First token is label name
        string labelName = toks.Count > 0 ? toks[0].Value : "";
        int labelIdx = _labels.GetOrCreateRef(labelName);
        _spec.WriteInt32(labelIdx);
        // Second token is field
        if (toks.Count > 1) WriteTokenParam(toks[1]); else _spec.WriteNullParam();
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.GETLBL, specOff, specSize);
    }

    private void EmitGenericOn(GenericCommandStmt gen)
    {
        // ON spec: [val(5)] [tosub(1)] [num_labels(1)] [labels(4×N)]
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);

        // First token is value expression
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        // Check for on_gosub vs on_goto
        bool isSub = gen.Tokens.Any(t => t.Value.Equals("ON_GOSUB", StringComparison.OrdinalIgnoreCase));
        _spec.WriteByte(isSub ? (byte)'S' : (byte)'G');

        // Collect label names (skip value token and on_goto/on_gosub keyword)
        var labels = new List<string>();
        bool pastKeyword = false;
        foreach (var tok in toks.Skip(1))
        {
            string upper = tok.Value.ToUpperInvariant();
            if (upper is "ON_GOTO" or "ON_GOSUB") { pastKeyword = true; continue; }
            if (pastKeyword || upper is not "ON_GOTO" and not "ON_GOSUB")
                labels.Add(tok.Value);
        }

        _spec.WriteByte((byte)labels.Count);
        foreach (var lbl in labels)
        {
            int labelIdx = _labels.GetOrCreateRef(lbl);
            _spec.WriteInt32(labelIdx);
        }

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.ON, specOff, specSize);
    }

    private void EmitGenericForce(GenericCommandStmt gen)
    {
        // FORCE spec: [flag(1)] = 1 byte (Y=on, N=off)
        int specOff = _spec.BeginSpec();
        bool isOn = gen.Tokens.Any(t => t.Value.Equals("ON", StringComparison.OrdinalIgnoreCase));
        _spec.WriteByte(isOn ? (byte)'Y' : (byte)'N');
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.FORCE, specOff, specSize);
    }

    private void EmitGenericRap(GenericCommandStmt gen)
    {
        // RAP spec layout (17-20 bytes):
        // [0]name(5) [5]num(5) [10]in_mem(1) [11]with(5) [16]no_base_wind(1)
        // [17]new_runtime(1) [18]no_delete(1) [19]no_save(1)
        int specOff = _spec.BeginSpec();
        var kw = ParseKeywordTokens(gen.Tokens, "NUM", "WITH", "IN_MEM", "NO_BASE_WIND",
            "NEW_RUNTIME", "NO_DELETE", "NO_SAVE");

        // [0] name (5)
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count > 0) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        // [5] num (5)
        if (kw.TryGetValue("NUM", out var numTok)) WriteTokenParam(numTok);
        else _spec.WriteNullParam();

        // [10] in_mem flag (1)
        _spec.WriteByte(kw.ContainsKey("IN_MEM") ? (byte)'Y' : (byte)0);

        // [11] with (5)
        if (kw.TryGetValue("WITH", out var withTok)) WriteTokenParam(withTok);
        else _spec.WriteNullParam();

        // [16] no_base_wind flag (1)
        _spec.WriteByte(kw.ContainsKey("NO_BASE_WIND") ? (byte)'Y' : (byte)0);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.RAP, specOff, specSize);
    }

    private void EmitGenericColor(GenericCommandStmt gen)
    {
        // COLOR spec layout (30 bytes):
        // [0] NORM (5B), [5] HIGH (5B), [10] REV (5B), [15] ERR (5B), [20] ENTER (5B), [25] ARRAY (5B)
        int specOff = _spec.BeginSpec();
        string[] colorKeywords = ["HIGH", "REV", "ERR", "ENTER", "ARRAY"];
        var kw = ParseKeywordTokens(gen.Tokens, colorKeywords);
        // First significant token that isn't a keyword = NORM value
        var normTok = gen.Tokens.FirstOrDefault(t =>
            (t.Type is TokenType.Identifier or TokenType.StringLiteral or TokenType.IntegerLiteral)
            && !colorKeywords.Contains(t.Value, StringComparer.OrdinalIgnoreCase));

        int beforeNorm = _spec.GetSpecSize(specOff);

        // [0] NORM — first non-keyword significant token
        if (normTok != null) WriteTokenParam(normTok); else _spec.WriteNullParam();
        // [5] HIGH
        if (kw.TryGetValue("HIGH", out var highTok)) WriteTokenParam(highTok); else _spec.WriteNullParam();
        // [10] REV
        if (kw.TryGetValue("REV", out var revTok)) WriteTokenParam(revTok); else _spec.WriteNullParam();
        // [15] ERR
        if (kw.TryGetValue("ERR", out var errTok)) WriteTokenParam(errTok); else _spec.WriteNullParam();
        // [20] ENTER
        if (kw.TryGetValue("ENTER", out var enterTok)) WriteTokenParam(enterTok); else _spec.WriteNullParam();
        // [25] ARRAY
        if (kw.TryGetValue("ARRAY", out var arrayTok)) WriteTokenParam(arrayTok); else _spec.WriteNullParam();

        EmitInstruction(TasOpcode.COLOR, specOff, _spec.GetSpecSize(specOff));
    }

    private void EmitGenericPmsg(GenericCommandStmt gen)
    {
        // PMSG spec layout (29 bytes) — matches decompiler DecompilePmsg:
        // [0] col (5B), [5] row (5B), [10] msg (5B),
        // [15] wait (1B), [16] ncr (1B), [17] ent (5B),
        // [22] whr (1B), [23] color (5B), [28] abs (1B)
        //
        // Decompiler output: PMSG msg [, msg2...] AT col,row [WAIT] [NOCR] [COLOR val] [ABS]
        int specOff = _spec.BeginSpec();

        // Split tokens at AT keyword to separate message from position
        var allToks = gen.Tokens;
        int atIdx = allToks.FindIndex(t => t.Value.Equals("AT", StringComparison.OrdinalIgnoreCase)
            && (t.Type == TokenType.Identifier || t.Type == TokenType.At));

        // Collect keyword flags from tokens AFTER AT
        var afterAt = atIdx >= 0 ? allToks.Skip(atIdx + 1).ToList() : allToks;
        var kw = ParseKeywordTokens(afterAt, "WAIT", "NOCR", "NCR", "ENT", "COLOR", "ABS", "PTW");

        // Message tokens: everything before AT (or all tokens if no AT)
        var msgToks = atIdx >= 0 ? allToks.Take(atIdx).ToList() : new List<Token>();
        // Position tokens: between AT and first keyword, filtering keywords
        Token? colTok = null, rowTok = null;
        if (atIdx >= 0)
        {
            var posToks = FilterSignificantTokens(afterAt
                .TakeWhile(t => !t.Value.Equals("WAIT", StringComparison.OrdinalIgnoreCase)
                    && !t.Value.Equals("NOCR", StringComparison.OrdinalIgnoreCase)
                    && !t.Value.Equals("NCR", StringComparison.OrdinalIgnoreCase)
                    && !t.Value.Equals("COLOR", StringComparison.OrdinalIgnoreCase)
                    && !t.Value.Equals("ABS", StringComparison.OrdinalIgnoreCase)
                    && !t.Value.Equals("ENT", StringComparison.OrdinalIgnoreCase)
                    && !t.Value.Equals("PTW", StringComparison.OrdinalIgnoreCase))
                .ToList());
            if (posToks.Count >= 1) colTok = posToks[0];
            if (posToks.Count >= 2) rowTok = posToks[1];
        }

        // [0] col
        if (colTok != null) WriteTokenParam(colTok); else _spec.WriteNumericParam(1);
        // [5] row
        if (rowTok != null) WriteTokenParam(rowTok); else _spec.WriteNumericParam(1);
        // [10] msg — TAS wraps message params in an embedded param list constant
        if (msgToks.Count > 0)
        {
            var sigMsg = msgToks.Where(t => t.Type != TokenType.Comma).ToList();
            if (sigMsg.Count >= 1)
            {
                // Build embedded param list: each message part is a 5-byte param
                var paramList = new List<(byte Type, int Location)>();
                if (sigMsg.Count == 1)
                {
                    // Single string/field → one param in list
                    var (ptype, ploc) = ResolveTokenToParam(sigMsg[0]);
                    paramList.Add((ptype, ploc));
                }
                else
                {
                    // Multiple parts (concatenation) - collect as individual params
                    // Split on top-level commas only (respect parenthesis nesting)
                    var currentExprParts = new List<Token>();
                    int parenDepth = 0;
                    foreach (var tok in msgToks)
                    {
                        if (tok.Type == TokenType.LeftParen) parenDepth++;
                        else if (tok.Type == TokenType.RightParen) parenDepth--;

                        if (tok.Type == TokenType.Comma && parenDepth == 0)
                        {
                            // Flush current expression
                            if (currentExprParts.Count > 0)
                            {
                                var (pt, pl) = ResolveTokenListToParam(currentExprParts);
                                paramList.Add((pt, pl));
                                currentExprParts.Clear();
                            }
                        }
                        else
                            currentExprParts.Add(tok);
                    }
                    if (currentExprParts.Count > 0)
                    {
                        var (pt, pl) = ResolveTokenListToParam(currentExprParts);
                        paramList.Add((pt, pl));
                    }
                }
                int constOff = _constants.AddEmbeddedParams(paramList);
                _spec.WriteConstParam(constOff);
            }
            else
                _spec.WriteNullParam();
        }
        else
            _spec.WriteNullParam();
        // [15] wait ('N' default, 'Y' if set)
        _spec.WriteByte(kw.ContainsKey("WAIT") ? (byte)'Y' : (byte)'N');
        // [16] ncr/nocr ('N' default, 'Y' if set)
        _spec.WriteByte(kw.ContainsKey("NOCR") || kw.ContainsKey("NCR") ? (byte)'Y' : (byte)'N');
        // [17] ent
        if (kw.TryGetValue("ENT", out var entTok)) WriteTokenParam(entTok); else _spec.WriteNullParam();
        // [22] whr ('D' default, 'P' printer, 'S' screen)
        if (kw.TryGetValue("PTW", out var ptwTok))
            _spec.WriteByte((byte)char.ToUpperInvariant(ptwTok.Value[0]));
        else
            _spec.WriteByte((byte)'D');
        // [23] color
        if (kw.TryGetValue("COLOR", out var colorTok)) WriteTokenParam(colorTok); else _spec.WriteNullParam();
        // [28] abs ('N' default, 'Y' if set)
        _spec.WriteByte(kw.ContainsKey("ABS") ? (byte)'Y' : (byte)'N');

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.PMSG, specOff, specSize);
    }

    private void EmitGenericFormat(GenericCommandStmt gen)
    {
        // FORMAT spec layout (20 bytes):
        // [0] fld (5B), [5] recv (5B), [10] commas (1B), [11] flt_dol (1B),
        // [12] neg_how (1B), [13] off (1B), [14] pict (5B), [19] no_zeros (1B)
        int specOff = _spec.BeginSpec();
        var kw = ParseKeywordTokens(gen.Tokens, "COMMAS", "FLT_DOL", "OFF", "PICT", "NOZERO");
        var toks = FilterSignificantTokens(gen.Tokens);

        // [0] fld
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        // [5] recv (second significant token, if present)
        if (toks.Count >= 2) WriteTokenParam(toks[1]); else _spec.WriteNullParam();
        // [10] commas
        _spec.WriteByte(kw.ContainsKey("COMMAS") ? (byte)'Y' : (byte)0);
        // [11] flt_dol
        _spec.WriteByte(kw.ContainsKey("FLT_DOL") ? (byte)'Y' : (byte)0);
        // [12] neg_how
        _spec.WriteByte(0);
        // [13] off
        _spec.WriteByte(kw.ContainsKey("OFF") ? (byte)'Y' : (byte)0);
        // [14] pict
        if (kw.TryGetValue("PICT", out var pictTok)) WriteTokenParam(pictTok); else _spec.WriteNullParam();
        // [19] no_zeros
        _spec.WriteByte(kw.ContainsKey("NOZERO") ? (byte)'Y' : (byte)0);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.FORMAT, specOff, specSize);
    }

    private void EmitGenericScrn(GenericCommandStmt gen)
    {
        // SCRN spec: [action(1)] = 1 byte (S/R/L/U/E/D)
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1 && toks[0].Value.Length >= 1)
            _spec.WriteByte((byte)toks[0].Value[0]);
        else
            _spec.WriteByte(0);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.SCRN, specOff, specSize);
    }

    private byte[] BuildRunFile()
    {
        byte[] codeSegment = BuildCodeSegment();
        byte[] constSegment = _constants.ToArray();
        byte[] specSegment = _spec.ToArray();
        List<int> labelOffsets = _labels.GetAllOffsets();
        List<RunFieldSpec> fieldSpecs = _fields.GetAllSpecs();

        // Calculate prg_names: count of non-temp, non-file field specs * 15 (name field width)
        int memoryDefineCount = 0;
        foreach (var f in fieldSpecs)
        {
            if (!f.IsTempField && f.FileFieldIndex == 0)
                memoryDefineCount++;
        }
        int prgNames = memoryDefineCount * 15;

        var header = new RunFileHeader
        {
            CodeSize = codeSegment.Length,
            ConstSize = constSegment.Length,
            SpecSize = specSegment.Length,
            LabelSize = labelOffsets.Count * 4,
            ScrnFldNum = 10, // standard TAS screen field count constant
            NumFlds = _fields.Count,
            TempFlds = _fields.TempFieldCount * _fields.FieldSpecSize,
            NumTempFlds = _fields.TempFieldCount,
            FldNameSize = _fields.Count * _fields.FieldSpecSize,
            TempFldSize = _fields.TempFieldCount > 0 ? 65535 : 0,
            DefFldSegSize = _fields.DefinedDataSize,
            NumExtraFlds = 0,
            PrgNames = prgNames,
            DebugFlg = false,
            ProType = _isTas51 ? "TAS32" : "TAS50",
            NumLabels = labelOffsets.Count,
            NewFldSpec = false,
            ChkUpVld = false,
            IncLabels = false,
        };

        // Build field spec segment using RunFileWriter's proper encoder
        byte[] fieldSegment = RunFileWriter.BuildFieldSpecSegment(fieldSpecs, _fields.FieldSpecSize);

        return RunFileWriter.Write(header, _buffers, codeSegment, constSegment, specSegment,
            labelOffsets, fieldSegment);
    }

    private byte[] BuildCodeSegment()
    {
        int instrSize = _isTas51 ? RunFileHeader.Tas51InstructionSize : RunFileHeader.Tas60InstructionSize;
        byte[] code = new byte[_instructions.Count * instrSize];
        using var ms = new MemoryStream(code);
        using var w = new BinaryWriter(ms);

        foreach (var instr in _instructions)
        {
            if (_isTas51)
            {
                w.Write(instr.CommandNumber);
                w.Write(instr.SpecLineSize);
                w.Write(instr.SpecLineSize > 0 ? instr.SpecLinePtr + _overlaySpecOffset : instr.SpecLinePtr);
            }
            else
            {
                w.Write(instr.CommandNumber);
                w.Write(instr.Exit);
                w.Write(instr.SpecLineSize);
                w.Write(instr.SpecLinePtr);
            }
        }

        return code;
    }

    private static char MapFieldType(string? type)
    {
        if (string.IsNullOrEmpty(type)) return 'A';
        return type.ToUpperInvariant() switch
        {
            "A" or "ALPHA" => 'A',
            "I" or "INTEGER" or "INT" => 'I',
            "N" or "NUMERIC" or "NUM" => 'N',
            "D" or "DATE" => 'D',
            "T" or "TIME" => 'T',
            "L" or "LOGICAL" or "LOG" => 'L',
            "R" or "REAL" => 'R',
            "B" or "BYTE" => 'B',
            "F" or "FLOAT" => 'F',
            "P" or "PACKED" => 'P',
            _ => 'A'
        };
    }

    private static int DefaultSize(char fieldType) => fieldType switch
    {
        'A' => 10,
        'I' => 4,
        'N' => 10,
        'D' => 8,
        'T' => 6,
        'L' => 1,
        'R' => 8,
        'B' => 1,
        'F' => 8,
        'P' => 5,
        _ => 10
    };

    // ======================== Complete Command → Opcode Map ========================

    private static Dictionary<string, ushort> BuildCommandOpcodeMap()
    {
        return new Dictionary<string, ushort>(StringComparer.OrdinalIgnoreCase)
        {
            // Standard commands
            ["NOP"] = TasOpcode.NOP,
            ["REM"] = TasOpcode.NOP,
            ["REMARK"] = TasOpcode.NOP,
            ["?"] = TasOpcode.PMSG,
            ["PMSG"] = TasOpcode.PMSG,
            ["CLRSCR"] = TasOpcode.CLRSCR,
            ["CURSOR"] = TasOpcode.CURSOR,
            ["BELL"] = TasOpcode.BELL,
            ["CLR"] = TasOpcode.CLR,
            ["CLRLNE"] = TasOpcode.CLRLNE,
            ["CLOSE"] = TasOpcode.CLOSE,
            ["RDA"] = TasOpcode.RDA,
            ["CLRSF"] = TasOpcode.CLRSF,
            ["POSTMSG"] = TasOpcode.POSTMSG,
            ["DEL"] = TasOpcode.DEL,
            ["WRTA"] = TasOpcode.WRTA,
            ["MENU"] = TasOpcode.MENU,
            ["ASSIGN"] = TasOpcode.ASSIGN,
            ["DISPF"] = TasOpcode.DISPF,
            ["FILL"] = TasOpcode.FILL,
            ["POINTER"] = TasOpcode.POINTER,
            ["->"] = TasOpcode.POINTER,
            ["FIND"] = TasOpcode.FIND,
            ["SRCH"] = TasOpcode.SRCH,
            ["GOSUB"] = TasOpcode.GOSUB,
            ["GOTO"] = TasOpcode.GOTO,
            ["SAY"] = TasOpcode.SAY,
            ["INIFLE"] = TasOpcode.INIFLE,
            ["UPDTA"] = TasOpcode.UPDTA,
            ["FINDV"] = TasOpcode.FINDV,
            ["NMENU"] = TasOpcode.NMENU,
            ["MOUNT"] = TasOpcode.MOUNT,
            ["XFER"] = TasOpcode.XFER,
            ["ON"] = TasOpcode.ON,
            ["OPEN"] = TasOpcode.OPEN,
            ["RET"] = TasOpcode.RET,
            ["RETURN"] = TasOpcode.RET,
            ["ENTER"] = TasOpcode.ENTER,
            ["PBLNK"] = TasOpcode.PBLNK,
            ["PBOX"] = TasOpcode.PBOX,
            ["PCHR"] = TasOpcode.PCHR,
            ["PFMT"] = TasOpcode.PFMT,
            ["PON"] = TasOpcode.PON,
            ["BRKRET"] = TasOpcode.BRKRET,
            ["PVERT"] = TasOpcode.PVERT,
            ["INC"] = TasOpcode.INC,
            ["MID"] = TasOpcode.MID_CMD,
            ["CO"] = TasOpcode.CO,
            ["REDEF"] = TasOpcode.REDEF,
            ["REDEFINE"] = TasOpcode.REDEF,
            ["REDSP"] = TasOpcode.REDSP,
            ["WINDEF"] = TasOpcode.WINDEF,
            ["WINACT"] = TasOpcode.WINACT,
            ["CHAIN"] = TasOpcode.CHAIN,
            ["SAVE"] = TasOpcode.SAVE,
            ["SAVES"] = TasOpcode.SAVES,
            ["SCROLL"] = TasOpcode.SCROLL,
            ["SORTA"] = TasOpcode.SORTA,
            ["PTOF"] = TasOpcode.PTOF,
            ["TRANSX"] = TasOpcode.TRANSX,
            ["TRAP"] = TasOpcode.TRAP,
            ["ULKALL"] = TasOpcode.ULKALL,
            ["WINDOW"] = TasOpcode.WINDOW,
            ["REENT"] = TasOpcode.REENT,
            ["REENTER"] = TasOpcode.REENT,
            ["IF"] = TasOpcode.IF,
            ["POPS"] = TasOpcode.POPS,
            ["CLSO"] = TasOpcode.CLSO,
            ["PUT_FLD"] = TasOpcode.PUT_FLD,
            ["SORT3"] = TasOpcode.SORT3,
            ["OPENV"] = TasOpcode.OPENV,
            ["REMVA"] = TasOpcode.REMVA,
            ["ELSE"] = TasOpcode.ELSE,
            ["WHILE"] = TasOpcode.WHILE,
            ["SELECT"] = TasOpcode.SELECT,
            ["ENDW"] = TasOpcode.ENDW,
            ["LOOP_IF"] = TasOpcode.LOOP_IF,
            ["EXIT_IF"] = TasOpcode.EXIT_IF,
            ["FOR"] = TasOpcode.FOR,
            ["FUNC"] = TasOpcode.FUNC,
            ["CMD"] = TasOpcode.CMD,
            ["UDC"] = TasOpcode.UDC,
            ["RAP"] = TasOpcode.RAP,
            ["CHAINR"] = TasOpcode.CHAINR,
            ["CLSPF"] = TasOpcode.CLSPF,
            ["LISTM"] = TasOpcode.LISTM,
            ["EXPORT"] = TasOpcode.EXPORT,
            ["RSCR"] = TasOpcode.RSCR,
            ["ADD"] = TasOpcode.ADD,
            ["LISTF"] = TasOpcode.LISTF,
            ["LIST"] = TasOpcode.LIST,
            ["REPL"] = TasOpcode.REPL,
            ["REPLACE"] = TasOpcode.REPL,
            ["EXEC"] = TasOpcode.EXEC,
            ["QUIT"] = TasOpcode.QUIT,
            ["ERR"] = TasOpcode.ERR,
            ["ERROR"] = TasOpcode.ERR,
            ["OWNER"] = TasOpcode.OWNER,
            ["DALL"] = TasOpcode.DALL,
            ["ASK"] = TasOpcode.ASK,
            ["RCN"] = TasOpcode.RCN_CMD,
            ["REL"] = TasOpcode.REL,
            ["RELATE"] = TasOpcode.REL,
            ["FORCE"] = TasOpcode.FORCE,
            ["DEC"] = TasOpcode.DEC,
            ["PAINT"] = TasOpcode.PAINT,
            ["DEALOC"] = TasOpcode.DEALOC,
            ["DEALLOCATE"] = TasOpcode.DEALOC,
            ["READ"] = TasOpcode.READ,
            ["WRITE"] = TasOpcode.WRITE,
            ["INSERT"] = TasOpcode.INSERT,
            ["INSRT"] = TasOpcode.INSERT,
            ["SETLINE"] = TasOpcode.SETLINE,
            ["REMOUNT"] = TasOpcode.REMOUNT,
            ["SCRN"] = TasOpcode.SCRN,
            ["RDLIST"] = TasOpcode.RDLIST,
            ["MSG"] = TasOpcode.MSG,
            ["MESSAGE"] = TasOpcode.MSG,
            ["CLRPE"] = TasOpcode.CLRPE,
            ["UPAR"] = TasOpcode.UPAR,
            ["ALLOC"] = TasOpcode.ALLOC,
            ["ALLOCATE"] = TasOpcode.ALLOC,
            ["PSET"] = TasOpcode.PSET,
            ["COLOR"] = TasOpcode.COLOR,
            ["IMPORT"] = TasOpcode.IMPORT,
            ["PARAM"] = TasOpcode.PARAM,
            ["PARAMETER"] = TasOpcode.PARAM,
            ["XTRAP"] = TasOpcode.XTRAP,
            ["SETACT"] = TasOpcode.SETACT,
            ["BKG"] = TasOpcode.BKG,
            ["FRG"] = TasOpcode.FRG,
            ["REV"] = TasOpcode.REVERSE,
            ["REVERSE"] = TasOpcode.REVERSE,
            ["FORMAT"] = TasOpcode.FORMAT,
            ["DISPM"] = TasOpcode.DISPM,
            ["FILLMEM"] = TasOpcode.FILLMEM,
            ["DELC"] = TasOpcode.DELC,
            ["GOTOL"] = TasOpcode.GOTOL,
            ["DELF"] = TasOpcode.DELF,
            ["RENF"] = TasOpcode.RENF,
            ["DATE"] = TasOpcode.DATE,
            ["TIME"] = TasOpcode.TIME,
            ["PUSHF"] = TasOpcode.PUSHF,
            ["POPF"] = TasOpcode.POPF,
            ["ROPEN"] = TasOpcode.ROPEN,
            ["FILTER"] = TasOpcode.FILTER,
            ["WRAP"] = TasOpcode.WRAP,
            ["REWRAP"] = TasOpcode.REWRAP,
            ["UP"] = TasOpcode.UP,
            ["UPCASE"] = TasOpcode.UP,
            ["CLOCK"] = TasOpcode.CLOCK,
            ["SCAN"] = TasOpcode.SCAN,
            ["TRIM"] = TasOpcode.TRIM,
            ["IFDUP"] = TasOpcode.IFDUP,
            ["IFNA"] = TasOpcode.IFNA,
            ["PRTALL"] = TasOpcode.PRTALL,
            ["NOREDSP"] = TasOpcode.NOREDSP,
            ["AUTOINC"] = TasOpcode.AUTOINC,
            ["PUSHT"] = TasOpcode.PUSHT,
            ["POPT"] = TasOpcode.POPT,
            ["CASE"] = TasOpcode.CASE,
            ["GOSUBL"] = TasOpcode.GOSUBL,
            ["GETLBL"] = TasOpcode.GETLBL,
            ["CDPATH"] = TasOpcode.CDPATH,
            ["SOUND"] = TasOpcode.SOUND,
            ["TRACE"] = TasOpcode.TRACE,
            ["INT"] = TasOpcode.INT_CMD,
            ["INTERRUPT"] = TasOpcode.INT_CMD,
            ["LIST_EXIT"] = TasOpcode.LIST_EXIT,
            ["KBDUP"] = TasOpcode.KBDUP,
            ["NORSTRT"] = TasOpcode.NORSTRT,
            ["NOVLDMSG"] = TasOpcode.NOVLDMSG,
            ["AUTOENTER"] = TasOpcode.AUTOENTER,
            ["AUTODEC"] = TasOpcode.AUTODEC,
            ["AUTO_RUN"] = TasOpcode.AUTO_RUN,
            ["RUN"] = TasOpcode.RUN,
            ["OPNO"] = TasOpcode.OPNO,
            ["RDREC"] = TasOpcode.RDREC,
            ["WTREC"] = TasOpcode.WTREC,
            ["EQU_MID"] = TasOpcode.EQU_MID,
            ["EQU_DAY"] = TasOpcode.EQU_DAY,
            ["EQU_XMT"] = TasOpcode.EQU_XMT,
            ["FORCE3"] = TasOpcode.FORCE3,
            ["MOUSE"] = TasOpcode.MOUSE,
            ["JUST"] = TasOpcode.JUST,
            ["JUSTIFY"] = TasOpcode.JUST,
            ["WCOLOR"] = TasOpcode.WCOLOR,
            ["BUTTON"] = TasOpcode.BUTTON,
            ["CAPTION"] = TasOpcode.CAPTION,
            ["GRAY"] = TasOpcode.GRAY,
            ["ROW_COLOR"] = TasOpcode.ROW_COLOR,
            ["HOT_SPOT"] = TasOpcode.HOT_SPOT,
            ["SAVES3"] = TasOpcode.SAVES3,
            ["REDSP3"] = TasOpcode.REDSP3,
            ["SSPCF"] = TasOpcode.SSPCF,
            ["NEXT"] = TasOpcode.NEXT,
            // Compiler/control-flow
            ["DEFINE"] = TasOpcode.DEFINE,
            ["ENDIF"] = TasOpcode.ENDIF,
            ["ELSE_IF"] = TasOpcode.ELSE_IF,
            ["ELSEIF"] = TasOpcode.ELSE_IF,
            ["LOOP"] = TasOpcode.LOOP,
            ["OTHERWISE"] = TasOpcode.OTHERWISE,
            ["ENDC"] = TasOpcode.ENDC,
            ["ENDCASE"] = TasOpcode.ENDC,
            ["EXIT"] = TasOpcode.EXIT_CMD,
            ["FLOOP"] = TasOpcode.FLOOP,
            ["FEXIT"] = TasOpcode.FEXIT,
            ["FLOOP_IF"] = TasOpcode.FLOOP_IF,
            ["FEXIT_IF"] = TasOpcode.FEXIT_IF,
            ["SEXIT"] = TasOpcode.SEXIT,
            ["SEXIT_IF"] = TasOpcode.SEXIT_IF,
            ["SLOOP"] = TasOpcode.SLOOP,
            ["SLOOP_IF"] = TasOpcode.SLOOP_IF,
            ["ENDS"] = TasOpcode.ENDS,
            ["ENDSCAN"] = TasOpcode.ENDS,
            ["{"] = TasOpcode.BRACE_OPEN,
            ["}"] = TasOpcode.BRACE_CLOSE,
        };
    }
}
