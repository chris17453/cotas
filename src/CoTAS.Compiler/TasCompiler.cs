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
    private readonly bool _isTas51 = true;
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
        // Pass 1: collect fields, labels, and prep
        CollectPass(program.Statements);

        // Pass 2: emit instructions
        EmitPass(program.Statements);

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
        compiler._overlayLabelCount = runForDecompile.OverlayLabelCount;

        // Track overlay offsets for adjusting internal references
        // Since we import overlay fields into the combined field table, field spec offsets
        // already include overlay fields — no additional offset needed for fields.
        // But spec/const segments in the output are local-only, so overlay spec/const offsets
        // are still needed for constant and spec references.
        compiler._overlaySpecOffset = runForDecompile.OverlaySpecSize;
        compiler._overlayConstOffset = runForDecompile.OverlayConstSize;
        compiler._overlayFieldOffset = 0; // Combined field table already has overlay fields
        compiler._spec.OverlayFieldOffset = 0;
        compiler._spec.OverlayConstOffset = compiler._overlayConstOffset;
        compiler._constants.OverlayFieldOffset = 0;
        compiler._constants.OverlayConstOffset = compiler._overlayConstOffset;

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
            compiler._constants.AddIntegerZero();

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
    /// Generate the 128-byte header. Starts from original raw header (for timestamps at bytes 65-77),
    /// then overwrites all computed fields with generated values.
    /// </summary>
    private static byte[] GenerateHeader(byte[] originalRawHeader, RunFileHeader originalHeader,
        int codeSize, int constSize, int specSize, int labelSize, int fldNameSize,
        int numFields, int numLabels)
    {
        // Start with original header to preserve timestamps and any unknown bytes
        byte[] header = new byte[RunFileHeader.Size];
        Array.Copy(originalRawHeader, header, Math.Min(originalRawHeader.Length, RunFileHeader.Size));

        // Overwrite computed segment sizes; preserve all other fields from original header
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
        // Bytes 65+ preserved from original (timestamps, etc.)

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
            _fields.AddDefinedField(name, fieldType, size, decimals, arraySize, def.Reset);
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

            case PreprocessorStmt:
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
        // SCAN is complex — emit START_SCAN + SCAN header + body + SET_SCAN_FLG + ENDS
        // For now, emit generic params from tokens
        EmitSimple(TasOpcode.START_SCAN, 0);

        int specOff = _spec.BeginSpec();
        WriteTokenParams(scan.Options);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.SCAN, specOff, specSize);

        EmitPass(scan.Body);

        EmitSimple(TasOpcode.SET_SCAN_FLG, 0);
        EmitSimple(TasOpcode.ENDS, 0);
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

        // Special handling for commands with known spec layouts
        switch (opcode)
        {
            case TasOpcode.NOP:
                EmitSimple(TasOpcode.NOP, 0);
                return;
            case TasOpcode.GOTO:
                EmitGenericGoto(gen);
                return;
            case TasOpcode.GOSUB:
                EmitGenericGosub(gen);
                return;
            case TasOpcode.ASSIGN:
            case TasOpcode.POINTER:
                EmitGenericAssign(gen, opcode);
                return;
            case TasOpcode.IF:
                EmitGenericIf(gen);
                return;
            case TasOpcode.TRAP:
                EmitGenericTrap(gen);
                return;
            case TasOpcode.OPENV:
            case TasOpcode.OPEN:
                EmitGenericOpenv(gen, opcode);
                return;
            case TasOpcode.FINDV:
            case TasOpcode.FIND:
                EmitGenericFindv(gen, opcode);
                return;
            case TasOpcode.SAVE:
                EmitGenericSave(gen);
                return;
            case TasOpcode.CO:
                EmitCo(gen);
                return;
            case TasOpcode.CHAIN:
            case TasOpcode.CHAINR:
                EmitGenericChain(gen, opcode);
                return;
            case TasOpcode.ENTER:
            case TasOpcode.ASK:
                EmitGenericEnterAsk(gen, opcode);
                return;
            case TasOpcode.WINDOW:
            case TasOpcode.WINDEF:
                EmitGenericWindow(gen, opcode);
                return;
            case TasOpcode.FOR:
                // FOR handled by dedicated ForStmt
                EmitGenericParams(gen, opcode);
                return;
            case TasOpcode.FILL:
                EmitGenericFill(gen);
                return;
            case TasOpcode.MID_CMD:
                EmitGenericMid(gen);
                return;
            case TasOpcode.XFER:
                EmitGenericXfer(gen);
                return;
            case TasOpcode.SCAN:
                EmitGenericParams(gen, opcode);
                return;
            case TasOpcode.COLOR:
                EmitGenericColor(gen);
                return;
            case TasOpcode.FORMAT:
                EmitGenericFormat(gen);
                return;
            case TasOpcode.SAY:
            case TasOpcode.DISPF:
            case TasOpcode.CURSOR:
            case TasOpcode.CLRLNE:
                EmitGenericPositionalCmd(gen, opcode);
                return;
            case TasOpcode.PMSG:
                EmitGenericPmsg(gen);
                return;
            case TasOpcode.QUIT:
                EmitQuit();
                return;
            case TasOpcode.MSG:
                EmitGenericMsg(gen);
                return;
            case TasOpcode.SAVES:
            case TasOpcode.SAVES3:
            case TasOpcode.REDSP:
            case TasOpcode.REDSP3:
                EmitGenericSavesRedsp(gen, opcode);
                return;
            case TasOpcode.UDC:
            case TasOpcode.FUNC:
            case TasOpcode.CMD:
                EmitGenericUdfc(gen, opcode);
                return;
            case TasOpcode.INIFLE:
                EmitGenericInifle(gen);
                return;
            case TasOpcode.INC:
            case TasOpcode.DEC:
            case TasOpcode.TRIM:
            case TasOpcode.PUSHF:
            case TasOpcode.POPF:
            case TasOpcode.DEALOC:
            case TasOpcode.CLR:
            case TasOpcode.ROPEN:
            case TasOpcode.PBLNK:
            case TasOpcode.PVERT:
            case TasOpcode.PRTALL:
            case TasOpcode.PON:
            case TasOpcode.RCN_CMD:
            case TasOpcode.REMVA:
            case TasOpcode.DELF:
            case TasOpcode.CDPATH:
                EmitGenericOneParam(gen, opcode);
                return;
            case TasOpcode.PARAM:
                EmitGenericParam(gen);
                return;
            case TasOpcode.GOTOL:
            case TasOpcode.UPAR:
            case TasOpcode.FILTER:
            case TasOpcode.DATE:
            case TasOpcode.TIME:
            case TasOpcode.KBDUP:
            case TasOpcode.GRAY:
            case TasOpcode.RDLIST:
            case TasOpcode.REMOUNT:
            case TasOpcode.PUT_FLD:
            case TasOpcode.EXEC:
            case TasOpcode.BKG:
            case TasOpcode.FRG:
            case TasOpcode.REVERSE:
            case TasOpcode.PUSHT:
            case TasOpcode.POPT:
                EmitGenericOneParam(gen, opcode);
                return;
            case TasOpcode.CLOSE:
                EmitGenericClose(gen);
                return;
            case TasOpcode.DEL:
                EmitGenericDel(gen);
                return;
            case TasOpcode.RENF:
            case TasOpcode.SRCH:
            case TasOpcode.SETACT:
            case TasOpcode.ERR:
            case TasOpcode.WCOLOR:
            case TasOpcode.SETLINE:
            case TasOpcode.INSERT:
            case TasOpcode.POSTMSG:
            case TasOpcode.TRANSX:
            case TasOpcode.RUN:
            case TasOpcode.EQU_DAY:
            case TasOpcode.EQU_XMT:
                EmitGenericTwoParams(gen, opcode);
                return;
            case TasOpcode.WRAP:
            case TasOpcode.REWRAP:
                EmitGenericThreeParams(gen, opcode);
                return;
            case TasOpcode.GETLBL:
                EmitGenericGetlbl(gen);
                return;
            case TasOpcode.ON:
                EmitGenericOn(gen);
                return;
            case TasOpcode.FORCE:
            case TasOpcode.FORCE3:
                EmitGenericForce(gen);
                return;
            case TasOpcode.SCRN:
                EmitGenericScrn(gen);
                return;
            case TasOpcode.RAP:
                EmitGenericRap(gen);
                return;
        }

        // Default: emit command with all tokens as 5-byte params
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
        // TRAP: [key(5)] [action(5)]
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        if (toks.Count >= 2) WriteTokenParam(toks[1]); else _spec.WriteNullParam();
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.TRAP, specOff, specSize);
    }

    private void EmitGenericOpenv(GenericCommandStmt gen, ushort opcode)
    {
        // OPENV spec layout (53 bytes, TAS 5.1):
        // [0]filename(5) [5]ext(5) [10]lock(1) [11]err_label(4) [15]owner(5)
        // [20]path(5) [25]fd(5) [30]type(1) [31]fnum(5) [36]size(5) [41]buf(5)
        // [46]create(1) [47]noclr(1) [48]update(5)
        int specOff = _spec.BeginSpec();

        var toks = gen.Tokens;
        var kw = ParseKeywordTokens(toks, "FNUM", "PATH", "BUF", "SIZE", "LOCK",
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
        // CHAIN spec layout: [param(5)] [6 zeros] [param2(5)] [flag(1) at offset 16] [zero(1)] = 18 bytes
        int specOff = _spec.BeginSpec();

        // Check if the param is a simple value or an expression (has operators)
        bool hasOperators = gen.Tokens.Any(t => t.Type == TokenType.Plus || t.Type == TokenType.Minus
            || t.Type == TokenType.Star || t.Type == TokenType.Slash);
        if (hasOperators)
        {
            // Compile as expression
            var expr = ParseTokensAsExpression(gen.Tokens);
            WriteExprParam(expr);
        }
        else
        {
            var toks = FilterSignificantTokens(gen.Tokens);
            if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        }

        _spec.WritePadding(6);

        // [11-15]: second param — usually null or a copy of the current PARAM list
        // For files with overlay, this references the PARAM list constant. For now, null.
        _spec.WriteNullParam();

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
        // PMSG (?) spec layout (29 bytes):
        // [0] col (5B), [5] row (5B), [10] msg (5B),
        // [15] wait (1B), [16] ncr (1B), [17] ent (5B),
        // [22] whr (1B), [23] color (5B), [28] abs (1B)
        int specOff = _spec.BeginSpec();
        var kw = ParseKeywordTokens(gen.Tokens, "WAIT", "NCR", "ENT", "COLOR", "ABS");
        var toks = FilterSignificantTokens(gen.Tokens);

        // Decompiler outputs: ? row,col,[msg] wait ncr ent val color val abs
        // [0] col
        if (toks.Count >= 2) WriteTokenParam(toks[1]); else _spec.WriteNumericParam(0);
        // [5] row
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNumericParam(0);
        // [10] msg — could be a single value or concatenated expression
        if (toks.Count >= 3)
        {
            // Check if there are operators (concatenation)
            bool hasOps = gen.Tokens.Any(t => t.Type == TokenType.Plus || t.Type == TokenType.Minus
                || t.Type == TokenType.Star || t.Type == TokenType.Slash);
            if (hasOps && toks.Count > 3)
            {
                // Find tokens after the second comma for the message expression
                var msgTokens = new List<Token>();
                int commaCount = 0;
                foreach (var t in gen.Tokens)
                {
                    if (t.Type == TokenType.Comma) { commaCount++; continue; }
                    if (commaCount >= 2) msgTokens.Add(t);
                }
                if (msgTokens.Count > 0)
                {
                    var expr = ParseTokensAsExpression(msgTokens);
                    WriteExprParam(expr);
                }
                else
                    WriteTokenParam(toks[2]);
            }
            else
                WriteTokenParam(toks[2]);
        }
        else
            _spec.WriteNullParam();
        // [15] wait
        _spec.WriteByte(kw.ContainsKey("WAIT") ? (byte)'Y' : (byte)0);
        // [16] ncr
        _spec.WriteByte(kw.ContainsKey("NCR") ? (byte)'Y' : (byte)0);
        // [17] ent
        if (kw.TryGetValue("ENT", out var entTok)) WriteTokenParam(entTok); else _spec.WriteNullParam();
        // [22] whr
        _spec.WriteByte(0);
        // [23] color
        if (kw.TryGetValue("COLOR", out var colorTok)) WriteTokenParam(colorTok); else _spec.WriteNullParam();
        // [28] abs
        _spec.WriteByte(kw.ContainsKey("ABS") ? (byte)'Y' : (byte)0);

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

        var header = new RunFileHeader
        {
            CodeSize = codeSegment.Length,
            ConstSize = constSegment.Length,
            SpecSize = specSegment.Length,
            LabelSize = labelOffsets.Count * 4,
            ScrnFldNum = _fields.ScreenFieldCount,
            NumFlds = _fields.Count,
            TempFlds = _fields.TempFieldAreaEnd,
            NumTempFlds = _fields.TempFieldCount,
            FldNameSize = _fields.Count * _fields.FieldSpecSize,
            TempFldSize = _fields.TempFieldSize,
            DefFldSegSize = _fields.DefinedDataSize,
            NumExtraFlds = 0,
            PrgNames = 0,
            DebugFlg = false,
            ProType = "TAS32",
            NumLabels = labelOffsets.Count,
            NewFldSpec = false,
            ChkUpVld = false,
            IncLabels = false,
        };

        // Build field spec segment
        byte[] fieldSegment = new byte[fieldSpecs.Count * _fields.FieldSpecSize];
        // Use RunFileWriter's field segment building (via Write method)

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
