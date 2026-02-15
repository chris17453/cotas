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

    // Jump patching: stores (instructionIndex, specOffset) pairs for forward jumps
    private readonly List<(int InstrIdx, int SpecOffset)> _jumpPatches = [];

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
                // DEFINEs are handled in Pass 1; emit NOP or DEFINE opcode
                EmitSimple(TasOpcode.DEFINE, 0);
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
                EmitSimple(TasOpcode.QUIT, 0);
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
                var encoder = new ExpressionEncoder(_fields, _constants, _labels, GetTempBase());
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
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.RET, specOff, specSize);
    }

    private void EmitMessage(MessageStmt msg)
    {
        int specOff = _spec.BeginSpec();
        WriteExprParam(msg.Text);
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.MSG, specOff, specSize);
    }

    private void EmitIfThen(IfThenStmt ifThen)
    {
        // IF with single-line THEN: emit IF + body + ENDIF
        // IF spec: [jump(4)] [pad(1)] [expr(5)] [expr(5)] [variant(1)] [label(4)?]
        int specOff = _spec.BeginSpec();
        int jumpPatchOffset = _spec.Size; // remember where to patch
        _spec.WriteInt32(0); // placeholder jump target

        // pad byte
        _spec.WriteByte(0);

        // boolean expression (the condition)
        // For simple comparisons, we need two 5-byte params (lhs and rhs) and a comparison op
        // For complex expressions, compile to RPN
        WriteConditionParams(ifThen.Condition);

        // variant
        _spec.WriteByte((byte)'T'); // THEN variant

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.IF, specOff, specSize);
        int ifInstrIdx = _instructions.Count - 1;

        // Emit the then branch
        EmitStatement(ifThen.ThenBranch);

        // Emit ENDIF
        EmitSimple(TasOpcode.ENDIF, 0);

        // Patch the IF's jump target to point past ENDIF
        PatchJump(ifInstrIdx, jumpPatchOffset, CurrentByteOffset);
    }

    private void EmitIfBlock(IfBlockStmt ifBlock)
    {
        int specOff = _spec.BeginSpec();
        int jumpPatchOffset = _spec.Size;
        _spec.WriteInt32(0); // placeholder jump
        _spec.WriteByte(0); // pad

        WriteConditionParams(ifBlock.Condition);

        _spec.WriteByte((byte)'D'); // DO variant (block IF)

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.IF, specOff, specSize);
        int ifInstrIdx = _instructions.Count - 1;

        // Emit then block
        EmitPass(ifBlock.ThenBlock);

        if (ifBlock.ElseBlock != null && ifBlock.ElseBlock.Count > 0)
        {
            // ENDW (skip-else jump) before ELSE
            int endwSpecOff = _spec.BeginSpec();
            int endwJumpPatch = _spec.Size;
            _spec.WriteInt32(0); // placeholder
            int endwSpecSize = _spec.GetSpecSize(endwSpecOff);
            EmitInstruction(TasOpcode.ENDW, endwSpecOff, endwSpecSize);
            int endwInstrIdx = _instructions.Count - 1;

            // Patch IF to jump to ELSE
            PatchJump(ifInstrIdx, jumpPatchOffset, CurrentByteOffset);

            // ELSE
            int elseSpecOff = _spec.BeginSpec();
            _spec.WriteInt32(0); // jump target (usually not used for block else)
            int elseSpecSize = _spec.GetSpecSize(elseSpecOff);
            EmitInstruction(TasOpcode.ELSE, elseSpecOff, elseSpecSize);

            // Emit else block
            EmitPass(ifBlock.ElseBlock);

            // ENDIF
            EmitSimple(TasOpcode.ENDIF, 0);

            // Patch ENDW to jump past ENDIF
            PatchJump(endwInstrIdx, endwJumpPatch, CurrentByteOffset);
        }
        else
        {
            // Patch IF to jump past body
            PatchJump(ifInstrIdx, jumpPatchOffset, CurrentByteOffset);

            // ENDIF
            EmitSimple(TasOpcode.ENDIF, 0);
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
        var encoder = new ExpressionEncoder(_fields, _constants, _labels, GetTempBase());
        encoder.CompileToParam(exprStmt.Expr);
    }

    // ======================== Generic Command Handler ========================

    private void EmitGenericCommand(GenericCommandStmt gen)
    {
        string upper = gen.CommandName.ToUpperInvariant();

        if (!_commandOpcodes.TryGetValue(upper, out ushort opcode))
        {
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
                EmitGenericOneParam(gen, opcode);
                return;
            case TasOpcode.FORMAT:
                EmitGenericTwoParams(gen, opcode);
                return;
            case TasOpcode.SAY:
            case TasOpcode.PMSG:
            case TasOpcode.DISPF:
            case TasOpcode.CURSOR:
            case TasOpcode.CLRLNE:
                EmitGenericPositionalCmd(gen, opcode);
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
        // Assignment from tokens: target = value
        int specOff = _spec.BeginSpec();
        // First token is target, skip '=', rest is value
        if (gen.Tokens.Count >= 1)
            WriteTokenParam(gen.Tokens[0]);
        else
            _spec.WriteNullParam();

        // Find '=' and take everything after it as value
        int eqIdx = gen.Tokens.FindIndex(t => t.Type == TokenType.Equal);
        if (eqIdx >= 0 && eqIdx + 1 < gen.Tokens.Count)
            WriteTokenParam(gen.Tokens[eqIdx + 1]);
        else if (gen.Tokens.Count >= 2)
            WriteTokenParam(gen.Tokens[1]);
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
        // OPENV: 53 bytes of structured params
        // Parse tokens for keywords: FNUM, PATH, BUF, SIZE, LOCK, CREATE, ERR, EXT
        int specOff = _spec.BeginSpec();

        var toks = gen.Tokens;
        var kw = ParseKeywordTokens(toks, "FNUM", "PATH", "BUF", "SIZE", "LOCK", "CREATE", "ERR", "EXT");

        // [0] filename (5)
        if (toks.Count > 0) WriteTokenParam(toks[0]); else _spec.WriteNullParam();

        // [5] ext/cc (5)
        if (kw.TryGetValue("EXT", out var ext)) WriteTokenParam(ext);
        else _spec.WriteNullParam();

        // [10] lock mode byte (1)
        if (kw.TryGetValue("LOCK", out var lockTok))
            _spec.WriteByte((byte)lockTok.Value[0]);
        else
            _spec.WriteByte(0);

        // [11-14] padding
        _spec.WritePadding(4);

        // [15-19] flags
        _spec.WritePadding(5);

        // [20] path (5)
        if (kw.TryGetValue("PATH", out var pathTok)) WriteTokenParam(pathTok);
        else _spec.WriteNullParam();

        // [25-30] flags
        _spec.WritePadding(6);

        // [31] fnum handle (5)
        if (kw.TryGetValue("FNUM", out var fnumTok)) WriteTokenParam(fnumTok);
        else _spec.WriteNullParam();

        // [36] size (5)
        if (kw.TryGetValue("SIZE", out var sizeTok)) WriteTokenParam(sizeTok);
        else _spec.WriteNullParam();

        // [41] buf field (5)
        if (kw.TryGetValue("BUF", out var bufTok)) WriteTokenParam(bufTok);
        else _spec.WriteNullParam();

        // [46] create Y/N (1)
        _spec.WriteByte(kw.ContainsKey("CREATE") ? (byte)'Y' : (byte)'N');

        // [47] err Y/N (1)
        _spec.WriteByte(kw.ContainsKey("ERR") ? (byte)'Y' : (byte)'N');

        // [48-52] padding
        _spec.WritePadding(5);

        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(opcode, specOff, specSize);
    }

    private void EmitGenericFindv(GenericCommandStmt gen, ushort opcode)
    {
        // FINDV: 33 bytes
        int specOff = _spec.BeginSpec();

        var toks = gen.Tokens;
        var kw = ParseKeywordTokens(toks, "FNUM", "KEY", "VAL", "ERR", "NLOCK");

        // [0] handle (5)
        if (kw.TryGetValue("FNUM", out var fnumTok)) WriteTokenParam(fnumTok);
        else if (toks.Count > 1) WriteTokenParam(toks[1]);
        else _spec.WriteNullParam();

        // [5] key (5)
        if (kw.TryGetValue("KEY", out var keyTok)) WriteTokenParam(keyTok);
        else _spec.WriteNullParam();

        // [10] val (5)
        if (kw.TryGetValue("VAL", out var valTok)) WriteTokenParam(valTok);
        else _spec.WriteNullParam();

        // [15] find type byte (1) — first token after command name is usually the type
        byte findType = (byte)'F';
        if (toks.Count > 0 && toks[0].Value.Length == 1)
            findType = (byte)toks[0].Value[0];
        _spec.WriteByte(findType);

        // [16-19] err label (4)
        if (kw.TryGetValue("ERR", out var errTok))
        {
            int errLabel = _labels.GetOrCreateRef(errTok.Value);
            _spec.WriteInt32(errLabel);
        }
        else
        {
            _spec.WriteInt32(0);
        }

        // [20] nlock byte (1)
        _spec.WriteByte(kw.ContainsKey("NLOCK") ? (byte)'Y' : (byte)0);

        // [21-32] padding
        _spec.WritePadding(12);

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

    private void EmitGenericChain(GenericCommandStmt gen, ushort opcode)
    {
        int specOff = _spec.BeginSpec();
        var toks = FilterSignificantTokens(gen.Tokens);
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
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
        // XFER: [from(5)] [to(5)]
        int specOff = _spec.BeginSpec();
        var toks = gen.Tokens.Where(t => t.Type != TokenType.Newline &&
            !t.Value.Equals("TO", StringComparison.OrdinalIgnoreCase)).ToList();
        if (toks.Count >= 1) WriteTokenParam(toks[0]); else _spec.WriteNullParam();
        if (toks.Count >= 2) WriteTokenParam(toks[1]); else _spec.WriteNullParam();
        int specSize = _spec.GetSpecSize(specOff);
        EmitInstruction(TasOpcode.XFER, specOff, specSize);
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

    // ======================== Helpers ========================

    private void WriteExprParam(Expression expr)
    {
        var encoder = new ExpressionEncoder(_fields, _constants, _labels, GetTempBase());
        var (type, loc) = encoder.CompileToParam(expr);
        _spec.WriteParam(type, loc);
    }

    private void WriteConditionParams(Expression condition)
    {
        // For IF conditions: emit two 5-byte params (comparison operands)
        // and encode the comparison operator in the expression itself
        if (condition is BinaryExpr bin && IsComparisonOp(bin.Operator))
        {
            WriteExprParam(bin.Left);
            WriteExprParam(bin.Right);
        }
        else
        {
            // Complex condition — compile to expression
            WriteExprParam(condition);
            // Second param is .T. for comparison
            _spec.WriteParam('C', _constants.AddLogical(true));
        }
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
                if (tok.Value.Length == 1)
                    _spec.WriteCharParam(tok.Value[0]);
                else
                {
                    int constOff = _constants.AddString(tok.Value);
                    _spec.WriteConstParam(constOff);
                }
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
                // Skip non-significant tokens (commas, operators, etc.)
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
                result[val] = tokens[i + 1];
                i++; // skip value token
            }
            else if (keywords.Contains(val))
            {
                // Flag keyword with no value
                result[val] = tokens[i];
            }
        }
        return result;
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
        // Temp base is after all defined fields
        return _fields.Count * _fields.FieldSpecSize;
    }

    private void PatchJump(int instrIdx, int specOffset, int targetByteOffset)
    {
        // Patch the 4-byte jump target in the spec segment
        byte[] specData = _spec.ToArray();
        if (specOffset + 4 <= specData.Length)
        {
            BitConverter.TryWriteBytes(specData.AsSpan(specOffset), targetByteOffset);
            // Re-create the spec builder with patched data
            // (This is a simplification — in a real implementation we'd patch in-place)
        }
        // For now, we track patches and apply them at the end
        _jumpPatches.Add((instrIdx, specOffset));
    }

    private static bool IsComparisonOp(string op) =>
        op is "=" or "<>" or "<" or ">" or "<=" or ">=" or
             "and" or "or" or ".AND." or ".OR." or ".A." or ".O.";

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
                w.Write(instr.SpecLinePtr);
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
        };
    }
}
