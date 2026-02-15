using System.Text;

namespace CoTAS.Parser.RunFile;

/// <summary>
/// Decompiles TAS .RUN bytecode back to readable pseudo-source.
/// Uses the opcode table, field list, constant segment, and spec decoder
/// to reconstruct something close to the original TAS source code.
/// </summary>
public sealed class RunFileDecompiler
{
    private readonly RunFileReader _run;
    private readonly SpecDecoder _spec;
    private readonly Dictionary<int, List<int>> _labelsByInstr;
    private readonly int _instrSize;
    // Block stack tracks what kind of block each ENDW closes
    private readonly Stack<string> _blockStack = new();

    public RunFileDecompiler(RunFileReader run)
    {
        _run = run;
        _spec = new SpecDecoder(run);
        _instrSize = run.Header.ProType == "TAS32"
            ? RunFileHeader.Tas51InstructionSize
            : RunFileHeader.Tas60InstructionSize;

        // Build label-to-instruction index map
        _labelsByInstr = new Dictionary<int, List<int>>();
        for (int i = 0; i < run.LabelOffsets.Count; i++)
        {
            int instrIdx = run.LabelOffsets[i] / _instrSize;
            if (!_labelsByInstr.ContainsKey(instrIdx))
                _labelsByInstr[instrIdx] = [];
            _labelsByInstr[instrIdx].Add(i);
        }
    }

    /// <summary>
    /// Decompile the entire program to pseudo-source lines.
    /// </summary>
    public string Decompile()
    {
        var sb = new StringBuilder();

        // Header comment
        sb.AppendLine($"; Decompiled from {Path.GetFileName("")}");
        sb.AppendLine($"; Fields: {_run.Header.NumFlds}, Labels: {_run.Header.NumLabels}, Instructions: {_run.Instructions.Count}");
        sb.AppendLine();

        // Field definitions (non-temp, non-file)
        foreach (var f in _run.GetDefinedFields())
        {
            if (string.IsNullOrWhiteSpace(f.Name) || f.Name == "?") continue;
            string arr = f.ArrayCount > 0 ? $" array {f.ArrayCount}" : "";
            string dec = f.Decimals > 0 ? $" dec {f.Decimals}" : "";
            string reset = f.IsReset ? " reset" : "";
            sb.AppendLine($"define {f.Name} type {f.FieldType} size {f.DisplaySize}{dec}{arr}{reset}");
        }
        sb.AppendLine();

        // Instructions
        _blockStack.Clear();
        for (int i = 0; i < _run.Instructions.Count; i++)
        {
            // Label markers
            if (_labelsByInstr.TryGetValue(i, out var labels))
            {
                foreach (var li in labels)
                    sb.AppendLine($"LABEL_{li}:");
            }

            var instr = _run.Instructions[i];

            // Track block nesting for ENDW disambiguation.
            // Only push WHILE and FOR — they always close with ENDW (opcode 69).
            // IFs often lack ENDW (optimized away when block ends with RET/QUIT).
            // SCAN blocks end with SET_SCAN_FLG/ENDS, not ENDW.
            switch (instr.CommandNumber)
            {
                case TasOpcode.WHILE:
                    _blockStack.Push("while");
                    break;
                case TasOpcode.FOR:
                    _blockStack.Push("for");
                    break;
            }

            string line = DecompileInstruction(instr, i);
            if (line.Length > 0)
                sb.AppendLine($"  {line}");
        }

        return sb.ToString();
    }

    /// <summary>
    /// Decompile a single instruction to pseudo-source.
    /// </summary>
    public string DecompileInstruction(RunBytecodeInstruction instr, int instrIndex)
    {
        var spec = _spec.GetSpecBytes(instr);

        return instr.CommandNumber switch
        {
            TasOpcode.NOP => "",  // skip no-ops
            TasOpcode.ASSIGN => DecompileAssign(spec),
            TasOpcode.IF => DecompileIf(spec),
            TasOpcode.ELSE => DecompileElse(spec),
            TasOpcode.ENDIF => "endif",
            TasOpcode.ENDW => DecompileEndw(spec, instrIndex),
            TasOpcode.GOTO => DecompileGoto(spec),
            TasOpcode.GOSUB => DecompileGosub(spec),
            TasOpcode.RET => DecompileRet(spec),
            TasOpcode.QUIT => "quit",
            TasOpcode.SAY => DecompileSay(spec),
            TasOpcode.PMSG => DecompilePmsg(spec),
            TasOpcode.MSG => DecompileMsg(spec),
            TasOpcode.CLRSCR => "clrscr",
            TasOpcode.BELL => "bell",
            TasOpcode.INC => DecompileIncDec(spec, "inc"),
            TasOpcode.DEC => DecompileIncDec(spec, "dec"),
            TasOpcode.OPENV => DecompileOpenv(spec),
            TasOpcode.OPEN => DecompileOpenv(spec),  // similar format
            TasOpcode.FINDV => DecompileFindv(spec),
            TasOpcode.FIND => DecompileFindv(spec),
            TasOpcode.CLOSE => DecompileClose(spec),
            TasOpcode.CLR => DecompileClr(spec),
            TasOpcode.SAVE => DecompileSave(spec),
            TasOpcode.CHAIN => DecompileChain(spec),
            TasOpcode.TRAP => DecompileTrap(spec),
            TasOpcode.ENTER => DecompileEnter(spec),
            TasOpcode.ASK => DecompileAsk(spec),
            TasOpcode.WHILE => DecompileWhile(spec),
            TasOpcode.FOR => DecompileFor(spec),
            TasOpcode.NEXT => "next",
            TasOpcode.SELECT => DecompileSelect(spec),
            TasOpcode.CASE => DecompileCase(spec),
            TasOpcode.OTHERWISE => "otherwise",
            TasOpcode.ENDC => "endc",
            TasOpcode.SCAN => DecompileScan(spec),
            TasOpcode.ENDS => "ends",
            TasOpcode.EXIT_CMD => "exit",
            TasOpcode.LOOP => "loop",
            TasOpcode.SEXIT => "sexit",
            TasOpcode.SLOOP => "sloop",
            TasOpcode.FLOOP_IF => DecompileCondBreak(spec, "floop_if"),
            TasOpcode.FEXIT_IF => DecompileCondBreak(spec, "fexit_if"),
            TasOpcode.SLOOP_IF => DecompileCondBreak(spec, "sloop_if"),
            TasOpcode.SEXIT_IF => DecompileCondBreak(spec, "sexit_if"),
            TasOpcode.FEXIT => "fexit",
            TasOpcode.FLOOP => "floop",
            TasOpcode.REENT => "reent",
            TasOpcode.CLRSF => "clrsf",
            TasOpcode.CLRPE => "clrpe",
            TasOpcode.NOREDSP => "noredsp",
            TasOpcode.NORSTRT => "norstrt",
            TasOpcode.NOVLDMSG => "novldmsg",
            TasOpcode.ULKALL => "ulkall",
            TasOpcode.RSCR => "rscr",
            TasOpcode.BRKRET => "brkret",
            TasOpcode.SAVES => DecompileSaves(spec),
            TasOpcode.REDSP => DecompileRedsp(spec),
            TasOpcode.ROPEN => DecompileRopen(spec),
            TasOpcode.COLOR => DecompileColor(spec),
            TasOpcode.FILL => DecompileFill(spec),
            TasOpcode.DISPF => DecompileDispf(spec),
            TasOpcode.CURSOR => DecompileCursor(spec),
            TasOpcode.CLRLNE => DecompileClrlne(spec),
            TasOpcode.PUSHT => DecompilePusht(spec),
            TasOpcode.POPT => DecompilePopt(spec),
            TasOpcode.PUSHF => DecompilePushPop(spec, "pushf"),
            TasOpcode.POPF => DecompilePushPop(spec, "popf"),
            TasOpcode.FORMAT => DecompileFormat(spec),
            TasOpcode.RAP => DecompileRap(spec),
            TasOpcode.UDC => DecompileUdfc(spec, instr, "udc"),
            TasOpcode.FUNC => DecompileUdfc(spec, instr, "func"),
            TasOpcode.CMD => DecompileUdfc(spec, instr, "cmd"),
            TasOpcode.RUN => DecompileRun(spec),
            TasOpcode.EXEC => DecompileExec(spec),
            TasOpcode.LOOP_IF => DecompileCondBreak(spec, "loop_if"),
            TasOpcode.EXIT_IF => DecompileCondBreak(spec, "exit_if"),
            TasOpcode.BUTTON => DecompileButton(spec),
            TasOpcode.WINDOW => DecompileWindow(spec, "window"),
            TasOpcode.WINDEF => DecompileWindow(spec, "windef"),
            TasOpcode.WINACT => DecompileOneParam(spec, "winact"),
            TasOpcode.MENU => DecompileMenuOld(spec),
            TasOpcode.NMENU => DecompileMenu(spec, "nmenu"),
            TasOpcode.UPAR => DecompileOneParam(spec, "upar"),
            TasOpcode.SCRN => DecompileScrn(spec),
            TasOpcode.XFER => DecompileXfer(spec),
            TasOpcode.TRIM => DecompileOneParam(spec, "trim"),
            TasOpcode.PSET => DecompilePset(spec),
            TasOpcode.DEALOC => DecompileOneParam(spec, "dealoc"),
            TasOpcode.ALLOC => DecompileOneParam(spec, "alloc"),
            TasOpcode.XTRAP => DecompileXtrap(spec),
            TasOpcode.UPDTA => DecompileUpdta(spec),
            TasOpcode.START_SCAN => "", // internal scan setup, skip
            TasOpcode.SET_SCAN_FLG => "", // internal scan flag, skip
            TasOpcode.PRTALL => DecompileOneParam(spec, "prtall"),
            TasOpcode.MOUNT => DecompileMount(spec),
            TasOpcode.SORTA => DecompileSorta(spec),
            TasOpcode.MID_CMD => DecompileMid(spec),
            TasOpcode.LISTM => DecompileListf(spec, "listm"),
            TasOpcode.LISTF => DecompileListf(spec, "listf"),
            TasOpcode.LIST => DecompileList(spec),
            TasOpcode.READ => DecompileRdwrt(spec, "read"),
            TasOpcode.WRITE => DecompileRdwrt(spec, "write"),
            TasOpcode.GRAY => DecompileOneParam(spec, "gray"),
            TasOpcode.CO => DecompileOneParam(spec, "co"),
            TasOpcode.CAPTION => DecompileCaption(spec),
            TasOpcode.PAINT => DecompilePaint(spec),
            TasOpcode.POINTER => DecompileAssign(spec),  // same as ASSIGN
            TasOpcode.DEL => DecompileDel(spec),
            TasOpcode.DALL => DecompileDall(spec),
            TasOpcode.RDA => DecompileRda(spec),
            TasOpcode.WRTA => DecompileWrta(spec),
            TasOpcode.REMVA => DecompileOneParam(spec, "remva"),
            TasOpcode.INSERT => DecompileInsert(spec),
            TasOpcode.REPL => DecompileRepl(spec),
            TasOpcode.FILTER => DecompileFilter(spec),
            TasOpcode.FORCE => DecompileForce(spec),
            TasOpcode.FORCE3 => DecompileForce3(spec),
            TasOpcode.SCROLL => DecompileScroll(spec),
            TasOpcode.SETACT => DecompileSetact(spec),
            TasOpcode.SETLINE => DecompileSetline(spec),
            TasOpcode.SRCH => DecompileSrch(spec),
            TasOpcode.INIFLE => DecompileOneParam(spec, "inifle"),
            TasOpcode.ON => DecompileOn(spec),
            TasOpcode.POSTMSG => DecompilePostmsg(spec),
            TasOpcode.PBLNK => DecompileOneParam(spec, "pblnk"),
            TasOpcode.PBOX => DecompilePbox(spec),
            TasOpcode.PCHR => DecompilePchr(spec),
            TasOpcode.PFMT => DecompilePfmt(spec),
            TasOpcode.PON => DecompilePon(spec),
            TasOpcode.PVERT => DecompileOneParam(spec, "pvert"),
            TasOpcode.PTOF => "ptof",
            TasOpcode.TRANSX => DecompileTransx(spec),
            TasOpcode.POPS => DecompileOneParam(spec, "pops"),
            TasOpcode.CLSO => "clso",
            TasOpcode.PUT_FLD => DecompilePutFld(spec),
            TasOpcode.SORT3 => DecompileSort3(spec),
            TasOpcode.EXPORT => DecompileExportImport(spec, "export"),
            TasOpcode.IMPORT => DecompileExportImport(spec, "import"),
            TasOpcode.ADD => DecompileAdd(spec),
            TasOpcode.ERR => DecompileErr(spec),
            TasOpcode.OWNER => DecompileOwner(spec),
            TasOpcode.RCN_CMD => DecompileRcn(spec),
            TasOpcode.REL => DecompileRel(spec),
            TasOpcode.REDEF => DecompileRedef(spec),
            TasOpcode.RDLIST => DecompileOneParam(spec, "rdlist"),
            TasOpcode.PARAM => DecompileParam(spec),
            TasOpcode.BKG => DecompileOneParam(spec, "bkg"),
            TasOpcode.FRG => DecompileOneParam(spec, "frg"),
            TasOpcode.REVERSE => DecompileOneParam(spec, "rev"),
            TasOpcode.DISPM => DecompileDispm(spec),
            TasOpcode.FILLMEM => DecompileFillmem(spec),
            TasOpcode.DELC => DecompileDelc(spec),
            TasOpcode.GOTOL => DecompileOneParam(spec, "gotol"),
            TasOpcode.DELF => DecompileOneParam(spec, "delf"),
            TasOpcode.RENF => DecompileRenf(spec),
            TasOpcode.DATE => DecompileOneParam(spec, "date"),
            TasOpcode.TIME => DecompileOneParam(spec, "time"),
            TasOpcode.WRAP => DecompileWrap(spec),
            TasOpcode.REWRAP => DecompileWrap(spec),  // same layout as wrap
            TasOpcode.UP => DecompileUp(spec),
            TasOpcode.CLOCK => DecompileClock(spec),
            TasOpcode.IFDUP => DecompileIfdup(spec),
            TasOpcode.IFNA => DecompileIfna(spec),
            TasOpcode.AUTOINC => "autoinc",
            TasOpcode.AUTOENTER => "autoenter",
            TasOpcode.AUTODEC => "autodec",
            TasOpcode.KBDUP => DecompileOneParam(spec, "kbdup"),
            TasOpcode.GETLBL => DecompileGetlbl(spec),
            TasOpcode.CDPATH => DecompileOneParam(spec, "cdpath"),
            TasOpcode.SOUND => DecompileSound(spec),
            TasOpcode.TRACE => DecompileTrace(spec),
            TasOpcode.REMOUNT => DecompileOneParam(spec, "remount"),
            TasOpcode.CHAINR => DecompileOneParam(spec, "chainr"),
            TasOpcode.CLSPF => "clspf",
            TasOpcode.OPNO => DecompileOpno(spec),
            TasOpcode.RDREC => DecompileRdwrt(spec, "rdrec"),
            TasOpcode.WTREC => DecompileRdwrt(spec, "wtrec"),
            TasOpcode.WCOLOR => DecompileWcolor(spec),
            TasOpcode.EQU_MID => DecompileEquMid(spec),
            TasOpcode.EQU_DAY => DecompileEquDay(spec),
            TasOpcode.EQU_XMT => DecompileEquXmt(spec),
            TasOpcode.MOUSE => DecompileMouse(spec),
            TasOpcode.ROW_COLOR => DecompileRowColor(spec),
            TasOpcode.HOT_SPOT => DecompileHotSpot(spec),
            TasOpcode.SAVES3 => DecompileSaves(spec),
            TasOpcode.REDSP3 => DecompileRedsp(spec),
            TasOpcode.LIST_EXIT => "list_exit",
            TasOpcode.BRACE_OPEN => "{",
            TasOpcode.BRACE_CLOSE => "}",
            TasOpcode.ELSE_IF => DecompileIf(spec),  // same format as IF
            0xFE00 => "", // end-of-program sentinel — skip
            _ => $"; UNK_{instr.CommandNumber} (spec={instr.SpecLineSize} bytes)"
        };
    }

    private string Param(byte[] spec, int offset) => _spec.ResolveSpecParam(spec, offset);

    /// <summary>
    /// Safe param: only resolve if the type byte is a known valid param type.
    /// Returns empty string for unknown/garbage type bytes instead of ?0x refs.
    /// </summary>
    private string SafeParam(byte[] spec, int offset)
    {
        if (offset + 5 > spec.Length) return "";
        char type = (char)spec[offset];
        if (type == '\0' || type == ' ') return "";
        if (!_validParamTypes.Contains(type)) return "";
        return _spec.ResolveSpecParam(spec, offset);
    }
    private string Loc(byte[] spec, int offset) => LabelName(_spec.ReadInt32(spec, offset));

    private string LabelName(int labelNum)
    {
        if (labelNum >= 0 && labelNum < _run.LabelOffsets.Count)
            return $"LABEL_{labelNum}";
        return $"LABEL?{labelNum}";
    }

    private string LabelFromByteOffset(int byteOffset)
    {
        int instrIdx = byteOffset / _instrSize;
        if (_labelsByInstr.TryGetValue(instrIdx, out var labels))
            return $"LABEL_{labels[0]}";
        return $"@instr_{instrIdx}";
    }

    // --- Command decompilers ---

    private string DecompileAssign(byte[] spec)
    {
        if (spec.Length < 10) return "; = (truncated)";
        string target = Param(spec, 0);
        string value = Param(spec, 5);
        return $"{target} = {value}";
    }

    private string DecompileIf(byte[] spec)
    {
        if (spec.Length < 16) return "; if (truncated)";
        // offset 0: goto address (4 bytes) for false branch
        // offset 10: boolean expression (5 bytes)
        // offset 15: IF variant type
        string expr = Param(spec, 10);
        byte variant = _spec.ReadByte(spec, 15);
        char variantChar = (char)variant;

        return variantChar switch
        {
            'D' => $"if {expr} do",
            'T' => $"if {expr} then",
            'G' => $"if {expr} goto {Loc(spec, 16)}",
            'S' => $"if {expr} gosub {Loc(spec, 16)}",
            'R' => $"if {expr} reent",
            'E' => $"if {expr} ret",
            _ => $"if {expr}"
        };
    }

    private string DecompileElse(byte[] spec)
    {
        // ELSE is also used as absolute jump
        return "else";
    }

    private string DecompileEndw(byte[] spec, int instrIndex)
    {
        // Opcode 69 (ENDW) is a generic "end block + unconditional jump".
        // If the next instruction is ELSE or ELSE_IF, this ENDW is just the
        // "skip else" jump — don't emit it and don't pop the block stack.
        int nextInstr = instrIndex + 1;
        if (nextInstr < _run.Instructions.Count)
        {
            ushort nextCmd = _run.Instructions[nextInstr].CommandNumber;
            if (nextCmd == TasOpcode.ELSE || nextCmd == TasOpcode.ELSE_IF)
                return ""; // invisible "skip else" jump
        }

        // Use the block stack to determine what it closes.
        if (_blockStack.Count > 0)
        {
            string block = _blockStack.Pop();
            return block switch
            {
                "while" => "endw",
                "for" => "next",
                "scan" => "endscan",
                _ => "endif"  // "if" or anything else
            };
        }
        // Fallback if stack is empty: use jump direction heuristic
        if (spec.Length >= 4)
        {
            int jumpTarget = _spec.ReadInt32(spec, 0);
            int targetInstr = _instrSize > 0 ? jumpTarget / _instrSize : jumpTarget;
            if (targetInstr <= instrIndex && targetInstr >= 0 && targetInstr < _run.Instructions.Count)
            {
                var targetCmd = _run.Instructions[targetInstr].CommandNumber;
                if (targetCmd == TasOpcode.FOR) return "next";
                return "endw";
            }
        }
        return "endif";
    }


    private string DecompileGoto(byte[] spec)
    {
        if (spec.Length < 4) return "; goto (truncated)";
        int labelNum = _spec.ReadInt32(spec, 0);
        return $"goto {LabelName(labelNum)}";
    }

    private string DecompileGosub(byte[] spec)
    {
        if (spec.Length < 4) return "; gosub (truncated)";
        int labelNum = _spec.ReadInt32(spec, 0);
        return $"gosub {LabelName(labelNum)}";
    }

    private string DecompileRet(byte[] spec)
    {
        if (spec.Length >= 5)
        {
            string retVal = Param(spec, 0);
            if (!string.IsNullOrEmpty(retVal))
                return $"ret {retVal}";
        }
        return "ret";
    }

    private string DecompileSay(byte[] spec)
    {
        // SAY spec layout (25 bytes) from specline.pas:
        // [0] fld (5B), [5] col (5B), [10] row (5B), [15] color (5B), [20] pict (5B)
        if (spec.Length < 15) return DecompileGenericParams(spec, "say");
        string fld = Param(spec, 0);
        string col = Param(spec, 5);
        string row = Param(spec, 10);
        var sb = new StringBuilder($"say {row},{col},{fld}");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        AppendParam(15, "color");
        AppendParam(20, "pict");
        return sb.ToString();
    }

    private string DecompilePmsg(byte[] spec)
    {
        // PRTMSG spec layout from specline.pas:
        // [0] col (5B), [5] row (5B), [10] msg (5B),
        // [15] wait (1B Y/N), [16] ncr (1B Y/N), [17] ent (5B),
        // [22] whr (1B), [23] color (5B), [28] abs (1B Y/N)
        if (spec.Length < 15) return DecompileGenericParams(spec, "?");
        string col = Param(spec, 0);
        string row = Param(spec, 5);
        string msg = Param(spec, 10);
        var sb = new StringBuilder($"? {row},{col},{msg}");
        if (spec.Length > 15 && spec[15] == 'Y') sb.Append(" wait");
        if (spec.Length > 16 && spec[16] == 'Y') sb.Append(" ncr");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        AppendParam(17, "ent");
        AppendParam(23, "color");
        if (spec.Length > 28 && spec[28] == 'Y') sb.Append(" abs");
        return sb.ToString();
    }

    private string DecompileMsg(byte[] spec)
    {
        // MSG spec layout from specline.pas:
        // [0] fld (5B), [5] no_wait (1B Y/N)
        if (spec.Length < 5) return "; msg (truncated)";
        string msg = Param(spec, 0);
        var sb = new StringBuilder($"msg {msg}");
        if (spec.Length > 5 && spec[5] == 'Y') sb.Append(" nowait");
        return sb.ToString();
    }

    private string DecompileIncDec(byte[] spec, string cmd)
    {
        if (spec.Length < 5) return $"; {cmd} (truncated)";
        string fld = Param(spec, 0);
        return $"{cmd} {fld}";
    }

    private string DecompileWindow(byte[] spec, string cmd)
    {
        // WINDOW spec layout (81 bytes) from specline.pas:
        // [0] col (5B), [5] row (5B), [10] len (5B), [15] wdt (5B),
        // [20] wcolor (5B), [25] ttl (5B), [30] ttlw (5B),
        // [35] box (5B), [40] bcolor (5B), [45] shd (5B), [50] scolor (5B),
        // [55] icolor (5B), [60] savef (5B),
        // [65] sve_fld_sze (4B at 65-68), [69] wtext (5B), [74] btext (5B),
        // [79] dco (1B), [80] active (1B Y/N)
        if (spec.Length < 20) return DecompileGenericParams(spec, cmd);

        var sb = new StringBuilder($"{cmd}");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }

        AppendParam(0, "col");
        AppendParam(5, "row");
        AppendParam(10, "len");
        AppendParam(15, "wdt");
        AppendParam(20, "wcolor");
        AppendParam(25, "ttl");
        AppendParam(30, "ttlw");
        AppendParam(35, "box");
        AppendParam(40, "bcolor");
        AppendParam(45, "shd");
        AppendParam(50, "scolor");
        AppendParam(55, "icolor");
        AppendParam(60, "savef");
        AppendParam(69, "wtext");
        AppendParam(74, "btext");
        AppendFlag(80, "active");

        return sb.ToString();
    }

    private string DecompileButton(byte[] spec)
    {
        // BUTTON spec layout (58 bytes) from specline.pas:
        // [0] col (5B), [5] row (5B), [10] len (5B), [15] wdt (5B), [20] color (5B),
        // [25] caption (5B), [30] txtcolor (5B), [35] key (5B),
        // [40] remove (1B Y/N), [41] save_to (5B), [46] restore_from (5B),
        // [51] off (1B Y/N), [52] on (1B Y/N), [53] using (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "button");

        var sb = new StringBuilder("button");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }

        AppendParam(0, "col");
        AppendParam(5, "row");
        AppendParam(10, "len");
        AppendParam(15, "wdt");
        AppendParam(20, "color");
        AppendParam(25, "caption");
        AppendParam(30, "txtcolor");
        AppendParam(35, "key");
        AppendFlag(40, "remove");
        AppendParam(41, "save_to");
        AppendParam(46, "restore_from");
        AppendFlag(51, "off");
        AppendFlag(52, "on");
        AppendParam(53, "using");

        return sb.ToString();
    }

    private string DecompileOpenv(byte[] spec)
    {
        // OPENV/OPEN spec layout (54 bytes) from specline.pas:
        // [0]  filename (5B), [5] ext/cc (5B), [10] lock type (1B char NRFAXDO),
        // [11] err_label (4B), [15] owner (5B), [20] path (5B), [25] fd/schema (5B),
        // [30] file_type (1B char TXDFBC), [31] fnum handle (5B), [36] rec_size (5B),
        // [41] buffer_name (5B), [46] create_flg (1B Y/N), [47] noclr_flg (1B Y/N),
        // [48] update_udf (5B), [53] addallflds (1B Y/N)
        if (spec.Length < 10) return DecompileGenericParams(spec, "openv");

        var sb = new StringBuilder();
        sb.Append($"openv {Param(spec, 0)}");

        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }
        void AppendChar(int off, string kw, char skip) { if (spec.Length > off && spec[off] != 0 && spec[off] != skip) sb.Append($" {kw} {(char)spec[off]}"); }

        AppendParam(5, "ext");
        AppendChar(10, "lock", 'N');
        if (spec.Length >= 15) { int errLbl = _spec.ReadInt32(spec, 11); if (errLbl != 0) sb.Append($" err {LabelName(errLbl)}"); }
        AppendParam(15, "owner");
        AppendParam(20, "path");
        AppendParam(25, "fd");
        AppendChar(30, "type", 'T');
        AppendParam(31, "fnum");
        AppendParam(36, "size");
        AppendParam(41, "buf");
        AppendFlag(46, "create");
        AppendFlag(47, "noclr");
        AppendParam(48, "update");
        AppendFlag(53, "addallflds");

        return sb.ToString();
    }

    private string DecompileFindv(byte[] spec)
    {
        // FINDV/FIND spec layout (33 bytes) from specline.pas:
        // [0] handle (5B), [5] key (5B), [10] val (5B),
        // [15] find_type (1B char MGFLNPRES), [16] err_label (4B),
        // [20] nlock (1B Y/N), [21] key_only (1B Y/N),
        // [22] for_expr (5B), [27] while_expr (5B), [32] noclr (1B Y/N)
        if (spec.Length < 16) return DecompileGenericParams(spec, "findv");

        string handle = Param(spec, 0);
        string key = Param(spec, 5);
        string val = Param(spec, 10);
        byte findType = _spec.ReadByte(spec, 15);
        char ft = findType >= 0x20 && findType < 0x7F ? (char)findType : 'F';

        string valStr = val;
        if (val.StartsWith("[") && val.EndsWith("]"))
            valStr = val[1..^1];

        var sb = new StringBuilder();
        sb.Append($"findv {ft} fnum {handle}");
        if (!string.IsNullOrEmpty(key)) sb.Append($" key {key}");
        if (!string.IsNullOrEmpty(valStr)) sb.Append($" val {valStr}");
        if (spec.Length >= 20) { int errLbl = _spec.ReadInt32(spec, 16); if (errLbl != 0) sb.Append($" err {LabelName(errLbl)}"); }
        if (spec.Length > 20 && spec[20] == 'Y') sb.Append(" nlock");
        if (spec.Length > 21 && spec[21] == 'Y') sb.Append(" keyo");
        if (spec.Length >= 27) { string forExpr = Param(spec, 22); if (!string.IsNullOrEmpty(forExpr)) sb.Append($" for {forExpr}"); }
        if (spec.Length >= 32) { string whileExpr = Param(spec, 27); if (!string.IsNullOrEmpty(whileExpr)) sb.Append($" while {whileExpr}"); }
        if (spec.Length > 32 && spec[32] == 'Y') sb.Append(" noclr");
        return sb.ToString();
    }

    private string DecompileClose(byte[] spec)
    {
        // CLOSE spec layout from specline.pas:
        // [0] handle (5B), [5] delete (1B Y/N)
        if (spec.Length < 5) return "close";
        string handle = Param(spec, 0);
        var sb = new StringBuilder($"close {handle}");
        if (spec.Length > 5 && spec[5] == 'Y') sb.Append(" delete");
        return sb.ToString();
    }

    private string DecompileClr(byte[] spec)
    {
        if (spec.Length < 5) return "clr";
        string handle = Param(spec, 0);
        return $"clr {handle}";
    }

    private string DecompileSave(byte[] spec)
    {
        // SAVE spec layout from specline.pas:
        // [0] handle (5B), [5] no_clear (1B Y/N), [6] no_ask (1B Y/N),
        // [7] nosave_lbl (4B), [11] err_lbl (4B), [15] unlock (1B Y/N)
        if (spec.Length < 5) return "save";
        string handle = Param(spec, 0);
        var sb = new StringBuilder($"save {handle}");
        if (spec.Length > 5 && spec[5] == 'Y') sb.Append(" noclr");
        if (spec.Length > 6 && spec[6] == 'Y') sb.Append(" noask");
        if (spec.Length >= 11) { int lbl = _spec.ReadInt32(spec, 7); if (lbl != 0) sb.Append($" nosave {LabelName(lbl)}"); }
        if (spec.Length >= 15) { int lbl = _spec.ReadInt32(spec, 11); if (lbl != 0) sb.Append($" err {LabelName(lbl)}"); }
        if (spec.Length > 15 && spec[15] == 'Y') sb.Append(" unlock");
        return sb.ToString();
    }

    private string DecompileChain(byte[] spec)
    {
        if (spec.Length < 5) return "; chain (truncated)";
        string prg = Param(spec, 0);
        return $"chain {prg}";
    }

    private string DecompileTrap(byte[] spec)
    {
        // TRAP spec layout (10 bytes) from specline.pas:
        // [0] key (5B param), [5] wtd (1B char G/S/I/D), [6] label (4B)
        if (spec.Length < 10) return "; trap (truncated)";
        string key = DecodeTrapKeys(spec, 0);
        byte wtd = _spec.ReadByte(spec, 5);
        int lbl = _spec.ReadInt32(spec, 6);
        string action = (char)wtd switch
        {
            'G' => $"goto {LabelName(lbl)}",
            'S' => $"gosub {LabelName(lbl)}",
            'I' => "ignr",
            'D' => "dflt",
            _ => lbl != 0 ? $"{(char)wtd} {LabelName(lbl)}" : $"{(char)wtd}"
        };
        return $"trap {key} {action}";
    }

    /// <summary>
    /// Decode trap key constant as comma-separated key names.
    /// The constant is a string of single-byte TAS key codes.
    /// </summary>
    private string DecodeTrapKeys(byte[] spec, int offset)
    {
        var (type, location) = _spec.ReadSpecParam(spec, offset);
        if (type != 'C') return Param(spec, offset);

        var cs = _run.ConstantSegment;
        if (location < 0 || location >= cs.Length) return Param(spec, offset);

        byte cType = cs[location];
        if (cType != (byte)'A' && cType != (byte)'a') return Param(spec, offset);

        int dispSize = location + 4 <= cs.Length
            ? BitConverter.ToUInt16(cs, location + 2)
            : 0;

        int dataStart = location + 4;
        int dataLen = Math.Min(dispSize, cs.Length - dataStart);
        if (dataLen <= 0) return Param(spec, offset);

        var names = new List<string>();
        for (int i = dataStart; i < dataStart + dataLen; i++)
        {
            byte b = cs[i];
            if (b == 0) break; // null terminator
            string name = SpecDecoder.GetTrapKeyName(b);
            names.Add(name);
        }
        return names.Count > 0 ? string.Join(",", names) : Param(spec, offset);
    }

    private string DecompileEnter(byte[] spec)
    {
        // ENTER spec layout (97 bytes) from specline.pas:
        // [0] col (5B), [5] row (5B), [10] field (5B), [15] mask (5B), [20] help (5B),
        // [25] up_label (4B), [29] valid (5B), [34] acr (1B), [35] pswd (1B), [36] upc (1B),
        // [37] color (5B), [42] pre (5B), [47] post (5B), [54] dflt (5B), [59] vmsg (5B),
        // [64] norev (1B), [65] do (5B), [70] nosave (1B), [71] return (1B), [72] noesc (1B),
        // [73] array (1B), [74] enum (5B), [79] cntr (5B), [84] auto_srch (1B),
        // [85] group (5B), [90] nc_off (1B), [91] nc_on (1B), [92] mbl (5B)
        if (spec.Length < 15) return DecompileGenericParams(spec, "enter");

        var sb = new StringBuilder();
        string fld = Param(spec, 10);
        sb.Append($"enter {fld}");

        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }

        AppendParam(15, "mask");
        AppendParam(20, "help");
        if (spec.Length >= 29) { int upLbl = _spec.ReadInt32(spec, 25); if (upLbl != 0) sb.Append($" upar {LabelName(upLbl)}"); }
        AppendParam(29, "vld");
        AppendFlag(34, "acr");
        AppendFlag(35, "pswd");
        AppendFlag(36, "upc");
        AppendParam(0, "at");
        if (spec.Length >= 10) { string row = Param(spec, 5); if (!string.IsNullOrEmpty(row)) sb.Append($",{row}"); }
        AppendParam(37, "color");
        AppendParam(42, "pre");
        AppendParam(47, "post");
        AppendParam(54, "dflt");
        AppendParam(59, "vmsg");
        AppendFlag(64, "norev");
        AppendParam(65, "do");
        AppendFlag(73, "array");
        AppendParam(74, "enum");
        AppendParam(79, "cntr");
        AppendFlag(84, "auto_srch");
        AppendParam(85, "group");
        AppendParam(92, "mbl");

        return sb.ToString();
    }

    private string DecompileAsk(byte[] spec)
    {
        // ASK spec layout (30 bytes) from specline.pas:
        // [0] message (5B), [5] default (5B), [10] caption (5B),
        // [15] use (5B), [20] leftbutton (5B), [25] rightbutton (5B)
        if (spec.Length < 5) return "ask";
        var sb = new StringBuilder($"ask {Param(spec, 0)}");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        AppendParam(5, "dflt");
        AppendParam(10, "caption");
        AppendParam(15, "use");
        AppendParam(20, "leftbutton");
        AppendParam(25, "rightbutton");
        return sb.ToString();
    }

    private string DecompileWhile(byte[] spec)
    {
        // Layout: [0-3] jump addr (4 bytes), [4-8] expression (5 bytes)
        if (spec.Length >= 9)
        {
            string expr = Param(spec, 4);
            return $"while {expr}";
        }
        return "while .t.";
    }

    private string DecompileFor(byte[] spec)
    {
        // Layout: [0-3] exit jump (4), [4-8] stop (5), [9-13] step (5), [14-18] counter (5), [19-23] start (5) [24] direction
        if (spec.Length >= 24)
        {
            string counter = Param(spec, 14);
            string start = Param(spec, 19);
            string stop = Param(spec, 4);
            string step = Param(spec, 9);
            if (!string.IsNullOrEmpty(step) && step != "1")
                return $"for {counter} = {start} to {stop} step {step}";
            return $"for {counter} = {start} to {stop}";
        }
        return "for";
    }

    private string DecompileSelect(byte[] spec)
    {
        if (spec.Length < 5) return "select";
        string selector = Param(spec, 0);
        return $"select {selector}";
    }

    private string DecompileCase(byte[] spec)
    {
        if (spec.Length < 5) return "case";
        string value = Param(spec, 0);
        return $"case {value}";
    }

    private string DecompileScan(byte[] spec)
    {
        // SCAN spec layout (43 bytes) from specline.pas:
        // [0] end_jump (4B), [4] handle (5B), [9] key (5B), [14] start (5B),
        // [19] scope (1B char), [20] sval (5B), [25] for (5B), [30] while (5B),
        // [35] display (1B Y/N), [36] no_lock (1B Y/N), [42] reverse (1B Y/N)
        if (spec.Length < 9) return "scan";

        var sb = new StringBuilder("scan");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }

        AppendParam(4, "fnum");
        AppendParam(9, "key");
        AppendParam(14, "start");
        if (spec.Length > 19 && spec[19] != 0 && spec[19] != ' ') sb.Append($" scope {(char)spec[19]}");
        AppendParam(20, "sval");
        AppendParam(25, "for");
        AppendParam(30, "while");
        AppendFlag(35, "display");
        AppendFlag(36, "nlock");
        AppendFlag(42, "reverse");

        return sb.ToString();
    }

    private string DecompileSaves(byte[] spec)
    {
        if (spec.Length < 5) return "saves";
        string handle = Param(spec, 0);
        return $"saves {handle}";
    }

    private string DecompileRedsp(byte[] spec)
    {
        if (spec.Length < 5) return "redsp";
        string handle = Param(spec, 0);
        return $"redsp {handle}";
    }

    private string DecompileRopen(byte[] spec)
    {
        if (spec.Length < 5) return "ropen";
        string handle = Param(spec, 0);
        return $"ropen {handle}";
    }

    private string DecompileColor(byte[] spec)
    {
        if (spec.Length < 5) return "color";
        string val = Param(spec, 0);
        return $"color {val}";
    }

    private string DecompileFill(byte[] spec)
    {
        // FILL spec layout from specline.pas:
        // [0] fld (5B), [5] chr (5B), [10] times (5B), [15] where (1B)
        if (spec.Length < 10) return "; fill (truncated)";
        string target = Param(spec, 0);
        string value = Param(spec, 5);
        var sb = new StringBuilder($"fill {target} {value}");
        if (spec.Length >= 15) { string times = Param(spec, 10); if (!string.IsNullOrEmpty(times)) sb.Append($" {times}"); }
        return sb.ToString();
    }

    private string DecompileDispf(byte[] spec)
    {
        // DISPF uses SAY spec layout from specline.pas:
        // [0] fld (5B), [5] col (5B), [10] row (5B), [15] color (5B), [20] pict (5B)
        if (spec.Length < 15) return "; dispf (truncated)";
        string fld = Param(spec, 0);
        string col = Param(spec, 5);
        string row = Param(spec, 10);
        var sb = new StringBuilder($"dispf {row},{col},{fld}");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        AppendParam(15, "color");
        AppendParam(20, "pict");
        return sb.ToString();
    }

    private string DecompileCursor(byte[] spec)
    {
        // CURSOR spec layout from specline.pas:
        // [0] start (5B), [5] stop (5B), [10] on_off (1B), [11] wait (1B), [12] dflt (1B)
        if (spec.Length < 5) return "cursor";
        var sb = new StringBuilder("cursor");
        string start = Param(spec, 0);
        if (!string.IsNullOrEmpty(start)) sb.Append($" {start}");
        if (spec.Length >= 10) { string stop = Param(spec, 5); if (!string.IsNullOrEmpty(stop)) sb.Append($" {stop}"); }
        if (spec.Length > 10 && spec[10] != 0) sb.Append($" {(char)spec[10]}");
        if (spec.Length > 11 && spec[11] == 'Y') sb.Append(" wait");
        if (spec.Length > 12 && spec[12] == 'Y') sb.Append(" dflt");
        return sb.ToString();
    }

    private string DecompileClrlne(byte[] spec)
    {
        // CLRLNE spec layout from specline.pas:
        // [0] col (5B), [5] row (5B), [10] chrs (5B),
        // [15] no_color (1B Y/N), [16] color (5B), [21] abs (1B Y/N)
        if (spec.Length < 10) return "clrlne";
        string col = Param(spec, 0);
        string row = Param(spec, 5);
        var sb = new StringBuilder($"clrlne {row},{col}");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        AppendParam(10, "chrs");
        if (spec.Length > 15 && spec[15] == 'Y') sb.Append(" nocolor");
        AppendParam(16, "color");
        if (spec.Length > 21 && spec[21] == 'Y') sb.Append(" abs");
        return sb.ToString();
    }

    private string DecompilePusht(byte[] spec)
    {
        if (spec.Length < 5) return "pusht";
        string name = Param(spec, 0);
        return $"pusht {name}";
    }

    private string DecompilePopt(byte[] spec)
    {
        if (spec.Length < 5) return "popt";
        string name = Param(spec, 0);
        return $"popt {name}";
    }

    private string DecompilePushPop(byte[] spec, string cmd)
    {
        if (spec.Length < 5) return cmd;
        string fld = Param(spec, 0);
        return $"{cmd} {fld}";
    }

    private string DecompileFormat(byte[] spec)
    {
        // FORMAT spec layout from specline.pas:
        // [0] fld (5B), [5] recv (5B), [10] commas (1B), [11] flt_dol (1B),
        // [12] neg_how (1B), [13] off (1B), [14] pict (5B), [19] no_zeros (1B)
        if (spec.Length < 10) return "; format (truncated)";
        string fld = Param(spec, 0);
        string recv = Param(spec, 5);
        var sb = new StringBuilder($"format {fld}");
        if (!string.IsNullOrEmpty(recv)) sb.Append($" {recv}");
        if (spec.Length > 10 && spec[10] == 'Y') sb.Append(" commas");
        if (spec.Length > 11 && spec[11] == 'Y') sb.Append(" flt_dol");
        if (spec.Length > 13 && spec[13] == 'Y') sb.Append(" off");
        if (spec.Length >= 19) { string pict = Param(spec, 14); if (!string.IsNullOrEmpty(pict)) sb.Append($" pict {pict}"); }
        if (spec.Length > 19 && spec[19] == 'Y') sb.Append(" nozero");
        return sb.ToString();
    }

    private string DecompileRap(byte[] spec)
    {
        // RAP spec layout from specline.pas:
        // [0] name (5B), [5] num (5B), [10] in_mem (1B Y/N),
        // [11] with (5B), [16] no_base_wind (1B Y/N),
        // [17] new_runtime (1B Y/N), [18] no_delete (1B Y/N), [19] no_save (1B Y/N)
        if (spec.Length < 5) return "; rap (truncated)";
        var sb = new StringBuilder($"rap {Param(spec, 0)}");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }
        AppendParam(5, "num");
        AppendFlag(10, "in_mem");
        AppendParam(11, "with");
        AppendFlag(16, "no_base_wind");
        AppendFlag(17, "new_runtime");
        AppendFlag(18, "no_delete");
        AppendFlag(19, "no_save");
        return sb.ToString();
    }

    private string DecompileUdfc(byte[] spec, RunBytecodeInstruction instr, string keyword)
    {
        // UDC/FUNC/CMD spec layout from specline.pas:
        // [0] label (4B), [4] flist (5B)
        // In TAS 5.1, UDC with SLSize=0: label is encoded via the spec pointer mechanism
        // differently — the SLPtr byte offset into spec is the label byte offset
        if (spec.Length >= 4)
        {
            int labelNum = _spec.ReadInt32(spec, 0);
            var sb = new StringBuilder($"{keyword} {LabelName(labelNum)}");
            if (spec.Length >= 9) { string flist = SafeParam(spec, 4); if (!string.IsNullOrEmpty(flist)) sb.Append($" [{flist}]"); }
            return sb.ToString();
        }
        // SLSize=0: SLPtr might be unused or encode label differently
        // For now, try treating SLPtr as the label number
        int label = instr.SpecLinePtr;
        return label > 0 ? $"{keyword} {LabelName(label)}" : keyword;
    }

    private string DecompileRun(byte[] spec)
    {
        // RUN spec layout from specline.pas:
        // [0] name (5B), [5] tail (5B)
        if (spec.Length < 5) return "; run (truncated)";
        string prg = Param(spec, 0);
        var sb = new StringBuilder($"run {prg}");
        if (spec.Length >= 10) { string tail = Param(spec, 5); if (!string.IsNullOrEmpty(tail)) sb.Append($" {tail}"); }
        return sb.ToString();
    }

    private string DecompileExec(byte[] spec)
    {
        if (spec.Length < 5) return "; exec (truncated)";
        string prg = Param(spec, 0);
        return $"exec {prg}";
    }

    /// <summary>
    /// Decompile a conditional loop/exit (LOOP_IF, EXIT_IF, FLOOP_IF, FEXIT_IF, SLOOP_IF, SEXIT_IF).
    /// Layout: [0-3] jump address (4 bytes), [4-8] expression (5 bytes).
    /// </summary>
    private string DecompileCondBreak(byte[] spec, string cmd)
    {
        if (spec.Length >= 9)
        {
            string expr = Param(spec, 4);
            return $"{cmd} {expr}";
        }
        return cmd;
    }

    /// <summary>
    /// Decompile a command with one parameter.
    /// </summary>
    private string DecompileOneParam(byte[] spec, string cmd)
    {
        if (spec.Length >= 5)
        {
            string p = Param(spec, 0);
            if (!string.IsNullOrEmpty(p))
                return $"{cmd} {p}";
        }
        return cmd;
    }

    /// <summary>
    /// Decompile MID command: target = MID(source, start, length).
    /// </summary>
    private string DecompileMid(byte[] spec)
    {
        if (spec.Length < 10) return "mid";
        string target = Param(spec, 0);
        string source = Param(spec, 5);
        if (spec.Length >= 20)
        {
            string start = Param(spec, 10);
            string len = Param(spec, 15);
            return $"mid {target} = {source},{start},{len}";
        }
        if (spec.Length >= 15)
        {
            string start = Param(spec, 10);
            return $"mid {target} = {source},{start}";
        }
        return $"mid {target} = {source}";
    }

    /// <summary>
    /// Decompile XFER: transfer data between buffers/fields.
    /// </summary>
    private string DecompileXfer(byte[] spec)
    {
        // XFER spec layout from specline.pas:
        // [0] from_fld (5B), [5] to_fld (5B), [10] numchr (5B),
        // [15] fmem (5B), [20] tmem (5B), [25] rec_buff (1B)
        if (spec.Length < 10) return DecompileOneParam(spec, "xfer");
        string from = Param(spec, 0);
        string to = Param(spec, 5);
        var sb = new StringBuilder($"xfer {from} to {to}");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        AppendParam(10, "numchr");
        AppendParam(15, "fmem");
        AppendParam(20, "tmem");
        return sb.ToString();
    }

    /// <summary>
    /// Decompile old-style MENU (menuo layout from specline.pas, 89 bytes).
    /// </summary>
    private string DecompileMenuOld(byte[] spec)
    {
        // MENUO spec layout from specline.pas:
        // [0] col (5B), [5] row (5B), [10] lt (5B), [15] wdt (5B),
        // [20] wclr (5B), [25] ttlfld (5B), [30] ttlloc (5B),
        // [35] box (5B), [40] bclr (5B), [45] shd (5B), [50] sclr (5B),
        // [55] cpc (5B), [60] afld (5B), [65] cntr (5B), [70] nchcs (5B),
        // [75] mcwdt (5B), [80] esc_lbl (4B), [84] hlp_lbl (4B), [88] hold (1B Y/N)
        if (spec.Length < 10) return DecompileGenericParams(spec, "menu");

        var sb = new StringBuilder("menu");
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }

        AppendParam(0, "col");
        AppendParam(5, "row");
        AppendParam(10, "len");
        AppendParam(15, "wdt");
        AppendParam(20, "wcolor");
        AppendParam(25, "ttl");
        AppendParam(30, "ttlw");
        AppendParam(35, "box");
        AppendParam(40, "bcolor");
        AppendParam(45, "shd");
        AppendParam(50, "scolor");
        AppendParam(55, "cpc");
        AppendParam(60, "afld");
        AppendParam(65, "cntr");
        AppendParam(70, "nchcs");
        AppendParam(75, "mcwdt");
        if (spec.Length >= 84) { int lbl = _spec.ReadInt32(spec, 80); if (lbl != 0) sb.Append($" esc {LabelName(lbl)}"); }
        if (spec.Length >= 88) { int lbl = _spec.ReadInt32(spec, 84); if (lbl != 0) sb.Append($" hlp {LabelName(lbl)}"); }
        AppendFlag(88, "hold");

        return sb.ToString();
    }

    /// <summary>
    /// Decompile NMENU — new-style menu with mixed 5-byte params and 1-byte flags.
    /// </summary>
    private string DecompileMenu(byte[] spec, string cmd)
    {
        // MENU/NMENU spec layout (139 bytes) from specline.pas:
        // [0] flds (5B), [5] col (5B), [10] row (5B), [15] msg (5B), [20] width (5B),
        // [25] chcs (5B), [30] chose (5B), [35] help (5B), [40] ttl_w (5B), [45] ttlfld (5B),
        // [50] cntr (5B), [55] box (5B), [60] shad_w (5B),
        // [65] hold (1B Y/N), [66] array (1B Y/N),
        // [67] mcolor (5B), [72] ccolor (5B), [77] scolor (5B),
        // [82] larrow_lbl (4B), [86] rarrow_lbl (4B),
        // [90] length (5B), [95] bcolor (5B),
        // [100] auto (1B Y/N), [101] nowait (1B Y/N),
        // [102] on_mve (5B), [107] use_traps (1B Y/N),
        // [108] mtext (5B), [113] ctext (5B), [118] btext (5B),
        // [123] dco (1B), [124] retval (5B), [129] sub_num (5B), [134] no_esc (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, cmd);

        var sb = new StringBuilder(cmd);
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }

        AppendParam(0, "flds");
        AppendParam(5, "col");
        AppendParam(10, "row");
        AppendParam(15, "msg");
        AppendParam(20, "width");
        AppendParam(25, "chcs");
        AppendParam(30, "chose");
        AppendParam(35, "help");
        AppendParam(40, "ttl_w");
        AppendParam(45, "ttl");
        AppendParam(50, "cntr");
        AppendParam(55, "box");
        AppendParam(60, "shad_w");
        AppendFlag(65, "hold");
        AppendFlag(66, "array");
        AppendParam(67, "mcolor");
        AppendParam(72, "ccolor");
        AppendParam(77, "scolor");
        if (spec.Length >= 86) { int lbl = _spec.ReadInt32(spec, 82); if (lbl != 0) sb.Append($" larrow {LabelName(lbl)}"); }
        if (spec.Length >= 90) { int lbl = _spec.ReadInt32(spec, 86); if (lbl != 0) sb.Append($" rarrow {LabelName(lbl)}"); }
        AppendParam(90, "length");
        AppendParam(95, "bcolor");
        AppendFlag(100, "auto");
        AppendFlag(101, "nowait");
        AppendParam(102, "on_mve");
        AppendFlag(107, "use_traps");
        AppendParam(108, "mtext");
        AppendParam(113, "ctext");
        AppendParam(118, "btext");
        AppendParam(124, "retval");
        AppendParam(129, "sub_num");
        AppendParam(134, "no_esc");

        return sb.ToString();
    }

    /// <summary>
    /// Decompile LISTF/LISTM — complex layout with mixed 5-byte params and 1-byte flags.
    /// </summary>
    private string DecompileListf(byte[] spec, string cmd)
    {
        // LISTF/LISTM spec layout (126 bytes) from specline.pas:
        // [0] lst (5B), [5] cntr (5B), [10] actv (5B), [15] chse (5B),
        // [20] rnd (1B Y/N), [21] menu (1B Y/N),
        // [22] oth (5B), [27] srch (5B), [32] fhndl (5B), [37] fkey (5B),
        // [42] for (5B), [47] while (5B), [52] start (5B),
        // [57] no_wait (1B Y/N), [58] enter (5B), [63] no_add (1B Y/N),
        // [64] lnes (5B), [69] hlp (5B), [74] cc (5B),
        // [79] up (1B Y/N), [80] on_mve (5B), [85] styp (5B),
        // [90] list_end (1B Y/N), [91] fline (5B), [96] cbf (5B),
        // [101] blnes (5B), [106] noshift (1B Y/N), [107] ec (5B),
        // [112] use_traps (1B Y/N), [113] insrt_at_end (1B Y/N), [114] touch_scrn (1B Y/N),
        // [115] cctext (5B), [120] ectext (5B), [125] dco (1B)
        if (spec.Length < 10) return DecompileGenericParams(spec, cmd);

        var sb = new StringBuilder(cmd);
        void AppendParam(int off, string kw) { if (spec.Length >= off + 5) { string v = SafeParam(spec, off); if (!string.IsNullOrEmpty(v)) sb.Append($" {kw} {v}"); } }
        void AppendFlag(int off, string kw) { if (spec.Length > off && spec[off] == 'Y') sb.Append($" {kw}"); }

        AppendParam(0, "lst");
        AppendParam(5, "cntr");
        AppendParam(10, "actv");
        AppendParam(15, "chse");
        AppendFlag(20, "rnd");
        AppendFlag(21, "menu");
        AppendParam(22, "oth");
        AppendParam(27, "srch");
        AppendParam(32, "fhndl");
        AppendParam(37, "fkey");
        AppendParam(42, "for");
        AppendParam(47, "while");
        AppendParam(52, "start");
        AppendFlag(57, "nowait");
        AppendParam(58, "enter");
        AppendFlag(63, "noadd");
        AppendParam(64, "lnes");
        AppendParam(69, "hlp");
        AppendParam(74, "cc");
        AppendFlag(79, "up");
        AppendParam(80, "on_mve");
        AppendParam(85, "styp");
        AppendFlag(90, "list_end");
        AppendParam(91, "fline");
        AppendParam(96, "cbf");
        AppendParam(101, "blnes");
        AppendFlag(106, "noshift");
        AppendParam(107, "ec");
        AppendFlag(112, "use_traps");
        AppendFlag(113, "insrt_at_end");
        AppendFlag(114, "touch_scrn");
        AppendParam(115, "cctext");
        AppendParam(120, "ectext");

        return sb.ToString();
    }

    // --- Additional command decompilers using specline.pas layouts ---

    private string DecompileXtrap(byte[] spec)
    {
        // TRAP_OPS: [0] wtd (1B char), [1] fld (5B), [6] cvals (varies)
        if (spec.Length < 6) return "xtrap";
        char wtd = (char)_spec.ReadByte(spec, 0);
        string fld = SafeParam(spec, 1);
        return $"xtrap {wtd} {fld}";
    }

    private string DecompileUpdta(byte[] spec)
    {
        // update_array: [0] all (5B), [5] wtd (1B), [6] times (5B), [11] val (5B), [16] fn (5B)
        if (spec.Length < 5) return "updta";
        var sb = new StringBuilder($"updta {SafeParam(spec, 0)}");
        if (spec.Length > 5 && spec[5] != 0) sb.Append($" {(char)spec[5]}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(6, "times");
        A(11, "val");
        A(16, "fn");
        return sb.ToString();
    }

    private string DecompileMount(byte[] spec)
    {
        // mount: [0] fmt (5B), [5] typ (1B), [6] wtp (1B), [7] rfile (5B), [12] sve_to (5B), [17] winform (1B)
        if (spec.Length < 5) return "mount";
        var sb = new StringBuilder($"mount {SafeParam(spec, 0)}");
        if (spec.Length > 5 && spec[5] != 0) sb.Append($" type {(char)spec[5]}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(7, "rfile");
        A(12, "sve_to");
        return sb.ToString();
    }

    private string DecompileSorta(byte[] spec)
    {
        // sort: [0] fld (5B), [5] num (5B), [10] move (5B), [15] cntr (5B), [20] way (1B)
        if (spec.Length < 5) return "sorta";
        var sb = new StringBuilder($"sorta {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "num");
        A(10, "move");
        A(15, "cntr");
        if (spec.Length > 20 && spec[20] != 0) sb.Append($" way {(char)spec[20]}");
        return sb.ToString();
    }

    private string DecompileRda(byte[] spec)
    {
        // rd_array: [0] frm (5B), [5] to (5B), [10] hndl (5B), [15] key (5B),
        // [20] strt (5B), [25] scope (1B), [26] sval (5B), [31] for (5B),
        // [36] while (5B), [41] cntr (5B), [46] display (1B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "rda");
        var sb = new StringBuilder($"rda {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "to");
        A(10, "hndl");
        A(15, "key");
        A(20, "strt");
        if (spec.Length > 25 && spec[25] != 0 && spec[25] != ' ') sb.Append($" scope {(char)spec[25]}");
        A(26, "sval");
        A(31, "for");
        A(36, "while");
        A(41, "cntr");
        if (spec.Length > 46 && spec[46] == 'Y') sb.Append(" display");
        return sb.ToString();
    }

    private string DecompileWrta(byte[] spec)
    {
        // wrt_array: [0] frm (5B), [5] to (5B), [10] scope (1B), [11] sval (5B),
        // [16] for (5B), [21] hndl (5B), [26] rec (5B), [31] display (1B), [32] cntr (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "wrta");
        var sb = new StringBuilder($"wrta {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "to");
        if (spec.Length > 10 && spec[10] != 0 && spec[10] != ' ') sb.Append($" scope {(char)spec[10]}");
        A(11, "sval");
        A(16, "for");
        A(21, "hndl");
        A(26, "rec");
        if (spec.Length > 31 && spec[31] == 'Y') sb.Append(" display");
        A(32, "cntr");
        return sb.ToString();
    }

    private string DecompileScroll(byte[] spec)
    {
        // scroll: [0] col (5B), [5] row (5B), [10] ht (5B), [15] wdt (5B),
        // [20] lnes (5B), [25] direction (1B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "scroll");
        var sb = new StringBuilder("scroll");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "col");
        A(5, "row");
        A(10, "ht");
        A(15, "wdt");
        A(20, "lnes");
        if (spec.Length > 25 && spec[25] != 0) sb.Append($" dir {(char)spec[25]}");
        return sb.ToString();
    }

    private string DecompilePbox(byte[] spec)
    {
        // prt_box: [0] col (5B), [5] row (5B), [10] lnth (5B), [15] wdt (5B),
        // [20] lines (1B), [21] clr_set (1B), [22] color (5B), [27] brdr (5B), [32] abs (1B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "pbox");
        var sb = new StringBuilder("pbox");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "col");
        A(5, "row");
        A(10, "lnth");
        A(15, "wdt");
        A(22, "color");
        A(27, "brdr");
        if (spec.Length > 32 && spec[32] == 'Y') sb.Append(" abs");
        return sb.ToString();
    }

    private string DecompilePchr(byte[] spec)
    {
        // prt_chr$: [0] ptw (1B), [1] num (5B), [6] fld (5B)
        if (spec.Length < 6) return "pchr";
        var sb = new StringBuilder("pchr");
        string num = SafeParam(spec, 1);
        string fld = spec.Length >= 11 ? SafeParam(spec, 6) : "";
        if (!string.IsNullOrEmpty(num)) sb.Append($" {num}");
        if (!string.IsNullOrEmpty(fld)) sb.Append($" {fld}");
        return sb.ToString();
    }

    private string DecompilePfmt(byte[] spec)
    {
        // prtfmt: [0] col (5B), [5] row (5B), [10] wait (1B), [11] ncr (1B),
        // [12] line (5B), [17] whr (1B), [18] thru (5B), [23] abs (1B), [24] bks (1B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "pfmt");
        var sb = new StringBuilder("pfmt");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "col");
        A(5, "row");
        if (spec.Length > 10 && spec[10] == 'Y') sb.Append(" wait");
        if (spec.Length > 11 && spec[11] == 'Y') sb.Append(" ncr");
        A(12, "line");
        A(18, "thru");
        if (spec.Length > 23 && spec[23] == 'Y') sb.Append(" abs");
        return sb.ToString();
    }

    private string DecompileOn(byte[] spec)
    {
        // on: [0] val (5B), [5] tosub (1B), [6] num_labels (1B), [7+] labels (4B each)
        if (spec.Length < 7) return DecompileGenericParams(spec, "on");
        string val = SafeParam(spec, 0);
        char tosub = (char)_spec.ReadByte(spec, 5);
        byte numLabels = _spec.ReadByte(spec, 6);
        string cmd = tosub == 'S' ? "on_gosub" : "on_goto";
        var sb = new StringBuilder($"on {val} {cmd}");
        for (int i = 0; i < numLabels && 7 + i * 4 + 4 <= spec.Length; i++)
        {
            int lbl = _spec.ReadInt32(spec, 7 + i * 4);
            sb.Append($" {LabelName(lbl)}");
        }
        return sb.ToString();
    }

    private string DecompileRedef(byte[] spec)
    {
        // redefine: [0] fld (5B), [5] ftyp (5B), [10] fsze (5B), [15] dchr (5B),
        // [20] fnum (5B), [25] ofst (5B), [30] knum (5B), [35] pict (5B),
        // [40] loc (5B), [45] up (5B)
        if (spec.Length < 5) return "redef";
        var sb = new StringBuilder($"redef {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "ftyp");
        A(10, "fsze");
        A(15, "dchr");
        A(20, "fnum");
        A(25, "ofst");
        A(30, "knum");
        A(35, "pict");
        A(40, "loc");
        A(45, "up");
        return sb.ToString();
    }

    private string DecompileAdd(byte[] spec)
    {
        // add_fld: [0] name (5B), [5] ftyp (5B), [10] fsze (5B), [15] fdec (5B),
        // [20] fupc (5B), [25] fmask (5B), [30] fhndl (5B), [35] fofst (5B),
        // [40] ffptr (5B), [45] fary (5B), [50] fknum (5B)
        if (spec.Length < 5) return "add";
        var sb = new StringBuilder($"add {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "type");
        A(10, "size");
        A(15, "dec");
        A(20, "upc");
        A(25, "mask");
        A(30, "hndl");
        A(35, "ofst");
        A(40, "fptr");
        A(45, "ary");
        A(50, "knum");
        return sb.ToString();
    }

    private string DecompileRel(byte[] spec)
    {
        // relate: [0] slv (5B), [5] slv_key (5B), [10] mstr_fle (5B), [15] mstr_fld (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "rel");
        var sb = new StringBuilder("rel");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "slv");
        A(5, "key");
        A(10, "mstr");
        A(15, "mfld");
        return sb.ToString();
    }

    private string DecompileDelc(byte[] spec)
    {
        // del_chrs: [0] fld (5B), [5] at (5B), [10] nchr (5B), [15] mem (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "delc");
        var sb = new StringBuilder($"delc {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "at");
        A(10, "nchr");
        A(15, "mem");
        return sb.ToString();
    }

    private string DecompileInsert(byte[] spec)
    {
        // insert: [0] fld (5B), [5] flst (5B)
        if (spec.Length < 5) return "insert";
        string fld = SafeParam(spec, 0);
        var sb = new StringBuilder($"insert {fld}");
        if (spec.Length >= 10) { string flst = SafeParam(spec, 5); if (!string.IsNullOrEmpty(flst)) sb.Append($" {flst}"); }
        return sb.ToString();
    }

    private string DecompileRepl(byte[] spec)
    {
        // stuff: [0] stuffed (5B), [5] at (5B), [10] num_chrs (5B), [15] stuffee (5B),
        // [20] overwrite (1B), [21] mem (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "repl");
        var sb = new StringBuilder($"repl {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "at");
        A(10, "nchr");
        A(15, "stuffee");
        if (spec.Length > 20 && spec[20] == 'Y') sb.Append(" overwrite");
        A(21, "mem");
        return sb.ToString();
    }

    private string DecompileSound(byte[] spec)
    {
        // sound: [0] note_ary (5B), [5] beat_ary (5B), [10] max_notes (5B), [15] wav_file (5B)
        if (spec.Length < 5) return "sound";
        var sb = new StringBuilder($"sound {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "beat");
        A(10, "max");
        A(15, "wav");
        return sb.ToString();
    }

    private string DecompileSort3(byte[] spec)
    {
        // sort_p30: [0] mem (5B), [5] fld (5B), [10] size (5B), [15] num (5B), [20] wtd (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "sort3");
        var sb = new StringBuilder("sort3");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "mem");
        A(5, "fld");
        A(10, "size");
        A(15, "num");
        A(20, "wtd");
        return sb.ToString();
    }

    private string DecompileEquMid(byte[] spec)
    {
        // equ_mid_p30: [0] recv (5B), [5] fld (5B), [10] start (5B), [15] size (5B), [20] mem (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "equ_mid");
        var sb = new StringBuilder("equ_mid");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "recv");
        A(5, "fld");
        A(10, "start");
        A(15, "size");
        A(20, "mem");
        return sb.ToString();
    }

    private string DecompilePaint(byte[] spec)
    {
        // paint: [0] frm_x (5B), [5] frm_y (5B), [10] thr_x (5B), [15] thr_y (5B), [20] color (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "paint");
        var sb = new StringBuilder("paint");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "frm_x");
        A(5, "frm_y");
        A(10, "thr_x");
        A(15, "thr_y");
        A(20, "color");
        return sb.ToString();
    }

    private string DecompileRowColor(byte[] spec)
    {
        // row_color: [0] from (5B), [5] thru (5B), [10] bkg (5B), [15] text (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "row_color");
        var sb = new StringBuilder("row_color");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "from");
        A(5, "thru");
        A(10, "bkg");
        A(15, "text");
        return sb.ToString();
    }

    private string DecompileHotSpot(byte[] spec)
    {
        // hot_spot: [0] col (5B), [5] row (5B), [10] len (5B), [15] wdt (5B),
        // [20] hndl (5B), [25] key (5B), [30] remove (1B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "hot_spot");
        var sb = new StringBuilder("hot_spot");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "col");
        A(5, "row");
        A(10, "len");
        A(15, "wdt");
        A(20, "hndl");
        A(25, "key");
        if (spec.Length > 30 && spec[30] == 'Y') sb.Append(" remove");
        return sb.ToString();
    }

    private string DecompileDispm(byte[] spec)
    {
        // disp_mem: [0] col (5B), [5] row (5B), [10] mem_num (1B), [11] strt (5B),
        // [16] esze (5B), [21] ofst (5B), [26] nchr (5B), [31] nlne (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, "dispm");
        var sb = new StringBuilder("dispm");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "col");
        A(5, "row");
        A(11, "strt");
        A(16, "esze");
        A(21, "ofst");
        A(26, "nchr");
        A(31, "nlne");
        return sb.ToString();
    }

    private string DecompileFillmem(byte[] spec)
    {
        // fill_mem: [0] area_num (1B), [1] strt (5B), [6] nchrs (5B), [11] wchr (5B)
        if (spec.Length < 6) return "fillmem";
        var sb = new StringBuilder("fillmem");
        string strt = SafeParam(spec, 1);
        if (!string.IsNullOrEmpty(strt)) sb.Append($" {strt}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(6, "nchrs");
        A(11, "wchr");
        return sb.ToString();
    }

    private string DecompileWrap(byte[] spec)
    {
        // wrap: [0] fld (5B), [5] col (5B), [10] dlnes (5B)
        if (spec.Length < 5) return "wrap";
        var sb = new StringBuilder($"wrap {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "col");
        A(10, "dlnes");
        return sb.ToString();
    }

    private string DecompileGetlbl(byte[] spec)
    {
        // get_lbl: [0] label (4B), [4] fld (5B)
        if (spec.Length < 4) return "getlbl";
        int lbl = _spec.ReadInt32(spec, 0);
        var sb = new StringBuilder($"getlbl {LabelName(lbl)}");
        if (spec.Length >= 9) { string fld = SafeParam(spec, 4); if (!string.IsNullOrEmpty(fld)) sb.Append($" {fld}"); }
        return sb.ToString();
    }

    private string DecompileRdwrt(byte[] spec, string cmd)
    {
        // f_read/f_write: [0] hndl (5B), [5] pos (5B), [10] nchrs (5B), [15] tofrm (5B),
        // [20] mem_area (5B), [25] ofst (5B)
        if (spec.Length < 5) return cmd;
        var sb = new StringBuilder($"{cmd} {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "pos");
        A(10, "nchrs");
        A(15, "tofrm");
        A(20, "mem");
        A(25, "ofst");
        return sb.ToString();
    }

    private string DecompileExportImport(byte[] spec, string cmd)
    {
        // import/export: follows calc_recs layout [0-51] plus:
        // [0] frm (5B), [5] to (5B), [10] hndl (5B), [15] key (5B),
        // [20] strt (5B), [25] scope (1B), [26] sval (5B), [31] for (5B),
        // [36] while (5B), [41] cntr (5B), [46] numb (5B), [51] memory (1B), [52] display (1B),
        // [53] ftyp (5B), [58] dchr (5B), [63] apnd (5B), [65] name (1B), [66] qual (5B)
        if (spec.Length < 10) return DecompileGenericParams(spec, cmd);
        var sb = new StringBuilder(cmd);
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "frm");
        A(5, "to");
        A(10, "hndl");
        A(15, "key");
        A(20, "strt");
        if (spec.Length > 25 && spec[25] != 0 && spec[25] != ' ') sb.Append($" scope {(char)spec[25]}");
        A(26, "sval");
        A(31, "for");
        A(36, "while");
        A(41, "cntr");
        A(46, "numb");
        if (spec.Length > 51 && spec[51] == 'Y') sb.Append(" memory");
        if (spec.Length > 52 && spec[52] == 'Y') sb.Append(" display");
        A(53, "ftyp");
        A(58, "dchr");
        A(66, "qual");
        return sb.ToString();
    }

    private string DecompileOwner(byte[] spec)
    {
        // owner: [0] hndl (5B), [5] clr_set (1B), [6] name (5B), [11] crypt (1B), [12] rd_ok (1B)
        if (spec.Length < 5) return "owner";
        var sb = new StringBuilder($"owner {SafeParam(spec, 0)}");
        if (spec.Length >= 11) { string name = SafeParam(spec, 6); if (!string.IsNullOrEmpty(name)) sb.Append($" {name}"); }
        return sb.ToString();
    }

    private string DecompileAlloc(byte[] spec)
    {
        // alloc_fld: [0] fname (5B), [5] ftyp (5B), [10] fsze (5B), [15] fdec (5B),
        // [20] fary (5B), [25] upcse (5B), [30] fmask (5B)
        if (spec.Length < 5) return "alloc";
        var sb = new StringBuilder($"alloc {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "type");
        A(10, "size");
        A(15, "dec");
        A(20, "ary");
        A(25, "upc");
        A(30, "mask");
        return sb.ToString();
    }

    private string DecompileMouse(byte[] spec)
    {
        // mouse: [0] fn (5B), [5] cc (5B), [10] lok (1B), [11] err_lbl (4B),
        // [15] owner (5B), [20] path (5B), [25] fd (5B), [30] on_off (1B)
        if (spec.Length < 5) return "mouse";
        var sb = new StringBuilder($"mouse {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "cc");
        if (spec.Length >= 15) { int lbl = _spec.ReadInt32(spec, 11); if (lbl != 0) sb.Append($" err {LabelName(lbl)}"); }
        A(15, "owner");
        A(20, "path");
        A(25, "fd");
        if (spec.Length > 30 && spec[30] != 0) sb.Append($" {(char)spec[30]}");
        return sb.ToString();
    }

    private string DecompileDall(byte[] spec)
    {
        // del_all: [0] hndl (5B), [5] for (5B), [10] while (5B),
        // [15] disp (1B), [16] cntr (5B), [21] scope (1B), [22] sval (5B),
        // [27] key (5B), [32] strt (5B)
        if (spec.Length < 5) return "dall";
        var sb = new StringBuilder($"dall {SafeParam(spec, 0)}");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(5, "for");
        A(10, "while");
        if (spec.Length > 15 && spec[15] == 'Y') sb.Append(" display");
        A(16, "cntr");
        if (spec.Length > 21 && spec[21] != 0 && spec[21] != ' ') sb.Append($" scope {(char)spec[21]}");
        A(22, "sval");
        A(27, "key");
        A(32, "strt");
        return sb.ToString();
    }

    private string DecompileDel(byte[] spec)
    {
        // del_rec: [0] num (5B), [5] no_ask (1B), [6] nodel_lbl (4B), [11] err_lbl (4B)
        if (spec.Length < 5) return "del";
        var sb = new StringBuilder($"del {SafeParam(spec, 0)}");
        if (spec.Length > 5 && spec[5] == 'Y') sb.Append(" noask");
        if (spec.Length >= 10) { int lbl = _spec.ReadInt32(spec, 6); if (lbl != 0) sb.Append($" nodel {LabelName(lbl)}"); }
        if (spec.Length >= 15) { int lbl = _spec.ReadInt32(spec, 11); if (lbl != 0) sb.Append($" err {LabelName(lbl)}"); }
        return sb.ToString();
    }

    private string DecompileSetline(byte[] spec)
    {
        // setline: [0] recv (5B), [5] flst (5B)
        if (spec.Length < 5) return "setline";
        string recv = SafeParam(spec, 0);
        var sb = new StringBuilder($"setline {recv}");
        if (spec.Length >= 10) { string flst = SafeParam(spec, 5); if (!string.IsNullOrEmpty(flst)) sb.Append($" {flst}"); }
        return sb.ToString();
    }

    private string DecompileErr(byte[] spec)
    {
        // error: [0] fld (5B), [5] num (5B)
        if (spec.Length < 5) return "err";
        string fld = SafeParam(spec, 0);
        var sb = new StringBuilder($"err {fld}");
        if (spec.Length >= 10) { string num = SafeParam(spec, 5); if (!string.IsNullOrEmpty(num)) sb.Append($" {num}"); }
        return sb.ToString();
    }

    private string DecompileRenf(byte[] spec)
    {
        // rename: just two params [0] from (5B), [5] to (5B)
        if (spec.Length < 10) return DecompileOneParam(spec, "renf");
        string from = SafeParam(spec, 0);
        string to = SafeParam(spec, 5);
        return $"renf {from} to {to}";
    }

    private string DecompileTrace(byte[] spec)
    {
        // trace: [0] do_what (1B), [1] fw (5B)
        if (spec.Length < 1) return "trace";
        char wtd = (char)spec[0];
        var sb = new StringBuilder($"trace {wtd}");
        if (spec.Length >= 6) { string fw = SafeParam(spec, 1); if (!string.IsNullOrEmpty(fw)) sb.Append($" {fw}"); }
        return sb.ToString();
    }

    private string DecompileClock(byte[] spec)
    {
        // clock: [0] on_off (1B), [1] col (5B), [6] row (5B), [11] mil (1B)
        if (spec.Length < 1) return "clock";
        var sb = new StringBuilder($"clock {(char)spec[0]}");
        if (spec.Length >= 6) { string col = SafeParam(spec, 1); if (!string.IsNullOrEmpty(col)) sb.Append($" {col}"); }
        if (spec.Length >= 11) { string row = SafeParam(spec, 6); if (!string.IsNullOrEmpty(row)) sb.Append($",{row}"); }
        if (spec.Length > 11 && spec[11] == 'Y') sb.Append(" mil");
        return sb.ToString();
    }

    private string DecompileUp(byte[] spec)
    {
        // up_arrow: [0] udf (5B), [5] goto_lbl (4B)
        if (spec.Length < 5) return "up";
        string udf = SafeParam(spec, 0);
        var sb = new StringBuilder($"up {udf}");
        if (spec.Length >= 9) { int lbl = _spec.ReadInt32(spec, 5); if (lbl != 0) sb.Append($" goto {LabelName(lbl)}"); }
        return sb.ToString();
    }

    private string DecompileScrn(byte[] spec)
    {
        // scrn: 1 byte - action char (S/R/L/U/E/D)
        if (spec.Length < 1) return "scrn";
        char action = (char)spec[0];
        return action != '\0' ? $"scrn {action}" : "scrn";
    }

    private string DecompilePset(byte[] spec)
    {
        // prtset: 35 bytes - wdt@0, tlnes@5, plnes@10, pwhr@15, rtm@20, tag@25, bookmarks@30
        if (spec.Length < 5) return DecompileGenericParams(spec, "pset");
        var sb = new StringBuilder("pset");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "");          // wdt (positional, no keyword)
        A(5, "");          // tlnes
        A(10, "");         // plnes
        A(15, "pwhr");
        A(20, "rtm");
        A(25, "tag");
        A(30, "bookmarks");
        return sb.ToString();
    }

    private string DecompileCaption(byte[] spec)
    {
        // set_caption: 6 bytes - caption@0(5B), dontchange@5(1B Y/N)
        if (spec.Length < 5) return DecompileGenericParams(spec, "caption");
        var sb = new StringBuilder("caption");
        string val = SafeParam(spec, 0);
        if (!string.IsNullOrEmpty(val)) sb.Append($" {val}");
        if (spec.Length > 5 && spec[5] == 'Y') sb.Append(" dontchange");
        return sb.ToString();
    }

    private string DecompilePutFld(byte[] spec)
    {
        // put_fld_p30: 5 bytes - recv_fld@0
        if (spec.Length < 5) return "put_fld";
        string fld = SafeParam(spec, 0);
        return !string.IsNullOrEmpty(fld) ? $"put_fld {fld}" : "put_fld";
    }

    private string DecompileParam(byte[] spec)
    {
        // params: 5 bytes - field_list@0
        if (spec.Length < 5) return "param";
        string fld = SafeParam(spec, 0);
        return !string.IsNullOrEmpty(fld) ? $"param {fld}" : "param";
    }

    private string DecompileFilter(byte[] spec)
    {
        // set_filter: 5 bytes - expression@0
        if (spec.Length < 5) return "filter";
        string expr = SafeParam(spec, 0);
        return !string.IsNullOrEmpty(expr) ? $"filter {expr}" : "filter";
    }

    private string DecompileForce(byte[] spec)
    {
        // force: 1 byte Y/N flag @0
        if (spec.Length < 1) return "force";
        char flag = (char)spec[0];
        return flag == 'Y' ? "force on" : flag == 'N' ? "force off" : $"force {flag}";
    }

    private string DecompileForce3(byte[] spec)
    {
        // prt_out_wind: 1 byte Y/N flag @0
        if (spec.Length < 1) return "force3";
        char flag = (char)spec[0];
        return flag == 'Y' ? "force3 on" : flag == 'N' ? "force3 off" : $"force3 {flag}";
    }

    private string DecompileSetact(byte[] spec)
    {
        // set_active: 10 bytes - schema@0, file@5
        if (spec.Length < 5) return "setact";
        var sb = new StringBuilder("setact");
        string schema = SafeParam(spec, 0);
        if (!string.IsNullOrEmpty(schema)) sb.Append($" {schema}");
        if (spec.Length >= 10) { string file = SafeParam(spec, 5); if (!string.IsNullOrEmpty(file)) sb.Append($" file {file}"); }
        return sb.ToString();
    }

    private string DecompileSrch(byte[] spec)
    {
        // srchfle: 10 bytes - filename@0, key@5
        if (spec.Length < 5) return "srch";
        var sb = new StringBuilder("srch");
        string file = SafeParam(spec, 0);
        if (!string.IsNullOrEmpty(file)) sb.Append($" {file}");
        if (spec.Length >= 10) { string key = SafeParam(spec, 5); if (!string.IsNullOrEmpty(key)) sb.Append($" key {key}"); }
        return sb.ToString();
    }

    private string DecompilePostmsg(byte[] spec)
    {
        // postmsg: 10 bytes - routine@0, form@5
        if (spec.Length < 5) return "postmsg";
        var sb = new StringBuilder("postmsg");
        string routine = SafeParam(spec, 0);
        if (!string.IsNullOrEmpty(routine)) sb.Append($" {routine}");
        if (spec.Length >= 10) { string form = SafeParam(spec, 5); if (!string.IsNullOrEmpty(form)) sb.Append($" form {form}"); }
        return sb.ToString();
    }

    private string DecompileWcolor(byte[] spec)
    {
        // win_color_set: 10 bytes - color_name@0, color_val@5
        if (spec.Length < 5) return "wcolor";
        var sb = new StringBuilder("wcolor");
        string name = SafeParam(spec, 0);
        if (!string.IsNullOrEmpty(name)) sb.Append($" {name}");
        if (spec.Length >= 10) { string val = SafeParam(spec, 5); if (!string.IsNullOrEmpty(val)) sb.Append($" val {val}"); }
        return sb.ToString();
    }

    private string DecompileOpno(byte[] spec)
    {
        // opno_p30: 48 bytes - filename@0(5B), type@30(1B char), rsize@36(5B), bufname@41(5B)
        if (spec.Length < 5) return DecompileGenericParams(spec, "opno");
        var sb = new StringBuilder("opno");
        string fn = SafeParam(spec, 0);
        if (!string.IsNullOrEmpty(fn)) sb.Append($" {fn}");
        if (spec.Length > 30 && spec[30] != 0) sb.Append($" type {(char)spec[30]}");
        if (spec.Length >= 41) { string sz = SafeParam(spec, 36); if (!string.IsNullOrEmpty(sz)) sb.Append($" size {sz}"); }
        if (spec.Length >= 46) { string buf = SafeParam(spec, 41); if (!string.IsNullOrEmpty(buf)) sb.Append($" buf {buf}"); }
        return sb.ToString();
    }

    private string DecompileEquDay(byte[] spec)
    {
        // equ_day_p30: 10 bytes - recv@0, fld@5
        if (spec.Length < 5) return "equ_day";
        var sb = new StringBuilder("equ_day");
        string recv = SafeParam(spec, 0);
        if (!string.IsNullOrEmpty(recv)) sb.Append($" {recv}");
        if (spec.Length >= 10) { string fld = SafeParam(spec, 5); if (!string.IsNullOrEmpty(fld)) sb.Append($" fld {fld}"); }
        return sb.ToString();
    }

    private string DecompileEquXmt(byte[] spec)
    {
        // equ_mnth_p30: 10 bytes - recv@0, fld@5
        if (spec.Length < 5) return "equ_xmt";
        var sb = new StringBuilder("equ_xmt");
        string recv = SafeParam(spec, 0);
        if (!string.IsNullOrEmpty(recv)) sb.Append($" {recv}");
        if (spec.Length >= 10) { string fld = SafeParam(spec, 5); if (!string.IsNullOrEmpty(fld)) sb.Append($" fld {fld}"); }
        return sb.ToString();
    }

    private string DecompileList(byte[] spec)
    {
        // list_mem: complex layout, many options. Use SafeParam at key offsets.
        if (spec.Length < 5) return "list";
        var sb = new StringBuilder("list");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "");           // field list
        A(5, "enter");      // enter UDF
        A(10, "srch");      // search UDF (offset guess, may vary)
        A(15, "hlp");       // help UDF
        A(20, "choose");    // choose expr
        A(25, "other");     // other UDF
        return sb.ToString();
    }

    private string DecompileRcn(byte[] spec)
    {
        // rcn: set record number - likely 1 param
        if (spec.Length < 5) return "rcn";
        string fld = SafeParam(spec, 0);
        return !string.IsNullOrEmpty(fld) ? $"rcn {fld}" : "rcn";
    }

    private string DecompileIfdup(byte[] spec)
    {
        // ifdup: compiled in separate routine, spec is 0 or minimal
        // Typically has a jump offset + maybe an expression
        if (spec.Length < 4) return "ifdup";
        int jump = _spec.ReadInt32(spec, 0);
        var sb = new StringBuilder("ifdup");
        if (jump != 0) sb.Append($" goto {LabelName(jump)}");
        return sb.ToString();
    }

    private string DecompileIfna(byte[] spec)
    {
        // ifna: compiled in separate routine, spec is 0 or minimal
        if (spec.Length < 4) return "ifna";
        int jump = _spec.ReadInt32(spec, 0);
        var sb = new StringBuilder("ifna");
        if (jump != 0) sb.Append($" goto {LabelName(jump)}");
        return sb.ToString();
    }

    private string DecompilePon(byte[] spec)
    {
        // pon: printer on - likely 1 param or simple flag
        if (spec.Length < 5) return "pon";
        string val = SafeParam(spec, 0);
        return !string.IsNullOrEmpty(val) ? $"pon {val}" : "pon";
    }

    private string DecompileTransx(byte[] spec)
    {
        // transx: transaction command - params vary
        if (spec.Length < 5) return DecompileGenericParams(spec, "transx");
        var sb = new StringBuilder("transx");
        void A(int o, string k) { if (spec.Length >= o + 5) { string v = SafeParam(spec, o); if (!string.IsNullOrEmpty(v)) sb.Append($" {k} {v}"); } }
        A(0, "");
        A(5, "");
        return sb.ToString();
    }

    private static readonly HashSet<char> _validParamTypes = ['F', 'C', 'N', 'X', 'x', 'Y', 'M', 's', 'q'];

    /// <summary>
    /// Generic decompiler: dump all 5-byte params from the spec, skipping invalid type markers.
    /// </summary>
    private string DecompileGenericParams(byte[] spec, string cmd)
    {
        if (spec.Length < 5) return cmd;

        var parts = new List<string> { cmd };
        for (int offset = 0; offset + 5 <= spec.Length; offset += 5)
        {
            var (type, _) = _spec.ReadSpecParam(spec, offset);
            if (!_validParamTypes.Contains(type))
                continue; // skip flags, jump addresses, or padding
            string p = Param(spec, offset);
            if (!string.IsNullOrEmpty(p))
                parts.Add(p);
        }
        return string.Join(" ", parts);
    }
}
