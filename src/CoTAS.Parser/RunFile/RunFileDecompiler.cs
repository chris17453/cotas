using System.Text;

namespace CoTAS.Parser.RunFile;

/// <summary>
/// Decompiles TAS .RUN bytecode back to readable TAS source code.
///
/// Approach: walk spec bytes sequentially using type-prefix encoding.
/// Each spec parameter is either:
///   - 5 bytes: type(1) + int32 location(4) for F/C/N/X/Y/M/G/D/S etc.
///   - 1 byte:  flag/marker byte (0x00, 'T', 'E', 'R', etc.)
///
/// Control flow commands (IF, GOTO, GOSUB, FOR, WHILE, etc.) have
/// fixed layouts with label addresses at known offsets.
/// Everything else uses sequential type-prefix walking.
/// </summary>
public sealed class RunFileDecompiler
{
    private readonly RunFileReader _run;
    private readonly SpecDecoder _spec;
    private readonly Dictionary<int, string> _labelsByInstrIndex = new();
    private int _indent;
    private bool _lastIfIsBlock;
    private bool _thenIndent; // indent next line after IF...THEN
    private readonly Dictionary<int, List<string>> _closingKeywords = new();
    private readonly HashSet<int> _suppressedElse = new(); // ELSE inside SELECT (jump to ENDC, not real ELSE)
    private readonly List<MountInfo> _screenOffsets = new();

    private record MountInfo(int Offset, char MountType, byte[] Spec);

    public RunFileDecompiler(RunFileReader run)
    {
        _run = run;
        _spec = new SpecDecoder(run);
        BuildLabelMap();
        BuildEndifMap();
    }

    /// <summary>
    /// Decompile the entire .RUN file to TAS source code.
    /// </summary>
    public string Decompile()
    {
        var sb = new StringBuilder();

        // Emit spec version pragma from ProType header
        string proType = _run.Header.ProType?.Trim('\0') ?? "";
        if (!string.IsNullOrEmpty(proType))
            sb.AppendLine($"#spec_version {proType}");

        // Emit field definitions
        EmitFieldDefinitions(sb);

        // Decompile instructions
        _indent = 0;
        for (int i = 0; i < _run.Instructions.Count; i++)
        {
            var instr = _run.Instructions[i];

            // Skip internal-only opcodes
            if (instr.CommandNumber == TasOpcode.SET_SCAN_FLG ||
                instr.CommandNumber == TasOpcode.START_SCAN)
                continue;
            // Skip ELSE opcodes that are just jump-to-ENDC inside SELECT blocks
            if (instr.CommandNumber == TasOpcode.ELSE && _suppressedElse.Contains(i))
                continue;
            if (instr.CommandNumber >= 0xFE00) // END marker
                break;

            // Emit synthesized closing keywords (ENDIF, NEXT, ENDS, ENDC) before this line
            if (_closingKeywords.TryGetValue(i, out var closings))
            {
                foreach (var kw in closings)
                {
                    if (_indent > 0) _indent--;
                    // ENDC closes both the last CASE body and the SELECT block
                    if (kw == "ENDC" && _indent > 0) _indent--;
                    sb.Append(new string(' ', _indent * 3));
                    sb.AppendLine(kw);
                }
            }

            // Emit label if this instruction is a label target
            if (_labelsByInstrIndex.TryGetValue(i, out var label))
                sb.AppendLine(label + ":");

            // Adjust indent BEFORE for closing constructs
            AdjustIndentBefore(instr.CommandNumber);

            string line = DecompileInstruction(instr, i);
            if (!string.IsNullOrEmpty(line))
            {
                int extra = _thenIndent ? 1 : 0;
                _thenIndent = false;
                sb.Append(new string(' ', (_indent + extra) * 3));
                sb.AppendLine(line);
            }

            // Adjust indent AFTER for opening constructs
            AdjustIndentAfter(instr.CommandNumber);
        }

        // Emit screen/report format definitions collected from MOUNT instructions
        if (_screenOffsets.Count > 0)
        {
            sb.AppendLine();
            sb.AppendLine(";End of Prg");
            int screenNum = 1;
            foreach (var mi in _screenOffsets)
                DecodeScreenFormat(sb, mi.Offset, screenNum++, mi.MountType, mi.Spec);
        }

        return sb.ToString();
    }

    /// <summary>
    /// Decompile a single instruction to a source line.
    /// </summary>
    public string DecompileInstruction(RunBytecodeInstruction instr, int instrIndex)
    {
        byte[] spec = _spec.GetSpecBytes(instr);
        ushort op = instr.CommandNumber;
        string name = TasOpcode.GetName(op);

        // Control flow with fixed-layout specs
        switch (op)
        {
            case TasOpcode.GOTO: return DecompileGoto(spec);
            case TasOpcode.GOSUB: return DecompileGosub(spec);
            case TasOpcode.GOSUBL: return DecompileGosubl(spec);
            case TasOpcode.GOTOL: return DecompileGotol(spec);
            case TasOpcode.IF: return DecompileIf(spec);
            case TasOpcode.ELSE_IF: return DecompileElseIf(spec);
            case TasOpcode.ELSE: return "ELSE";
            case TasOpcode.ENDIF: return "ENDIF";
            case TasOpcode.WHILE: return DecompileWhile(spec);
            case TasOpcode.ENDW: return "ENDW";
            case TasOpcode.FOR: return DecompileFor(spec);
            case TasOpcode.NEXT: return "NEXT";
            case TasOpcode.SELECT: return DecompileSelect(spec);
            case TasOpcode.CASE: return DecompileCase(spec);
            case TasOpcode.OTHERWISE: return "OTHERWISE";
            case TasOpcode.ENDC: return "ENDC";
            case TasOpcode.SCAN: return DecompileScan(spec);
            case TasOpcode.ENDS: return "ENDS";
            case TasOpcode.LOOP: return "LOOP";
            case TasOpcode.EXIT_CMD: return "EXIT";
            case TasOpcode.LOOP_IF: return DecompileCondJump(spec, "LOOP_IF");
            case TasOpcode.EXIT_IF: return DecompileCondJump(spec, "EXIT_IF");
            case TasOpcode.FLOOP: return "FLOOP";
            case TasOpcode.FEXIT: return "FEXIT";
            case TasOpcode.FLOOP_IF: return DecompileCondJump(spec, "FLOOP_IF");
            case TasOpcode.FEXIT_IF: return DecompileCondJump(spec, "FEXIT_IF");
            case TasOpcode.SLOOP: return "SLOOP";
            case TasOpcode.SEXIT: return "SEXIT";
            case TasOpcode.SLOOP_IF: return DecompileCondJump(spec, "SLOOP_IF");
            case TasOpcode.SEXIT_IF: return DecompileCondJump(spec, "SEXIT_IF");
            case TasOpcode.RET: return DecompileRet(spec);
            case TasOpcode.QUIT: return DecompileQuit(spec);
            case TasOpcode.NOP: return DecompileNop(spec);
            case TasOpcode.BRACE_OPEN: return "{";
            case TasOpcode.BRACE_CLOSE: return "}";

            // Assignment is special: target = source
            case TasOpcode.ASSIGN: return DecompileAssign(spec);
            case TasOpcode.POINTER: return DecompilePointer(spec);

            // TRAP has mixed layout: type-prefix + flag + label
            case TasOpcode.TRAP: return DecompileTrap(spec);
            case TasOpcode.XTRAP: return DecompileXtrap(spec);

            // ON GOTO/GOSUB has variable-length label list
            case TasOpcode.ON: return DecompileOn(spec);

            // DEFINE has its own format
            case TasOpcode.DEFINE: return DecompileDefine(spec, instrIndex);

            // FUNC/CMD user-defined function/command definition
            case TasOpcode.FUNC: return DecompileUdfc(spec, "FUNC");
            case TasOpcode.CMD: return DecompileUdfc(spec, "CMD");
            case TasOpcode.UDC: return DecompileUdc(spec);

            // Data commands
            case TasOpcode.OPEN: return DecompileOpen(spec, "OPEN");
            case TasOpcode.OPENV: return DecompileOpen(spec, "OPENV");
            case TasOpcode.OPNO: return DecompileOpeno(spec);
            case TasOpcode.CLOSE: return DecompileClose(spec);
            case TasOpcode.FIND: return DecompileFind(spec, "FIND");
            case TasOpcode.FINDV: return DecompileFind(spec, "FINDV");
            case TasOpcode.SAVE: return DecompileSave(spec);
            case TasOpcode.DEL: return DecompileDel(spec);
            case TasOpcode.DALL: return DecompileDall(spec);
            case TasOpcode.OWNER: return DecompileOwner(spec);
            case TasOpcode.REL: return DecompileRel(spec);
            case TasOpcode.CLR: return DecompileClr(spec);
            case TasOpcode.RDA: return DecompileRda(spec);
            case TasOpcode.WRTA: return DecompileWrta(spec);
            case TasOpcode.UPDTA: return DecompileUpdta(spec);
            case TasOpcode.REMVA: return DecompileGenericVal1("REMVA", spec);
            case TasOpcode.REDEF: return DecompileRedef(spec);
            case TasOpcode.ADD: return DecompileAdd(spec);
            case TasOpcode.ALLOC: return DecompileAlloc(spec);
            case TasOpcode.DEALOC: return DecompileGenericVal1("DEALOC", spec);
            case TasOpcode.INSERT: return DecompileInsert(spec);
            case TasOpcode.SETLINE: return DecompileSetline(spec);
            case TasOpcode.READ: return DecompileRdWrt(spec, "READ");
            case TasOpcode.WRITE: return DecompileRdWrt(spec, "WRITE");
            case TasOpcode.RDREC: return DecompileRdWrt(spec, "RDREC");
            case TasOpcode.WTREC: return DecompileRdWrt(spec, "WTREC");
            case TasOpcode.ROPEN: return DecompileGenericVal1("ROPEN", spec);
            case TasOpcode.FILTER: return DecompileGenericVal1("FILTER", spec);

            // Screen / display commands
            case TasOpcode.ENTER: return DecompileEnter(spec);
            case TasOpcode.MSG: return DecompileMsg(spec);
            case TasOpcode.PMSG: return DecompilePmsg(spec);
            case TasOpcode.PCHR: return DecompilePchr(spec);
            case TasOpcode.PFMT: return DecompilePfmt(spec);
            case TasOpcode.PBOX: return DecompilePbox(spec);
            case TasOpcode.PBLNK: return DecompilePblnk(spec);
            case TasOpcode.PVERT: return DecompilePvert(spec);
            case TasOpcode.SCROLL: return DecompileScroll(spec);
            case TasOpcode.WINDOW: return DecompileWindow(spec);
            case TasOpcode.WINDEF: return DecompileWindow(spec);  // same layout
            case TasOpcode.WINACT: return DecompileWindow(spec);  // same layout
            case TasOpcode.SAY: return DecompileSay(spec);
            case TasOpcode.CLRLNE: return DecompileClrline(spec);
            case TasOpcode.FILL: return DecompileFill(spec);
            case TasOpcode.MID_CMD: return DecompileMid(spec);
            case TasOpcode.DELC: return DecompileDelc(spec);
            case TasOpcode.FORMAT: return DecompileFormat(spec);
            case TasOpcode.WRAP: return DecompileWrap(spec, "WRAP");
            case TasOpcode.REWRAP: return DecompileWrap(spec, "REWRAP");
            case TasOpcode.CURSOR: return DecompileCursor(spec);
            case TasOpcode.BELL: return DecompileGenericVal1("BELL", spec);
            case TasOpcode.COLOR: return DecompileGenericVal1("COLOR", spec);
            case TasOpcode.CLRSF: return "CLRSF";
            case TasOpcode.CLRSCR: return "CLRSCR";
            case TasOpcode.DISPF: return DecompileGeneric("DISPF", spec);
            case TasOpcode.DISPM: return DecompileDispm(spec);
            case TasOpcode.FILLMEM: return DecompileFillmem(spec);
            case TasOpcode.PAINT: return DecompilePaint(spec);
            case TasOpcode.ROW_COLOR: return DecompileRowColor(spec);
            case TasOpcode.SCRN: return DecompileScrn(spec);
            case TasOpcode.SHOW_PLINE: return DecompileShowPline(spec);
            case TasOpcode.REDSP: return DecompileRedsp(spec);
            case TasOpcode.SAVES: return DecompileSaveScrn(spec);
            case TasOpcode.MOUNT: return DecompileMount(spec);
            case TasOpcode.REMOUNT: return DecompileGenericVal1("REMOUNT", spec);

            // Menu/List commands
            case TasOpcode.MENU: return DecompileMenu(spec, "MENU");
            case TasOpcode.NMENU: return DecompileMenu(spec, "NMENU");
            case TasOpcode.LISTF: return DecompileListfm(spec, "LISTF");
            case TasOpcode.LISTM: return DecompileListfm(spec, "LISTM");
            case TasOpcode.LIST: return DecompileGeneric("LIST", spec);
            case TasOpcode.RDLIST: return DecompileRdlist(spec, "RDLIST");
            case TasOpcode.PRTALL: return DecompileRdlist(spec, "PRTALL");
            case TasOpcode.RCN_CMD: return DecompileCalcRecs(spec, "RCN");

            // Print control
            case TasOpcode.PON: return DecompilePon(spec);
            case TasOpcode.PTOF: return DecompileGenericVal1("PTOF", spec);
            case TasOpcode.CLSPF: return "CLSPF";
            case TasOpcode.PRT_NUM: return DecompileGenericVal1("PRT_NUM", spec);
            case TasOpcode.POSTMSG: return DecompileGenericVal1("POSTMSG", spec);
            case TasOpcode.PSET: return DecompileGenericVal1("PSET", spec);

            // Transfer / string commands
            case TasOpcode.XFER: return DecompileXfer(spec);
            case TasOpcode.SORTA: return DecompileSorta(spec);

            // Peek / Poke
            case TasOpcode.PEEK: return DecompilePeekPoke(spec, "PEEK");
            case TasOpcode.POKE: return DecompilePeekPoke(spec, "POKE");

            // Chain / Exec / Run
            case TasOpcode.CHAIN: return DecompileChain("CHAIN", spec);
            case TasOpcode.CHAINR: return DecompileChain("CHAINR", spec);
            case TasOpcode.EXEC: return DecompileGenericVal1("EXEC", spec);
            case TasOpcode.RUN: return DecompileRunPrg(spec);

            // RAP
            case TasOpcode.RAP: return DecompileRap(spec);

            // Increment / Decrement / Force
            case TasOpcode.INC: return DecompileGenericVal1("INC", spec);
            case TasOpcode.DEC: return DecompileGenericVal1("DEC", spec);
            case TasOpcode.FORCE: return DecompileGenericVal1("FORCE", spec);
            case TasOpcode.FORCE3: return DecompileGenericVal1("FORCE", spec);

            // Clock / Trace / Sound / Error
            case TasOpcode.CLOCK: return DecompileClock(spec);
            case TasOpcode.TRACE: return DecompileTrace(spec);
            case TasOpcode.SOUND: return DecompileSound(spec);
            case TasOpcode.ERR: return DecompileErr(spec);

            // GETLBL / UPAR
            case TasOpcode.GETLBL: return DecompileGetlbl(spec);
            case TasOpcode.UPAR: return DecompileUpar(spec);

            // Import / Export (calc_recs layout + extensions)
            case TasOpcode.IMPORT: return DecompileImpExp(spec, "IMPORT");
            case TasOpcode.EXPORT: return DecompileImpExp(spec, "EXPORT");

            // Button / Hot spot / Load picture
            case TasOpcode.BUTTON: return DecompileButton(spec);
            case TasOpcode.HOT_SPOT: return DecompileHotSpot(spec);
            case TasOpcode.LOAD_PICTURE: return DecompileLoadPicture(spec);

            // Mouse
            case TasOpcode.MOUSE: return DecompileMouse(spec);

            // Misc flag/simple commands
            case TasOpcode.REENT: return "REENT";
            case TasOpcode.BRKRET: return "BRKRET";
            case TasOpcode.ULKALL: return "ULKALL";
            case TasOpcode.POPS: return "POPS";
            case TasOpcode.CLSO: return "CLSO";
            case TasOpcode.PUSHT: return "PUSHT";
            case TasOpcode.POPT: return "POPT";
            case TasOpcode.NOREDSP: return "NOREDSP";
            case TasOpcode.NORSTRT: return "NORSTRT";
            case TasOpcode.NOVLDMSG: return "NOVLDMSG";
            case TasOpcode.KBDUP: return "KBDUP";
            case TasOpcode.CLRPE: return "CLRPE";

            // Generic val1+val2 pattern commands
            case TasOpcode.AUTOINC: return DecompileGenericVal1("AUTOINC", spec);
            case TasOpcode.AUTODEC: return DecompileGenericVal1("AUTODEC", spec);
            case TasOpcode.AUTOENTER: return DecompileGenericVal1("AUTOENTER", spec);
            case TasOpcode.AUTONEW: return DecompileGenericVal1("AUTONEW", spec);
            case TasOpcode.AUTO_RUN: return DecompileGenericVal1("AUTO_RUN", spec);
            case TasOpcode.TRIM: return DecompileGenericVal1("TRIM", spec);
            case TasOpcode.UP: return DecompileGenericVal1("UP", spec);
            case TasOpcode.JUST: return DecompileGenericVal1("JUST", spec);
            case TasOpcode.GRAY: return DecompileGenericVal1("GRAY", spec);
            case TasOpcode.CAPTION: return DecompileGenericVal1("CAPTION", spec);
            case TasOpcode.REVERSE: return DecompileGenericVal1("REV", spec);
            case TasOpcode.BKG: return DecompileGenericVal1("BKG", spec);
            case TasOpcode.FRG: return DecompileGenericVal1("FRG", spec);
            case TasOpcode.SETACT: return DecompileGenericVal1("SETACT", spec);
            case TasOpcode.PARAM: return DecompileGeneric("PARAM", spec);
            case TasOpcode.WCOLOR: return DecompileGeneric("WCOLOR", spec);
            case TasOpcode.DATE: return DecompileGenericVal1("DATE", spec);
            case TasOpcode.TIME: return DecompileGenericVal1("TIME", spec);
            case TasOpcode.DELF: return DecompileGenericVal1("DELF", spec);
            case TasOpcode.RENF: return DecompileGenericVal12("RENF", spec);
            case TasOpcode.PUSHF: return DecompileGenericVal1("PUSHF", spec);
            case TasOpcode.POPF: return DecompileGenericVal1("POPF", spec);
            case TasOpcode.INIFLE: return DecompileGenericVal1("INIFLE", spec);
            case TasOpcode.PUT_FLD: return DecompileGenericVal12("PUT_FLD", spec);
            case TasOpcode.REPL: return DecompileGeneric("REPL", spec);
            case TasOpcode.PLAYWAV: return DecompileGenericVal1("PLAYWAV", spec);
            case TasOpcode.COMM: return DecompileGeneric("COMM", spec);
            case TasOpcode.PORT: return DecompileGeneric("PORT", spec);
            case TasOpcode.ASK: return DecompileGenericVal1("ASK", spec);
            case TasOpcode.IFDUP:
            {
                // IFDUP has same spec layout as IF: expression at offset 10, if_do at 15
                if (spec.Length < 15) return "IFDUP ???";
                string expr = _spec.ResolveSpecParam(spec, 10);
                byte doByte = spec.Length > 15 ? spec[15] : (byte)'D';
                switch ((char)doByte)
                {
                    case 'D': return $"IFDUP {expr}";
                    case 'G':
                        if (spec.Length >= 20)
                            return $"IFDUP {expr} GOTO {LabelName(BitConverter.ToInt32(spec, 16))}";
                        return $"IFDUP {expr} GOTO ???";
                    case 'S':
                        if (spec.Length >= 20)
                            return $"IFDUP {expr} GOSUB {LabelName(BitConverter.ToInt32(spec, 16))}";
                        return $"IFDUP {expr} GOSUB ???";
                    case 'T': return $"IFDUP {expr} THEN";
                    case 'E': return $"IFDUP {expr} RET";
                    case 'R': return $"IFDUP {expr} REENT";
                    default: return $"IFDUP {expr}";
                }
            }
            case TasOpcode.IFNA: return DecompileGenericVal1("IFNA", spec);
            case TasOpcode.SSPCF: return DecompileGenericVal1("SSPCF", spec);
            case TasOpcode.CDPATH: return DecompileGenericVal1("CDPATH", spec);
            case TasOpcode.LD_PDRV: return DecompileGenericVal1("LD_PDRV", spec);
            case TasOpcode.INT_CMD: return DecompileGenericVal1("INT", spec);
            case TasOpcode.LIST_EXIT: return "LIST_EXIT";
            case TasOpcode.CO: return DecompileGenericVal1("CO", spec);
            case TasOpcode.TRANSX: return DecompileGenericVal1("TRANSX", spec);
            case TasOpcode.SRCH: return DecompileGeneric("SRCH", spec);

            // Memory commands (Pro 3.0 era)
            case TasOpcode.MEM_PTR: return DecompileGeneric("MEM_PTR", spec);
            case TasOpcode.MEM_SPC: return DecompileGeneric("MEM_SPC", spec);
            case TasOpcode.SORT3: return DecompileGeneric("SORT3", spec);
            case TasOpcode.SAVES3: return DecompileGeneric("SAVES3", spec);
            case TasOpcode.REDSP3: return DecompileGeneric("REDSP3", spec);
            case TasOpcode.WRAP3: return DecompileGeneric("WRAP3", spec);

            // Equation variants
            case TasOpcode.EQU_MID: return DecompileEquMid(spec);
            case TasOpcode.EQU_DAY: return DecompileGenericVal12("EQU_DAY", spec);
            case TasOpcode.EQU_XMT: return DecompileGenericVal12("EQU_XMT", spec);

            case TasOpcode.RSCR: return "RSCR";
        }

        // Everything else: walk spec bytes sequentially
        return DecompileGeneric(name, spec);
    }

    // ===== Control Flow Commands =====

    private string DecompileGoto(byte[] spec)
    {
        if (spec.Length >= 4)
        {
            int addr = BitConverter.ToInt32(spec, 0);
            return $"GOTO {LabelName(addr)}";
        }
        return "GOTO";
    }

    private string DecompileGosub(byte[] spec)
    {
        if (spec.Length >= 4)
        {
            int addr = BitConverter.ToInt32(spec, 0);
            return $"GOSUB {LabelName(addr)}";
        }
        return "GOSUB";
    }

    private string DecompileGosubl(byte[] spec)
    {
        // GOSUBL: goto(4) + field_typ(1) + field_loc(4) = 9 bytes
        if (spec.Length >= 9)
        {
            string fld = _spec.ResolveSpecParam(spec, 4);
            return $"GOSUBL {fld}";
        }
        return "GOSUBL";
    }

    private string DecompileGotol(byte[] spec)
    {
        // GOTOL: typ(1) + loc(4)
        if (spec.Length >= 5)
        {
            string val = _spec.ResolveSpecParam(spec, 0);
            return $"GOTOL {val}";
        }
        return "GOTOL";
    }

    private string DecompileIf(byte[] spec)
    {
        // if_goto(4) + pad(2) + if_endif_goto(4) + if_typ(1) + if_loc(4) + if_do(1) + if_goto_gosub(4) = 20
        if (spec.Length < 15) return "IF ???";

        string expr = _spec.ResolveSpecParam(spec, 10);
        byte doByte = spec.Length > 15 ? spec[15] : (byte)0;

        // if_do: 'D' = block IF, 'G' = GOTO, 'S' = GOSUB, 'T' = THEN, 'E' = RET, 'R' = REENT
        _lastIfIsBlock = doByte == (byte)'D';
        _thenIndent = !_lastIfIsBlock;
        switch ((char)doByte)
        {
            case 'D': return $"IF {expr}";
            case 'G':
            {
                // Label index at offset 16 (if_goto_gosub)
                if (spec.Length >= 20)
                {
                    int lblIdx = BitConverter.ToInt32(spec, 16);
                    return $"IF {expr} GOTO {LabelName(lblIdx)}";
                }
                return $"IF {expr} GOTO ???";
            }
            case 'S':
            {
                if (spec.Length >= 20)
                {
                    int lblIdx = BitConverter.ToInt32(spec, 16);
                    return $"IF {expr} GOSUB {LabelName(lblIdx)}";
                }
                return $"IF {expr} GOSUB ???";
            }
            case 'T': return $"IF {expr} THEN";
            case 'E': return $"IF {expr} RET";
            case 'R': return $"IF {expr} REENT";
            default: return $"IF {expr}";
        }
    }

    private string DecompileElseIf(byte[] spec)
    {
        // Same layout as IF
        if (spec.Length < 15) return "ELSE_IF ???";
        string expr = _spec.ResolveSpecParam(spec, 10);
        return $"ELSE_IF {expr}";
    }

    private string DecompileWhile(byte[] spec)
    {
        // while_goto(4) + while_typ(1) + while_loc(4) = 9
        if (spec.Length >= 9)
        {
            string expr = _spec.ResolveSpecParam(spec, 4);
            return $"WHILE {expr}";
        }
        return "WHILE";
    }

    private string DecompileFor(byte[] spec)
    {
        // for_next_goto(4) + stop(5) + step(5) + cntr(5) + start(5) = 24
        if (spec.Length < 24) return "FOR ???";

        string stop = _spec.ResolveSpecParam(spec, 4);
        string step = _spec.ResolveSpecParam(spec, 9);
        string counter = _spec.ResolveSpecParam(spec, 14);
        string start = _spec.ResolveSpecParam(spec, 19);

        // Check if step is default (1)
        var (stepType, stepLoc) = _spec.ReadSpecParam(spec, 9);
        bool hasStep = !(stepType == 'N' && stepLoc == 1);

        if (hasStep)
            return $"FOR {counter} = {start}; {stop}; {step}";
        return $"FOR {counter} = {start}; {stop}";
    }

    private string DecompileSelect(byte[] spec)
    {
        // select: abs_goto(4) + field_typ(1) + field_loc(4) = 9
        if (spec.Length >= 9)
        {
            string fld = _spec.ResolveSpecParam(spec, 4);
            return $"SELECT {fld}";
        }
        if (spec.Length >= 5)
        {
            string fld = _spec.ResolveSpecParam(spec, 0);
            return $"SELECT {fld}";
        }
        return "SELECT";
    }

    private string DecompileCase(byte[] spec)
    {
        // IF layout: if_goto(0,4) + if_endif_goto(6,4) + if_typ(10,1) + if_loc(11,4) + if_do(15,1) + if_goto_gosub(16,4)
        // CASE uses if_typ/if_loc at offset 10 as the comparison value
        if (spec.Length < 15) return "CASE";

        string val = P(spec, 10);
        return !string.IsNullOrEmpty(val) ? $"CASE {val}" : "CASE";
    }

    private string DecompileScan(byte[] spec)
    {
        // scan_end_jump(4) + handle(5) + key(5) + start(5) + scope(1) + sval(5) + ...
        if (spec.Length < 19) return "SCAN";

        var parts = new List<string>();

        // Handle (file reference)
        string handle = _spec.ResolveSpecParam(spec, 4);
        if (!string.IsNullOrEmpty(handle)) parts.Add(handle);

        // Key
        string key = _spec.ResolveSpecParam(spec, 9);
        if (!string.IsNullOrEmpty(key)) parts.Add(key);

        // Start value
        string start = _spec.ResolveSpecParam(spec, 14);
        if (!string.IsNullOrEmpty(start)) parts.Add(start);

        // Scope flag
        if (spec.Length > 19)
        {
            byte scope = spec[19];
            if (scope == (byte)'W') parts.Add("WHILE");
            else if (scope == (byte)'F') parts.Add("FOR");
        }

        // Scan value (at offset 20)
        if (spec.Length >= 25)
        {
            string sval = _spec.ResolveSpecParam(spec, 20);
            if (!string.IsNullOrEmpty(sval)) parts.Add(sval);
        }

        // Additional params (NLOCK, etc.)
        int pos = 25;
        while (pos + 5 <= spec.Length)
        {
            char typ = (char)spec[pos];
            if (typ == '\0') break;
            string val = _spec.ResolveSpecParam(spec, pos);
            if (!string.IsNullOrEmpty(val)) parts.Add(val);
            pos += 5;
        }

        return $"SCAN {string.Join(", ", parts)}";
    }

    private string DecompileCondJump(byte[] spec, string keyword)
    {
        // While layout: jump_addr(0,4) + expr_typ(4,1) + expr_loc(5,4) = 9 bytes
        if (spec.Length >= 9)
        {
            string expr = _spec.ResolveSpecParam(spec, 4);
            return $"{keyword} {expr}";
        }
        return keyword;
    }

    private string DecompileRet(byte[] spec)
    {
        // ret: exit_byte(1) + address(4)
        return "RET";
    }

    private string DecompileQuit(byte[] spec)
    {
        // quit: exit_byte(1) + address(4)
        return "QUIT";
    }

    private string DecompileNop(byte[] spec)
    {
        // NOP may carry a comment string in constants
        if (spec.Length >= 5)
        {
            var (typ, loc) = _spec.ReadSpecParam(spec, 0);
            if (typ == 'C')
            {
                string comment = _spec.ResolveParam('C', loc);
                return $"* {comment}";
            }
        }
        return "*";
    }

    // ===== Assignment =====

    private string DecompileAssign(byte[] spec)
    {
        // equal_rec_typ(0) + equal_frm_typ(5) = 10 bytes
        if (spec.Length >= 10)
        {
            string target = _spec.ResolveSpecParam(spec, 0);
            string source = _spec.ResolveSpecParam(spec, 5);
            return $"{target} = {source}";
        }
        if (spec.Length >= 5)
        {
            string target = _spec.ResolveSpecParam(spec, 0);
            return $"{target} = ???";
        }
        return "= ???";
    }

    private string DecompilePointer(byte[] spec)
    {
        // Same as ASSIGN but -> instead of =
        if (spec.Length >= 10)
        {
            string target = _spec.ResolveSpecParam(spec, 0);
            string source = _spec.ResolveSpecParam(spec, 5);
            return $"{target} -> {source}";
        }
        return "-> ???";
    }

    // ===== TRAP =====

    private string DecompileTrap(byte[] spec)
    {
        // trap_num_typ(0) + trap_num_loc(1) = 5 bytes
        // trap_wtd(5) = 1 byte flag
        // trap_lbl(6) = 4 byte label address
        if (spec.Length < 10) return DecompileGeneric("TRAP", spec);

        var (keyType, keyLoc) = _spec.ReadSpecParam(spec, 0);
        string keys = _spec.DecodeTrapKeys(keyType, keyLoc);
        byte wtd = spec[5];
        int lbl = BitConverter.ToInt32(spec, 6);

        string action;
        switch ((char)wtd)
        {
            case 'G': action = $"GOTO {LabelName(lbl)}"; break;
            case 'S': action = $"GOSUB {LabelName(lbl)}"; break;
            case 'I': action = "IGNR"; break;
            case 'D': action = "DFLT"; break;
            default: action = $"?{(char)wtd}"; break;
        }

        return $"TRAP {keys} {action}";
    }

    private string DecompileXtrap(byte[] spec)
    {
        // XTRAP: wtd(1) byte only
        if (spec.Length >= 1)
        {
            byte wtd = spec[0];
            switch ((char)wtd)
            {
                case 'P': return "XTRAP PUSH";
                case 'O': return "XTRAP POP";
                case 'D': return "XTRAP DEFAULT";
                case 'A': return "XTRAP ALL_DFLT";
                default: return $"XTRAP {(char)wtd}";
            }
        }
        return "XTRAP";
    }

    // ===== ON GOTO/GOSUB =====

    private string DecompileOn(byte[] spec)
    {
        // on_val_typ(0)+loc(1) + on_tosub_flag(5) + on_num_labels(6) + labels(7+)
        if (spec.Length < 7) return DecompileGeneric("ON", spec);

        string val = _spec.ResolveSpecParam(spec, 0);
        byte toSub = spec[5];
        byte numLabels = spec[6];
        string keyword = toSub != 0 ? "GOSUB" : "GOTO";

        var labels = new List<string>();
        int pos = 7;
        for (int i = 0; i < numLabels && pos + 4 <= spec.Length; i++)
        {
            int addr = BitConverter.ToInt32(spec, pos);
            labels.Add(LabelName(addr));
            pos += 4;
        }

        return $"ON {val} {keyword} {string.Join(", ", labels)}";
    }

    // ===== DEFINE =====

    private string DecompileDefine(byte[] spec, int instrIndex)
    {
        // DEFINE doesn't use spec in the usual way.
        // The field info is in the field spec list.
        // Just skip - field definitions are emitted at the top.
        return "";
    }

    // ===== FUNC/CMD =====

    private string DecompileUdfc(byte[] spec, string keyword)
    {
        // FUNC/CMD: label(4) + param specs
        if (spec.Length < 4) return keyword;

        int addr = BitConverter.ToInt32(spec, 0);
        string label = LabelName(addr);

        var parts = new List<string>();
        int pos = 4;
        while (pos + 5 <= spec.Length)
        {
            char typ = (char)spec[pos];
            if (typ == '\0') break;
            string val = _spec.ResolveSpecParam(spec, pos);
            if (!string.IsNullOrEmpty(val)) parts.Add(val);
            pos += 5;
        }

        if (parts.Count > 0)
            return $"{keyword} {label}({string.Join(", ", parts)})";
        return $"{keyword} {label}";
    }

    private string DecompileUdc(byte[] spec)
    {
        // UDC (user defined command call): label(4) + params
        if (spec.Length < 4) return "UDC ???";

        int addr = BitConverter.ToInt32(spec, 0);
        string label = LabelName(addr);

        var parts = new List<string>();
        int pos = 4;
        while (pos + 5 <= spec.Length)
        {
            char typ = (char)spec[pos];
            if (typ == '\0') break;
            string val = _spec.ResolveSpecParam(spec, pos);
            if (!string.IsNullOrEmpty(val)) parts.Add(val);
            pos += 5;
        }

        if (parts.Count > 0)
            return $"{label} {string.Join(", ", parts)}";
        return label;
    }

    // ===== Helper: read param at offset, return empty if out of range =====

    private string P(byte[] spec, int offset)
    {
        if (offset + 5 > spec.Length) return "";
        return _spec.ResolveSpecParam(spec, offset);
    }

    private string Lbl4(byte[] spec, int offset)
    {
        if (offset + 4 > spec.Length) return "";
        return LabelName(BitConverter.ToInt32(spec, offset));
    }

    private byte Flag(byte[] spec, int offset)
    {
        return offset < spec.Length ? spec[offset] : (byte)0;
    }

    /// <summary>Build "CMD p1, p2, p3" from resolved params, skipping empties.</summary>
    private static string Emit(string name, params string[] parts)
    {
        var nonEmpty = parts.Where(p => !string.IsNullOrEmpty(p)).ToList();
        return nonEmpty.Count > 0 ? $"{name} {string.Join(", ", nonEmpty)}" : name;
    }

    // ===== Generic val1 / val1+val2 helpers =====

    private string DecompileGenericVal1(string name, byte[] spec)
    {
        return Emit(name, P(spec, 0));
    }

    private string DecompileChain(string name, byte[] spec)
    {
        // chain spec: prg_name(0) + null(5) + flag(10,1B) + param_list(11) + noclr(16,1B) + wait(17,1B)
        var sb = new System.Text.StringBuilder();
        sb.Append(Emit(name, P(spec, 0)));
        string paramList = P(spec, 11);
        if (!string.IsNullOrEmpty(paramList)) sb.Append($" WITH {paramList}");
        return sb.ToString();
    }

    private string DecompileGenericVal12(string name, byte[] spec)
    {
        return Emit(name, P(spec, 0), P(spec, 5));
    }

    // ===== Data Commands =====

    private string DecompileOpen(byte[] spec, string cmd)
    {
        // Emit with option keywords so the table-driven compiler can round-trip
        var parts = new List<string>();
        parts.Add(P(spec, 0)); // positional filename (OptionIndex=0)
        string cc = P(spec, 5);
        if (!string.IsNullOrEmpty(cc)) { parts.Add("EXT"); parts.Add(cc); }
        byte lok = Flag(spec, 10);
        if (lok != 0) { parts.Add("LOCK"); parts.Add(lok == (byte)'T' ? "T" : $"{(char)lok}"); }
        string errLbl = P(spec, 11);
        if (!string.IsNullOrEmpty(errLbl)) { parts.Add("ERR"); parts.Add(errLbl); }
        string owner = P(spec, 15);
        if (!string.IsNullOrEmpty(owner)) { parts.Add("OWNER"); parts.Add(owner); }
        string path = P(spec, 20);
        if (!string.IsNullOrEmpty(path)) { parts.Add("PATH"); parts.Add(path); }
        string fd = P(spec, 25);
        if (!string.IsNullOrEmpty(fd)) { parts.Add("FD"); parts.Add(fd); }
        byte typ = Flag(spec, 30);
        if (typ != 0) { parts.Add("TYPE"); parts.Add($"{(char)typ}"); }
        string hndl = P(spec, 31);
        if (!string.IsNullOrEmpty(hndl)) { parts.Add("FNUM"); parts.Add(hndl); }
        string rsze = P(spec, 36);
        if (!string.IsNullOrEmpty(rsze)) { parts.Add("SIZE"); parts.Add(rsze); }
        string bufnme = P(spec, 41);
        if (!string.IsNullOrEmpty(bufnme)) { parts.Add("BUFF"); parts.Add(bufnme); }
        byte create = Flag(spec, 46);
        if (create == (byte)'Y') parts.Add("CREATE");
        byte noclr = Flag(spec, 47);
        if (noclr == (byte)'Y') parts.Add("NOCLR");
        string updt = P(spec, 48);
        if (!string.IsNullOrEmpty(updt)) { parts.Add("UPDATE"); parts.Add(updt); }
        if (spec.Length > 53)
        {
            byte addflds = Flag(spec, 53);
            if (addflds == (byte)'Y') parts.Add("ADDALLFLDS");
        }
        return Emit(cmd, parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileOpeno(byte[] spec)
    {
        return Emit("OPNO", P(spec, 0), P(spec, 5), P(spec, 15), P(spec, 20), P(spec, 25));
    }

    private string DecompileClose(byte[] spec)
    {
        // close_hndl(0) + delete(5,1byte)
        string hndl = P(spec, 0);
        byte del = Flag(spec, 5);
        if (del != 0 && del != (byte)'N') return Emit("CLOSE", hndl, "DELETE");
        return Emit("CLOSE", hndl);
    }

    private string DecompileFind(byte[] spec, string name)
    {
        // find_hndl(0) + key(5) + val(10) + typ(15,1byte) + err_lbl(16,4byte) + lock(20,1byte) + key_only(21,1byte) + for(22) + while(27) + no_clr(32,1byte)
        // Syntax: FINDV typ FNUM hndl KEY key VAL val ERR lbl NLOCK KEYO NOCLR
        var sb = new System.Text.StringBuilder(name);
        byte typ = Flag(spec, 15);
        if (typ >= 0x20 && typ < 0x7F) sb.Append($" {(char)typ}");
        string hndl = P(spec, 0);
        if (!string.IsNullOrEmpty(hndl)) sb.Append($", {hndl}");
        string key = P(spec, 5);
        if (!string.IsNullOrEmpty(key)) sb.Append($", {key}");
        string val = P(spec, 10);
        if (!string.IsNullOrEmpty(val)) sb.Append($", {val}");
        string errLbl = Lbl4(spec, 16);
        if (!string.IsNullOrEmpty(errLbl)) sb.Append($", {errLbl}");
        byte noclr = Flag(spec, 32);
        if (noclr == (byte)'Y') sb.Append(", NOCLR");
        byte keyo = Flag(spec, 21);
        if (keyo == (byte)'Y') sb.Append(", KEYO");
        byte nlock = Flag(spec, 20);
        if (nlock == (byte)'Y') sb.Append(", NLOCK");
        return sb.ToString();
    }

    private string DecompileSave(byte[] spec)
    {
        // srec_hndl(0) + no_clear(5,1byte) + no_ask(6,1byte) + nosave_lbl(7,4byte) + err_lbl(11,4byte) + unlock(15,1byte)
        var parts = new List<string>();
        parts.Add(P(spec, 0));
        string errLbl = Lbl4(spec, 11);
        if (!string.IsNullOrEmpty(errLbl)) parts.Add(errLbl);
        return Emit("SAVE", parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileDel(byte[] spec)
    {
        // delrec_num(0) + no_ask(5,1byte) + nodel_lbl(6,4byte) + err_lbl(11,4byte)
        return Emit("DEL", P(spec, 0));
    }

    private string DecompileDall(byte[] spec)
    {
        // delall_hndl(0) + for(5) + while(10) + disp(15,1byte) + cntr(16) + scope(21,1byte) + sval(22) + key(27) + strt(32)
        var parts = new List<string>();
        parts.Add(P(spec, 0)); // handle
        string key = P(spec, 27);
        if (!string.IsNullOrEmpty(key)) parts.Add(key);
        string strt = P(spec, 32);
        if (!string.IsNullOrEmpty(strt)) parts.Add(strt);
        byte scope = Flag(spec, 21);
        if (scope == (byte)'W') { parts.Add("WHILE"); parts.Add(P(spec, 10)); }
        else if (scope == (byte)'F') { parts.Add("FOR"); parts.Add(P(spec, 5)); }
        return Emit("DALL", parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileOwner(byte[] spec)
    {
        // owner_hndl(0) + clr_set(5,1byte) + name(6) + crypt(11,1byte) + rd_ok(12,1byte)
        byte csf = Flag(spec, 5);
        string action = csf == (byte)'C' ? "CLR" : csf == (byte)'S' ? "SET" : "";
        return Emit("OWNER", P(spec, 0), action, P(spec, 6));
    }

    private string DecompileRel(byte[] spec)
    {
        // Repeating 20-byte entries: slave_file(0) + slave_key(5) + master_file(10) + master_field(15)
        var parts = new List<string>();
        int pos = 0;
        while (pos + 20 <= spec.Length)
        {
            string sf = P(spec, pos);
            string sk = P(spec, pos + 5);
            string mf = P(spec, pos + 10);
            string md = P(spec, pos + 15);
            if (!string.IsNullOrEmpty(sf)) parts.Add($"{sf}, {sk}, {mf}, {md}");
            pos += 20;
        }
        return parts.Count > 0 ? $"REL {string.Join("; ", parts)}" : "REL";
    }

    private string DecompileClr(byte[] spec)
    {
        // clrbuf_fn(0) + ra(5,1byte)
        return Emit("CLR", P(spec, 0));
    }

    private string DecompileRda(byte[] spec)
    {
        // rdary: from(0) + to(5) + hndl(10) + key(15) + strt(20) + scope(25,1byte) + sval(26) + for(31) + while(36) + cntr(41) + display(46,1byte)
        return Emit("RDA", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 20));
    }

    private string DecompileWrta(byte[] spec)
    {
        // wrtary: from(0) + to(5) + scope(10,1byte) + sval(11) + for(16) + hndl(21) + rec(26) + display(31,1byte) + cntr(32)
        return Emit("WRTA", P(spec, 0), P(spec, 5), P(spec, 21));
    }

    private string DecompileUpdta(byte[] spec)
    {
        // ua: all(0) + wtd(5,1byte) + times(6) + val(11) + fn(16)
        byte wtd = Flag(spec, 5);
        string action = wtd switch { (byte)'C' => "CLR", (byte)'A' => "ADD", (byte)'S' => "SUB", (byte)'F' => "FILL", _ => "" };
        return Emit("UPDTA", P(spec, 0), action, P(spec, 6), P(spec, 11));
    }

    private string DecompileRedef(byte[] spec)
    {
        // redef: fld(0) + ftyp(5) + fsze(10) + dchr(15) + fnum(20) + ofst(25) + knum(30) + pict(35) + loc(40) + up(45)
        return Emit("REDEF", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15));
    }

    private string DecompileAdd(byte[] spec)
    {
        // add_fld: name(0) + ftyp(5) + fsze(10) + fdec(15) + fupc(20) + fmask(25) + fhndl(30) + fofst(35) + ffptr(40) + fary(45) + fknum(50)
        return Emit("ADD", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 30));
    }

    private string DecompileAlloc(byte[] spec)
    {
        // aloc: fname(0) + ftyp(5) + fsze(10) + fdec(15) + fary(20) + upcse(25) + fmask(30)
        return Emit("ALLOC", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 20));
    }

    private string DecompileInsert(byte[] spec)
    {
        // ins: fld(0) + flist(5)
        return Emit("INSERT", P(spec, 0), P(spec, 5));
    }

    private string DecompileSetline(byte[] spec)
    {
        // setline: recv(0) + flist(5)
        return Emit("SETLINE", P(spec, 0), P(spec, 5));
    }

    private string DecompileRdWrt(byte[] spec, string name)
    {
        // rdwrt: hndl(0) + pos(5) + nchrs(10) + tofrm(15) + mem_area(20) + ofst(25)
        return Emit(name, P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15));
    }

    // ===== Screen / Display Commands =====

    private string DecompileEnter(byte[] spec)
    {
        // enter: col(0) + row(5) + fld(10) + msk(15) + hlp(20) + upa_lbl(25,4byte) + valid(29) + acr(34,1byte) + psw(35,1byte) + upc(36,1byte) + clr(37) + pre(42) + post(47) + dflt(54) + vmsg(59) + ...
        var parts = new List<string>();
        parts.Add(P(spec, 0)); // col
        parts.Add(P(spec, 5)); // row
        parts.Add(P(spec, 10)); // field
        string msk = P(spec, 15);
        if (!string.IsNullOrEmpty(msk)) parts.Add(msk);
        string hlp = P(spec, 20);
        if (!string.IsNullOrEmpty(hlp)) parts.Add(hlp);
        string valid = P(spec, 29);
        if (!string.IsNullOrEmpty(valid)) parts.Add(valid);
        string pre = P(spec, 42);
        if (!string.IsNullOrEmpty(pre)) parts.Add($"PRE={pre}");
        string post = P(spec, 47);
        if (!string.IsNullOrEmpty(post)) parts.Add($"POST={post}");
        string dflt = P(spec, 54);
        if (!string.IsNullOrEmpty(dflt)) parts.Add($"DFLT={dflt}");
        string vmsg = P(spec, 59);
        if (!string.IsNullOrEmpty(vmsg)) parts.Add($"VMSG={vmsg}");
        return Emit("ENTER", parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileMsg(byte[] spec)
    {
        // msg: fld(0) + no_wait(5,1byte)
        string fld = P(spec, 0);
        byte noWait = Flag(spec, 5);
        if (noWait != 0) return Emit("MSG", fld, "NOWAIT");
        return Emit("MSG", fld);
    }

    private string DecompilePmsg(byte[] spec)
    {
        // prtmsg: col(0) + row(5) + msg(10) + wait(15,1byte='Y') + ncr(16,1byte='Y') + ent(17) + whr(22,1byte) + color(23) + abs(28,1byte='Y')
        // Syntax: PMSG message AT col,row [ENTER field] [WAIT] [NOCR] [PTW whr] [COLOR color] [ABS]
        var sb = new System.Text.StringBuilder("PMSG");
        string msg = P(spec, 10);
        if (!string.IsNullOrEmpty(msg)) sb.Append($" {msg}");
        string col = P(spec, 0);
        string row = P(spec, 5);
        if (!string.IsNullOrEmpty(col) && !string.IsNullOrEmpty(row) && (col != "0" || row != "0"))
            sb.Append($" AT {col},{row}");
        string ent = P(spec, 17);
        if (!string.IsNullOrEmpty(ent)) sb.Append($" ENTER {ent}");
        if (Flag(spec, 15) == (byte)'Y') sb.Append(" WAIT");
        if (Flag(spec, 16) == (byte)'Y') sb.Append(" NOCR");
        byte whr = Flag(spec, 22);
        if (whr == (byte)'P') sb.Append(" PTW P");
        else if (whr == (byte)'S') sb.Append(" PTW S");
        string color = P(spec, 23);
        if (!string.IsNullOrEmpty(color)) sb.Append($" COLOR {color}");
        if (Flag(spec, 28) == (byte)'Y') sb.Append(" ABS");
        return sb.ToString();
    }

    private string DecompilePchr(byte[] spec)
    {
        // prtchr: ptw(0,1byte) + num(1) + fld(6)
        string fld = P(spec, 6);
        string num = P(spec, 1);
        if (!string.IsNullOrEmpty(num) && num != "0" && num != "1")
            return Emit("PCHR", fld, num);
        return Emit("PCHR", fld);
    }

    private string DecompilePfmt(byte[] spec)
    {
        // prtfmt: col(0) + row(5) + wait(10,1byte) + ncr(11,1byte) + line(12) + whr(17,1byte) + thru(18) + abs(23,1byte) + bks(24,1byte)
        var parts = new List<string>();
        parts.Add(P(spec, 0)); // col
        parts.Add(P(spec, 5)); // row
        string line = P(spec, 12);
        if (!string.IsNullOrEmpty(line)) parts.Add(line);
        string thru = P(spec, 18);
        if (!string.IsNullOrEmpty(thru)) parts.Add(thru);
        return Emit("PFMT", parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompilePbox(byte[] spec)
    {
        // prtbox: col(0) + row(5) + lnth(10) + wdt(15) + lines(20,1byte) + clr(21,1byte) + color(22) + brdr(27) + abs(32,1byte)
        return Emit("PBOX", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15));
    }

    private string DecompilePblnk(byte[] spec)
    {
        // pblnk: num(0) + whr(5,1byte)
        byte whr = Flag(spec, 5);
        string w = whr switch { (byte)'P' => "P", (byte)'S' => "S", _ => "" };
        return Emit("PBLNK", P(spec, 0), w);
    }

    private string DecompilePvert(byte[] spec)
    {
        // vert: num(0) + whr(5,1byte)
        byte whr = Flag(spec, 5);
        string w = whr switch { (byte)'P' => "P", (byte)'S' => "S", _ => "" };
        return Emit("PVERT", P(spec, 0), w);
    }

    private string DecompileScroll(byte[] spec)
    {
        // scroll: col(0) + row(5) + ht(10) + wdt(15) + lnes(20) + direction(25,1byte)
        byte dir = Flag(spec, 25);
        string d = dir switch { (byte)'U' => "UP", (byte)'D' => "DN", (byte)'L' => "LEFT", (byte)'R' => "RIGHT", _ => "" };
        return Emit("SCROLL", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 20), d);
    }

    private string DecompileWindow(byte[] spec)
    {
        // window: col(0) + row(5) + lt(10) + wdt(15) + wclr(20) + ttlfld(25) + ttlloc(30) + box(35) + bclr(40) + shad(45) + sclr(50) + in_clr(55) + sve_fld(60) + sve_fld_sze(65,4byte) + wtext(69) + btext(74) + dco(79,1byte) + active(80,1byte)
        var parts = new List<string>();
        parts.Add(P(spec, 0));  // col
        parts.Add(P(spec, 5));  // row
        parts.Add(P(spec, 10)); // length
        parts.Add(P(spec, 15)); // width
        string wclr = P(spec, 20);
        if (!string.IsNullOrEmpty(wclr)) parts.Add(wclr);
        string ttl = P(spec, 25);
        if (!string.IsNullOrEmpty(ttl)) parts.Add(ttl);
        string ttlloc = P(spec, 30);
        if (!string.IsNullOrEmpty(ttlloc)) parts.Add(ttlloc);
        string box = P(spec, 35);
        if (!string.IsNullOrEmpty(box)) parts.Add(box);
        string shad = P(spec, 45);
        if (!string.IsNullOrEmpty(shad)) parts.Add(shad);
        string sveFld = P(spec, 60);
        if (!string.IsNullOrEmpty(sveFld)) parts.Add(sveFld);
        return Emit("WINDOW", parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileSay(byte[] spec)
    {
        // say: fld(0) + col(5) + row(10) + color(15) + pict(20)
        return Emit("SAY", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 20));
    }

    private string DecompileClrline(byte[] spec)
    {
        // clrline: col(0) + row(5) + chrs(10) + no_color(15,1byte) + color(16) + abs(21,1byte)
        return Emit("CLRLNE", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 16));
    }

    private string DecompileFill(byte[] spec)
    {
        // fill: fld(0) + chr(5) + times(10) + where(15,1byte)
        return Emit("FILL", P(spec, 0), P(spec, 5), P(spec, 10));
    }

    private string DecompileMid(byte[] spec)
    {
        // stuff: stuffed(0) + at(5) + num_chrs(10) + stuffee(15) + overwrite(20,1byte) + mem(21)
        return Emit("MID", P(spec, 15), P(spec, 5), P(spec, 0), P(spec, 10));
    }

    private string DecompileDelc(byte[] spec)
    {
        // delc: fld(0) + at(5) + nchr(10) + mem(15)
        return Emit("DELC", P(spec, 0), P(spec, 5), P(spec, 10));
    }

    private string DecompileFormat(byte[] spec)
    {
        // format: fld(0) + recv(5) + commas(10,1byte) + flt_dol(11,1byte) + neg_how(12,1byte) + off(13,1byte) + pict(14) + no_zeros(19,1byte)
        string fld = P(spec, 0);
        string recv = P(spec, 5);
        string pict = P(spec, 14);
        var parts = new List<string> { fld };
        if (!string.IsNullOrEmpty(recv)) parts.Add(recv);
        if (!string.IsNullOrEmpty(pict)) parts.Add(pict);
        return Emit("FORMAT", parts.ToArray());
    }

    private string DecompileWrap(byte[] spec, string name)
    {
        // wrap: fld(0) + col(5) + dlnes(10)
        return Emit(name, P(spec, 0), P(spec, 5), P(spec, 10));
    }

    private string DecompileCursor(byte[] spec)
    {
        // cursor: strt(0) + stop(5) + on_off(10,1byte) + wait(11,1byte) + dflt(12,1byte)
        return Emit("CURSOR", P(spec, 0), P(spec, 5));
    }

    private string DecompileDispm(byte[] spec)
    {
        // dmem: col(0) + row(5) + mem_num(10,1byte) + strt(11) + esze(16) + ofst(21) + nchr(26) + nlne(31)
        return Emit("DISPM", P(spec, 0), P(spec, 5), P(spec, 11), P(spec, 16), P(spec, 26), P(spec, 31));
    }

    private string DecompileFillmem(byte[] spec)
    {
        // fmem: area_num(0,1byte) + strt(1) + nchrs(6) + wchr(11)
        return Emit("FILLMEM", P(spec, 1), P(spec, 6), P(spec, 11));
    }

    private string DecompilePaint(byte[] spec)
    {
        // paint: frm_x(0) + frm_y(5) + thr_x(10) + thr_y(15) + color(20)
        return Emit("PAINT", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 20));
    }

    private string DecompileRowColor(byte[] spec)
    {
        // row_color: from(0) + thru(5) + bkg(10) + text(15)
        return Emit("ROW_COLOR", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15));
    }

    private string DecompileScrn(byte[] spec)
    {
        // schr: col(0) + row(5) + dchr(10) + achr(15) + wtd(20,1byte) + tchr(21)
        return Emit("SCRN", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 21));
    }

    private string DecompileShowPline(byte[] spec)
    {
        // show_pl: col(0) + row(5)
        return Emit("SHOW_PLINE", P(spec, 0), P(spec, 5));
    }

    private string DecompileRedsp(byte[] spec)
    {
        // redsp: scrn(0) + specs(5,1byte)
        string scrn = P(spec, 0);
        if (!string.IsNullOrEmpty(scrn)) return Emit("REDSP", scrn);
        return "REDSP";
    }

    private string DecompileSaveScrn(byte[] spec)
    {
        // save_scrn: screen(0) + scope(5,1byte)
        string scrn = P(spec, 0);
        if (!string.IsNullOrEmpty(scrn)) return Emit("SAVES", scrn);
        return "SAVES";
    }

    private string DecompileMount(byte[] spec)
    {
        // mount: fmt(0) + typ(5,1byte) + wtp(6,1byte) + rfile(7) + sve_to(12) + winform(17,1byte)
        byte typ = Flag(spec, 5);
        string t = typ switch { (byte)'S' => "SCREEN", (byte)'R' => "REPORT", (byte)'B' => "REPORT", _ => "" };

        // Collect the constant offset for screen data extraction
        if (spec.Length >= 5 && spec[0] == 0)
        {
            int fmtLoc = BitConverter.ToInt32(spec, 1);
            if (fmtLoc >= 0 && fmtLoc + 4 <= _run.ConstantSegment.Length)
            {
                int screenOffset = BitConverter.ToInt32(_run.ConstantSegment, fmtLoc);
                if (screenOffset > 0 && screenOffset < _run.ConstantSegment.Length)
                {
                    char mt = typ == 0 ? 'S' : (char)typ;
                    _screenOffsets.Add(new MountInfo(screenOffset, mt, spec));
                }
            }
        }

        string wtp = "";
        byte wtpByte = Flag(spec, 6);
        if (wtpByte != 0 && wtpByte != ' ')
            wtp = ((char)wtpByte).ToString();

        return Emit("MOUNT", t, wtp, P(spec, 7), P(spec, 12));
    }

    // ===== Menu / List Commands =====

    private string DecompileMenu(byte[] spec, string name)
    {
        // menu: flds(0) + col(5) + row(10) + msg(15) + width(20) + chcs(25) + chose(30) + help(35) + ttl_w(40) + ttlfld(45) + cntr(50) + box(55) + shad_w(60) + hold(65,1byte) + array(66,1byte) + mcolor(67) + ccolor(72) + scolor(77) + larrow_lbl(82,4byte) + rarrow_lbl(86,4byte) + length(90) + bcolor(95) + ...
        var parts = new List<string>();
        parts.Add(P(spec, 0));  // field/choices list
        parts.Add(P(spec, 5));  // col
        parts.Add(P(spec, 10)); // row
        parts.Add(P(spec, 20)); // width
        string chcs = P(spec, 25);
        if (!string.IsNullOrEmpty(chcs)) parts.Add(chcs);
        string msg = P(spec, 15);
        if (!string.IsNullOrEmpty(msg)) parts.Add(msg);
        string chose = P(spec, 30);
        if (!string.IsNullOrEmpty(chose)) parts.Add(chose);
        string help = P(spec, 35);
        if (!string.IsNullOrEmpty(help)) parts.Add(help);
        string ttl = P(spec, 45);
        if (!string.IsNullOrEmpty(ttl)) parts.Add(ttl);
        string cntr = P(spec, 50);
        if (!string.IsNullOrEmpty(cntr)) parts.Add(cntr);
        return Emit(name, parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileListfm(byte[] spec, string name)
    {
        // act_wind: lst(0) + cntr(5) + actv(10) + chse(15) + rnd(20,1byte) + menu(21,1byte) + oth(22) + srch(27) + fhndl(32) + fkey(37) + for(42) + while(47) + start(52) + no_wait(57,1byte) + enter(58) + no_add(63,1byte) + lnes(64) + hlp(69) + cc(74) + up(79,1byte) + on_mve(80) + styp(85) + list_end(90,1byte) + fline(91) + cbf(96) + blnes(101) + noshift(106,1byte) + ec(107)
        var parts = new List<string>();
        parts.Add(P(spec, 0)); // list field
        parts.Add(P(spec, 5)); // counter
        string actv = P(spec, 10);
        if (!string.IsNullOrEmpty(actv)) parts.Add(actv);
        string chse = P(spec, 15);
        if (!string.IsNullOrEmpty(chse)) parts.Add(chse);
        string fhndl = P(spec, 32);
        if (!string.IsNullOrEmpty(fhndl)) parts.Add(fhndl);
        string fkey = P(spec, 37);
        if (!string.IsNullOrEmpty(fkey)) parts.Add(fkey);
        string forP = P(spec, 42);
        if (!string.IsNullOrEmpty(forP)) parts.Add(forP);
        string whileP = P(spec, 47);
        if (!string.IsNullOrEmpty(whileP)) parts.Add(whileP);
        string startP = P(spec, 52);
        if (!string.IsNullOrEmpty(startP)) parts.Add(startP);
        string hlp = P(spec, 69);
        if (!string.IsNullOrEmpty(hlp)) parts.Add(hlp);
        string cc = P(spec, 74);
        if (!string.IsNullOrEmpty(cc)) parts.Add(cc);
        return Emit(name, parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileRdlist(byte[] spec, string name)
    {
        // dlist: flst(0) + title(5) + hndl(10) + key(15) + strt(20) + scope(25,1byte) + sval(26) + for(31) + while(36) + cntr(41) + numb(46) + memory(51,1byte) + pwhr(52,1byte) + prt_f(53) + no_ff(58,1byte)
        var parts = new List<string>();
        parts.Add(P(spec, 0)); // field list
        string title = P(spec, 5);
        if (!string.IsNullOrEmpty(title)) parts.Add(title);
        parts.Add(P(spec, 10)); // handle
        string key = P(spec, 15);
        if (!string.IsNullOrEmpty(key)) parts.Add(key);
        string strt = P(spec, 20);
        if (!string.IsNullOrEmpty(strt)) parts.Add(strt);
        return Emit(name, parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileCalcRecs(byte[] spec, string name)
    {
        // calc_recs: from(0) + to(5) + hndl(10) + key(15) + strt(20) + scope(25,1byte) + sval(26) + for(31) + while(36) + cntr(41) + numb(46) + memory(51,1byte) + display(52,1byte)
        return Emit(name, P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 20));
    }

    // ===== Print Control =====

    private string DecompilePon(byte[] spec)
    {
        // prt_to: file(0) + wch(5,1byte)
        byte wch = Flag(spec, 5);
        string w = wch switch { (byte)'P' => "P", (byte)'S' => "S", (byte)'B' => "B", _ => "" };
        return Emit("PON", P(spec, 0), w);
    }

    // ===== Transfer / String =====

    private string DecompileXfer(byte[] spec)
    {
        // xfer: ffld(0) + tfld(5) + numchr(10) + fmem(15) + tmem(20) + rec_buff(25,1byte)
        return Emit("XFER", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 20));
    }

    private string DecompileSorta(byte[] spec)
    {
        // sort: fld(0) + num(5) + move(10) + cntr(15) + way(20,1byte)
        byte way = Flag(spec, 20);
        string w = way == (byte)'D' ? "DESC" : "";
        return Emit("SORTA", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), w);
    }

    // ===== Peek / Poke =====

    private string DecompilePeekPoke(byte[] spec, string name)
    {
        // pk: loc_ofs(0) + fld(5) + nchr(10)
        return Emit(name, P(spec, 0), P(spec, 5), P(spec, 10));
    }

    // ===== Chain / Run =====

    private string DecompileRunPrg(byte[] spec)
    {
        // run_prg: nme(0) + tail(5)
        return Emit("RUN", P(spec, 0), P(spec, 5));
    }

    // ===== RAP =====

    private string DecompileRap(byte[] spec)
    {
        // rap: name(0) + num(5) + in_mem(10,1byte) + with(11) + no_base_wind(16,1byte) + new_runtime(17,1byte) + no_delete(18,1byte) + no_save(19,1byte)
        return Emit("RAP", P(spec, 0), P(spec, 5), P(spec, 11));
    }

    // ===== Clock / Trace / Sound / Error =====

    private string DecompileClock(byte[] spec)
    {
        // clock: on_off(0,1byte) + col(1) + row(6) + mil(11,1byte)
        byte onOff = Flag(spec, 0);
        string state = onOff switch { (byte)'Y' => "ON", (byte)'N' => "OFF", _ => "" };
        return Emit("CLOCK", state, P(spec, 1), P(spec, 6));
    }

    private string DecompileTrace(byte[] spec)
    {
        // trace: do_what(0,1byte) + fw(1)
        byte dw = Flag(spec, 0);
        string what = dw switch { (byte)'Y' => "ON", (byte)'N' => "OFF", (byte)'S' => "STEP", _ => "" };
        return Emit("TRACE", what, P(spec, 1));
    }

    private string DecompileSound(byte[] spec)
    {
        // snd: note_ary(0) + beat_ary(5) + max_notes(10) + wav_file(15)
        return Emit("SOUND", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15));
    }

    private string DecompileErr(byte[] spec)
    {
        // error: fld(0) + num(5)
        return Emit("ERR", P(spec, 0), P(spec, 5));
    }

    // ===== GETLBL / UPAR =====

    private string DecompileGetlbl(byte[] spec)
    {
        // get_lbl: loc(0,4byte label) + fld(4)
        return Emit("GETLBL", Lbl4(spec, 0), P(spec, 4));
    }

    private string DecompileUpar(byte[] spec)
    {
        // up: udf(0) + goto_lbl(5,4byte)
        string udf = P(spec, 0);
        string lbl = Lbl4(spec, 5);
        if (!string.IsNullOrEmpty(lbl)) return Emit("UPAR", udf, lbl);
        return Emit("UPAR", udf);
    }

    // ===== Import / Export =====

    private string DecompileImpExp(byte[] spec, string name)
    {
        // Same as calc_recs layout + extensions at offset 53+
        var parts = new List<string>();
        parts.Add(P(spec, 0)); // from
        parts.Add(P(spec, 5)); // to
        parts.Add(P(spec, 10)); // handle
        string ftyp = P(spec, 53);
        if (!string.IsNullOrEmpty(ftyp)) parts.Add(ftyp);
        string dchr = P(spec, 58);
        if (!string.IsNullOrEmpty(dchr)) parts.Add(dchr);
        return Emit(name, parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    // ===== Button / Hot Spot / Load Picture =====

    private string DecompileButton(byte[] spec)
    {
        // button: col(0) + row(5) + len(10) + wdt(15) + color(20) + caption(25) + txtcolor(30) + key(35) + remove(40,1byte) + save_to(41) + restore_from(46) + off(51,1byte) + on(52,1byte) + using(53)
        var parts = new List<string>();
        parts.Add(P(spec, 0));  // col
        parts.Add(P(spec, 5));  // row
        parts.Add(P(spec, 10)); // length
        parts.Add(P(spec, 15)); // width
        string caption = P(spec, 25);
        if (!string.IsNullOrEmpty(caption)) parts.Add(caption);
        string key = P(spec, 35);
        if (!string.IsNullOrEmpty(key)) parts.Add(key);
        string color = P(spec, 20);
        if (!string.IsNullOrEmpty(color)) parts.Add(color);
        return Emit("BUTTON", parts.Where(p => !string.IsNullOrEmpty(p)).ToArray());
    }

    private string DecompileHotSpot(byte[] spec)
    {
        // hot: col(0) + row(5) + len(10) + wdt(15) + hndl(20) + key(25) + remove(30,1byte)
        return Emit("HOT_SPOT", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15), P(spec, 20), P(spec, 25));
    }

    private string DecompileLoadPicture(byte[] spec)
    {
        // load_pict: obj(0) + file(5) + field(10)
        return Emit("LOAD_PICTURE", P(spec, 0), P(spec, 5), P(spec, 10));
    }

    // ===== Mouse =====

    private string DecompileMouse(byte[] spec)
    {
        // mouse: fn(0) + cc(5) + lok(10,1byte) + err_lbl(11,4byte) + owner(15) + path(20) + fd(25) + on_off(30,1byte)
        return Emit("MOUSE", P(spec, 0), P(spec, 5), P(spec, 15));
    }

    // ===== Equation Variants =====

    private string DecompileEquMid(byte[] spec)
    {
        // equ_mid: recv_loc(0) + fld(5) + start(10) + size(15) + mem(20)
        return Emit("EQU_MID", P(spec, 0), P(spec, 5), P(spec, 10), P(spec, 15));
    }

    // ===== Generic: Walk spec bytes sequentially =====

    private string DecompileGeneric(string name, byte[] spec)
    {
        if (spec.Length == 0) return name;

        var parts = new List<string>();
        int pos = 0;

        while (pos < spec.Length)
        {
            byte b = spec[pos];

            // Check if this is a type-prefix param (5 bytes)
            if (IsParamType((char)b) && pos + 5 <= spec.Length)
            {
                string val = _spec.ResolveSpecParam(spec, pos);
                if (!string.IsNullOrEmpty(val))
                    parts.Add(val);
                pos += 5;
            }
            else if (b == 0x00)
            {
                // Null byte: padding or terminator, skip
                pos++;
            }
            else if (b >= 0x20 && b < 0x7F && !IsParamType((char)b))
            {
                // ASCII flag byte (single char like 'Y', 'N', etc.)
                // Only add if it seems meaningful
                char ch = (char)b;
                if (ch == 'Y' || ch == 'N' || ch == 'W' || ch == 'S' ||
                    ch == 'T' || ch == 'B' || ch == 'L' || ch == 'R' ||
                    ch == 'A' || ch == 'P' || ch == 'H' || ch == 'V')
                    parts.Add($"{ch}");
                pos++;
            }
            else
            {
                // Unknown byte, skip
                pos++;
            }
        }

        if (parts.Count == 0) return name;
        return $"{name} {string.Join(", ", parts)}";
    }

    /// <summary>
    /// Check if a byte is a known spec parameter type prefix.
    /// </summary>
    private static bool IsParamType(char c)
    {
        return c switch
        {
            'F' or 'C' or 'N' or 'X' or 'x' or 'Y' or 'M' or 'q' or 's' => true,
            'G' or 'D' or 'S' or 'I' => true,
            _ => false,
        };
    }

    // ===== Label Management =====

    private void BuildLabelMap()
    {
        // LabelOffsets[i] contains a code byte offset for label i.
        // Convert to instruction index (byte offset / 7 for TAS 5.1).
        int instrSize = _run.Header.ProType == "TAS32"
            ? RunFileHeader.Tas51InstructionSize
            : RunFileHeader.Tas60InstructionSize;

        for (int i = 0; i < _run.LabelOffsets.Count; i++)
        {
            int codeByteOffset = _run.LabelOffsets[i];
            if (codeByteOffset < 0) continue;
            int instrIndex = codeByteOffset / instrSize;
            if (!_labelsByInstrIndex.ContainsKey(instrIndex))
                _labelsByInstrIndex[instrIndex] = $"LABEL_{i}";
        }
    }

    /// <summary>
    /// Pre-scan all block constructs to determine where closing keywords should be emitted.
    /// Block IF/ELSE, FOR, SCAN, SELECT/CASE all store jump offsets at spec[0..3].
    /// Jump targets are combined code byte offsets (overlay + program), divided by
    /// instruction size and minus overlay instruction count to get RUN instruction index.
    ///
    /// IF's if_goto: direct code byte offset. ELSE sits at if_goto - 1 (end of true body).
    /// ELSE's abs_goto: INDIRECT - points to a spec segment offset that CONTAINS
    /// the actual code byte offset (r_abs_jmp reads from spec buffer then divides).
    ///
    /// SELECT/CASE: each CASE body ends with an ELSE that jumps (indirectly) to ENDC.
    /// These ELSE opcodes are suppressed in output.
    /// </summary>
    private void BuildEndifMap()
    {
        int instrSize = _run.Header.ProType == "TAS32"
            ? RunFileHeader.Tas51InstructionSize
            : RunFileHeader.Tas60InstructionSize;
        int ovlInstrs = _run.OverlayCodeSize / instrSize;
        int n = _run.Instructions.Count;

        // First pass: find SELECT blocks and their ENDC positions
        var selectRanges = new List<(int start, int endc)>();
        for (int i = 0; i < n; i++)
        {
            if (_run.Instructions[i].CommandNumber != TasOpcode.SELECT) continue;

            // First ELSE after SELECT jumps (indirectly) to ENDC
            for (int j = i + 1; j < n; j++)
            {
                if (_run.Instructions[j].CommandNumber != TasOpcode.ELSE) continue;
                int encPos = ResolveElseGoto(j, instrSize, ovlInstrs);
                if (encPos > 0 && encPos <= n)
                {
                    selectRanges.Add((i, encPos));
                    AddClosing(encPos, "ENDC");
                }
                break;
            }
        }

        // Mark ELSE opcodes that jump to ENDC as suppressed
        foreach (var (start, endc) in selectRanges)
        {
            for (int j = start + 1; j < Math.Min(endc, n); j++)
            {
                if (_run.Instructions[j].CommandNumber != TasOpcode.ELSE) continue;
                int target = ResolveElseGoto(j, instrSize, ovlInstrs);
                if (target == endc)
                    _suppressedElse.Add(j);
            }
        }

        // Second pass: handle IF, FOR, SCAN
        for (int i = 0; i < n; i++)
        {
            var instr = _run.Instructions[i];
            byte[] spec = _spec.GetSpecBytes(instr);

            switch (instr.CommandNumber)
            {
                case TasOpcode.IF:
                {
                    if (spec.Length < 16 || spec[15] != (byte)'D') break;
                    int ifGoto = BitConverter.ToInt32(spec, 0) / instrSize - ovlInstrs;
                    if (ifGoto <= 0 || ifGoto > n) break;

                    // ELSE sits at ifGoto - 1 (end of true body, before else body)
                    int elsePos = ifGoto - 1;
                    if (elsePos > i && elsePos < n &&
                        _run.Instructions[elsePos].CommandNumber == TasOpcode.ELSE &&
                        !_suppressedElse.Contains(elsePos))
                    {
                        // ELSE jumps indirectly to ENDIF position
                        int endifPos = ResolveElseGoto(elsePos, instrSize, ovlInstrs);
                        if (endifPos > 0 && endifPos <= n)
                            AddClosing(endifPos, "ENDIF");
                    }
                    else
                    {
                        // No ELSE - if_goto IS the ENDIF position
                        AddClosing(ifGoto, "ENDIF");
                    }
                    break;
                }
                case TasOpcode.FOR:
                {
                    if (spec.Length < 4) break;
                    int endLine = BitConverter.ToInt32(spec, 0) / instrSize - ovlInstrs;
                    if (endLine > 0 && endLine <= n)
                        AddClosing(endLine, "NEXT");
                    break;
                }
                case TasOpcode.SCAN:
                {
                    if (spec.Length < 4) break;
                    int endLine = BitConverter.ToInt32(spec, 0) / instrSize - ovlInstrs;
                    if (endLine > 0 && endLine <= n)
                        AddClosing(endLine, "ENDS");
                    break;
                }
            }
        }
    }

    /// <summary>
    /// Resolve an ELSE instruction's indirect jump target.
    /// ELSE stores a spec segment offset at spec[0..3]. The actual code byte offset
    /// is read from the spec segment at that location.
    /// </summary>
    private int ResolveElseGoto(int instrIndex, int instrSize, int ovlInstrs)
    {
        byte[] spec = _spec.GetSpecBytes(_run.Instructions[instrIndex]);
        if (spec.Length < 4) return -1;

        int specOffset = BitConverter.ToInt32(spec, 0);
        if (specOffset < 0 || specOffset + 4 > _run.SpecSegment.Length) return -1;

        int codeByteOffset = BitConverter.ToInt32(_run.SpecSegment, specOffset);
        return codeByteOffset / instrSize - ovlInstrs;
    }

    private void AddClosing(int line, string keyword)
    {
        if (!_closingKeywords.TryGetValue(line, out var list))
        {
            list = new List<string>();
            _closingKeywords[line] = list;
        }
        list.Add(keyword);
    }

    /// <summary>
    /// Get label name by label INDEX (used by GOTO, GOSUB, IF GOTO, TRAP, ON).
    /// Control flow stores label indices, not code byte offsets.
    /// </summary>
    private string LabelName(int labelIndex)
    {
        if (labelIndex < 0 || labelIndex == unchecked((int)0xFFFFFFFF))
            return "";
        if (labelIndex <= _run.LabelOffsets.Count)
            return $"LABEL_{labelIndex}";
        return $"LABELX_{labelIndex}";
    }

    // ===== Indentation =====

    private void AdjustIndentBefore(ushort op)
    {
        switch (op)
        {
            case TasOpcode.ELSE:
            case TasOpcode.ELSE_IF:
            case TasOpcode.ENDW:
            case TasOpcode.OTHERWISE:
            case TasOpcode.BRACE_CLOSE:
                if (_indent > 0) _indent--;
                break;
            case TasOpcode.CASE:
                // Close previous CASE body (drop from body level to case level)
                if (_indent > 0) _indent--;
                break;
        }
    }

    private void AdjustIndentAfter(ushort op)
    {
        switch (op)
        {
            case TasOpcode.IF:
                if (_lastIfIsBlock) _indent++;
                break;
            case TasOpcode.ELSE:
            case TasOpcode.ELSE_IF:
            case TasOpcode.WHILE:
            case TasOpcode.FOR:
            case TasOpcode.SELECT:
            case TasOpcode.CASE:
            case TasOpcode.OTHERWISE:
            case TasOpcode.SCAN:
            case TasOpcode.BRACE_OPEN:
                _indent++;
                break;
        }
    }

    // ===== Field Definitions =====

    private void EmitFieldDefinitions(StringBuilder sb)
    {
        foreach (var field in _run.Fields)
        {
            if (string.IsNullOrWhiteSpace(field.Name)) continue;
            if (field.Name.StartsWith("TEMP")) continue;
            if (field.Name.Any(c => c < ' ' || c > '~')) continue;

            string typeName = field.FieldType switch
            {
                'A' => "A",
                'N' => $"N({field.Decimals})",
                'I' => "I",
                'D' => "D",
                'L' => "L",
                'V' => "V",
                'T' => "T",
                _ => field.FieldType.ToString(),
            };

            string line;
            if (field.ArrayCount > 1)
                line = $"DEFINE {field.Name}, {typeName}, {field.DisplaySize}, {field.ArrayCount}";
            else
                line = $"DEFINE {field.Name}, {typeName}, {field.DisplaySize}";

            // Emit file field metadata for round-trip fidelity
            if (field.FileFieldIndex > 0)
                line += $" FILEFLD {field.FileBufferNumber},{field.KeyNumber},{field.Offset},{field.FileFieldIndex}";

            // Emit ForceUpperCase flag if set
            if (field.ForceUpperCase)
                line += " UPPER";

            sb.AppendLine(line);
        }

        if (_run.Fields.Count > 0) sb.AppendLine();
    }

    // ===== Screen/Report Format Decoder =====

    /// <summary>
    /// Collect unique file buffer names and their field counts from format fields.
    /// Returns ordered list of (bufferName, fieldCount) for the \FILES section.
    /// </summary>
    private List<(string Name, int Count)> CollectFileBuffers(byte[] cs, int startPos, int endPos)
    {
        const byte F_MARK = (byte)'F' + 128;
        const byte B_MARK = (byte)'B' + 128;
        const byte C_MARK = (byte)'C' + 128;
        int fldSpecSize = _run.Header.FieldSpecSize;
        var seen = new Dictionary<string, int>();
        var order = new List<string>();

        int p = startPos;
        while (p < endPos)
        {
            byte marker = cs[p];
            if (marker == 0) break;
            if (marker == F_MARK)
            {
                p++;
                while (p + 16 <= cs.Length && cs[p] != 0)
                {
                    char fldTyp = (char)cs[p];
                    int fldLoc = BitConverter.ToInt32(cs, p + 1);
                    if (fldTyp == 'F' && fldSpecSize > 0)
                    {
                        int idx = fldLoc / fldSpecSize;
                        if (idx >= 0 && idx < _run.Fields.Count)
                        {
                            var fs = _run.Fields[idx];
                            if (fs.IsFileField && fs.FileFieldIndex > 0)
                            {
                                int bufIdx = fs.FileFieldIndex - 1;
                                if (bufIdx >= 0 && bufIdx < _run.Buffers.Count)
                                {
                                    string bn = _run.Buffers[bufIdx].Name;
                                    if (!seen.ContainsKey(bn)) { seen[bn] = 0; order.Add(bn); }
                                    seen[bn]++;
                                }
                            }
                        }
                    }
                    p += 16;
                }
                if (p < cs.Length && cs[p] == 0) p++;
            }
            else if (marker == B_MARK)
            {
                p++;
                while (p < endPos && cs[p] != 0) { p++; }
                if (p < endPos && cs[p] == 0) p++;
            }
            else if (marker == C_MARK)
            {
                p++;
                while (p < endPos && cs[p] != 0) p += 5;
                if (p < endPos && cs[p] == 0) p++;
            }
            else break;
        }
        return order.Select(n => (n, seen[n])).ToList();
    }

    /// <summary>
    /// Count the number of text lines in a report format's B_MARK section.
    /// Report text lines are prefixed with 4-byte pointers; terminates on prefix=0.
    /// </summary>
    private int CountReportTextLines(byte[] cs, int startPos, int endPos)
    {
        const byte B_MARK = (byte)'B' + 128;
        int p = startPos;
        if (p >= endPos || cs[p] != B_MARK) return 0;
        p++; // skip B_MARK
        int count = 0;
        while (p + 4 < endPos)
        {
            int nextPtr = BitConverter.ToInt32(cs, p);
            if (nextPtr == 0) break;
            p += 4;
            count++;
            while (p < endPos && cs[p] != 0x0D) p++;
            if (p < endPos) p++; // skip CR
        }
        return count;
    }

    /// <summary>
    /// Pre-scan the field section to find the most common file buffer name.
    /// Used to reconstruct the S5 header's SSREC/SDREC/SCREC record names.
    /// </summary>
    private string ScanPrimaryBuffer(byte[] cs, int startPos, int endPos)
    {
        var buffers = CollectFileBuffers(cs, startPos, endPos);
        if (buffers.Count == 0) return "";
        return buffers.OrderByDescending(b => b.Count).First().Name;
    }

    /// <summary>
    /// Decode a compiled screen/report format from the constant segment
    /// and emit it in TAS source format (\S5name, \COLOR, \FIELDS, \\).
    /// Old format (TAS32): [NumFlds:2][sections...]
    /// New format (TASWN): [SizeOfFormat:2][NumFlds:2][sections...]
    /// Section markers: 0xC2=text, 0xC6=fields, 0xC3=colors
    /// </summary>
    private void DecodeScreenFormat(StringBuilder sb, int offset, int screenNum, char mountType, byte[] mountSpec)
    {
        var cs = _run.ConstantSegment;
        if (offset < 0 || offset + 4 >= cs.Length) return;

        bool oldFormat = _run.Header.ProType.StartsWith("TAS32");
        int numScrnFlds;
        int pos;
        int end;
        bool isReport = mountType == 'B' || mountType == 'R';

        if (isReport)
        {
            // Reports have NO numflds/size prefix — data starts directly with section markers
            numScrnFlds = 0;
            pos = offset;
            end = Math.Min(offset + 6801, cs.Length);
        }
        else if (oldFormat)
        {
            numScrnFlds = BitConverter.ToUInt16(cs, offset);
            pos = offset + 2;
            end = Math.Min(offset + 6801, cs.Length);
        }
        else
        {
            int sizeOfFormat = BitConverter.ToUInt16(cs, offset);
            numScrnFlds = BitConverter.ToUInt16(cs, offset + 2);
            pos = offset + 4;
            end = Math.Min(offset + sizeOfFormat, cs.Length);
        }

        string prefix = isReport ? "\\B5" : "\\S5";
        sb.AppendLine($"{prefix}FORMAT{screenNum}");

        // Emit S5/B5 header line — reconstruct from field bindings and mount spec
        if (!isReport)
        {
            // Pre-scan fields to find primary file buffer
            string primaryBuf = ScanPrimaryBuffer(cs, pos, end);
            string recSuffix = string.IsNullOrEmpty(primaryBuf) ? "WORK" : primaryBuf;
            // Positional header (cols 1-83):
            // 1-2: indent, 3: mode, 4: edit, 5-6: spaces, 7: flag
            // 8-22: save rec (15 chars), 23-37: disp rec (15 chars), 38-52: chg rec (15 chars)
            // 53-66: include (14 chars), 67-80: key (14 chars), 81+: flags
            string saveRec = $"SSREC_{recSuffix}";
            string dispRec = $"SDREC_{recSuffix}";
            string chgRec  = $"SCREC_{recSuffix}";
            sb.AppendLine($"  1Y  Y{saveRec,-15}{dispRec,-15}{chgRec,-15}{"NONE",-14}{"NONE",-14}YNN");
        }
        else
        {
            // Report header: count text lines from the binary, use primary file buffer
            int rptLines = CountReportTextLines(cs, pos, end);
            string primaryBuf = ScanPrimaryBuffer(cs, pos, end);
            string rptFile = string.IsNullOrEmpty(primaryBuf) ? "" : primaryBuf;
            sb.AppendLine($" {rptLines,2} 80100 5 18{rptFile,-8}NNN!");
        }

        const byte B_MARK = (byte)'B' + 128; // 0xC2
        const byte F_MARK = (byte)'F' + 128; // 0xC6
        const byte C_MARK = (byte)'C' + 128; // 0xC3

        int fldSpecSize = _run.Header.FieldSpecSize;
        int fieldSectionStart = pos; // track for \FILES collection

        while (pos < end)
        {
            byte marker = cs[pos];
            if (marker == 0) break;

            if (marker == B_MARK)
            {
                pos++;
                if (isReport)
                {
                    // Report text: each line prefixed with 4-byte pointer to next line
                    // When prefix=0, that's the terminator — skip 4+1 bytes total
                    while (pos + 4 < cs.Length)
                    {
                        int nextPtr = BitConverter.ToInt32(cs, pos);
                        if (nextPtr == 0)
                        {
                            pos += 5; // skip null prefix + terminating CR
                            break;
                        }
                        pos += 4;
                        int lineStart = pos;
                        while (pos < cs.Length && cs[pos] != 0x0D && cs[pos] != 0)
                            pos++;
                        int lineLen = pos - lineStart;
                        var lineChars = new char[lineLen];
                        for (int j = 0; j < lineLen; j++)
                            lineChars[j] = (char)cs[lineStart + j];
                        sb.AppendLine(new string(lineChars));
                        if (pos < cs.Length && cs[pos] == 0x0D) pos++;
                    }
                }
                else
                {
                    // Screen text: plain lines terminated by 0x0D, section ends with 0x00
                    while (pos < cs.Length && cs[pos] != 0)
                    {
                        int lineStart = pos;
                        while (pos < cs.Length && cs[pos] != 0x0D && cs[pos] != 0)
                            pos++;
                        int lineLen = pos - lineStart;
                        var lineChars = new char[lineLen];
                        for (int j = 0; j < lineLen; j++)
                            lineChars[j] = (char)cs[lineStart + j];
                        sb.AppendLine(new string(lineChars));
                        if (pos < cs.Length && cs[pos] == 0x0D) pos++;
                    }
                    if (pos < cs.Length && cs[pos] == 0) pos++;
                }
            }
            else if (marker == C_MARK)
            {
                sb.AppendLine("\\COLOR");
                pos++;
                while (pos + 5 <= cs.Length && cs[pos] != 0)
                {
                    byte a = cs[pos], b = cs[pos + 1], c = cs[pos + 2], d = cs[pos + 3], e = cs[pos + 4];
                    // Positions are 0-based in binary; source uses 1-based
                    sb.AppendLine($" {a,2} {b + 1,2} {c + 1,2} {d + 1,2} {e + 1,2}");
                    pos += 5;
                }
                if (pos < cs.Length && cs[pos] == 0) pos++;
            }
            else if (marker == F_MARK)
            {
                sb.AppendLine("\\FIELDS");
                pos++;
                // TScrnFld: Typ(1) Loc(4) Col(1) Row(1) dcolor(1) picture(4) fld_offset(4) = 16 bytes
                while (pos + 16 <= cs.Length && cs[pos] != 0)
                {
                    char fldTyp = (char)cs[pos];
                    int fldLoc = BitConverter.ToInt32(cs, pos + 1);
                    byte fldCol = cs[pos + 5];
                    byte fldRow = cs[pos + 6];
                    byte fldDColor = cs[pos + 7];
                    // Col is 0-based in binary for both screens and reports; source uses 1-based
                    // Row is 0-based for screens (compiler does -1); for reports, row is XlatTable value (no -1)
                    int srcCol = fldCol + 1;
                    int srcRow = isReport ? fldRow : fldRow + 1;

                    string fldName;
                    string exprSuffix = "";

                    if (fldTyp == 'F' && fldSpecSize > 0)
                    {
                        int idx = fldLoc / fldSpecSize;
                        if (idx >= 0 && idx < _run.Fields.Count)
                            fldName = _run.Fields[idx].Name;
                        else
                            fldName = $"FIELD[{idx}]";
                    }
                    else if (fldTyp == 'X')
                    {
                        // Expression display field — name is lost at compile time
                        fldName = $"EXPR_FLD";
                        exprSuffix = $"==A{_spec.ResolveParam('X', fldLoc)}";
                    }
                    else
                        fldName = $"{fldTyp}:{fldLoc}";

                    // Resolve field metadata from the field spec list
                    RunFieldSpec? fldSpec = null;
                    if (fldTyp == 'F' && fldSpecSize > 0)
                    {
                        int idx = fldLoc / fldSpecSize;
                        if (idx >= 0 && idx < _run.Fields.Count)
                            fldSpec = _run.Fields[idx];
                    }

                    // Source format: NAME  grp col row dc ? dec size ? ?TYPE+UC+RW+G+FILE  NN  isize  [==Aexpr]
                    int arrCount = fldSpec?.ArrayCount ?? 0;
                    char typeChar = fldSpec?.FieldType ?? (fldTyp == 'X' ? 'X' : '?');
                    string ucFlag = fldSpec != null ? (fldSpec.ForceUpperCase ? "Y" : "N") : "N";
                    string rwFlag = fldSpec != null ? (fldSpec.IsFileField ? "R" : "N") : "X";
                    int dsize = fldSpec?.DisplaySize ?? 0;
                    int dec = fldSpec?.Decimals ?? 0;
                    // Trailing size: screens repeat dsize, reports use 0 for isize column
                    int trailingSize = isReport ? 0 : dsize;

                    // Resolve buffer name from FileFieldIndex (1-based buffer index)
                    string bufName = "MEMORY";
                    if (fldSpec != null && fldSpec.IsFileField && fldSpec.FileFieldIndex > 0)
                    {
                        int bufIdx = fldSpec.FileFieldIndex - 1;
                        if (bufIdx >= 0 && bufIdx < _run.Buffers.Count)
                            bufName = _run.Buffers[bufIdx].Name;
                    }
                    // NN flag: present for memory fields and non-key file fields
                    string nnFlag = fldSpec != null && !fldSpec.IsFileField ? "NN" : 
                                    (fldSpec != null && fldSpec.IsFileField && fldSpec.KeyNumber == 0 ? "NN" : "  ");

                    if (isReport)
                        sb.Append($"{fldName,-18}{srcCol,3}{srcRow,3}{fldDColor,3} {dec}{dsize,7}{arrCount,5}{trailingSize,5}{typeChar}{ucFlag,6}{rwFlag}{bufName,-45}R");
                    else
                        sb.Append($"{fldName,-17}{arrCount,1} {srcCol,2} {srcRow,2} {fldDColor,2}  0 {dec} {dsize,4}    0    {arrCount}{typeChar}{ucFlag}{rwFlag}G{bufName,-8}{nnFlag} {trailingSize,5}");
                    if (!string.IsNullOrEmpty(exprSuffix))
                        sb.Append($"        {exprSuffix}");
                    sb.AppendLine();
                    pos += 16;
                }
                if (pos < cs.Length && cs[pos] == 0) pos++;
            }
            else
            {
                // Unknown marker - skip to end
                break;
            }
        }
        // Emit structural sections that the compiler expects
        if (!isReport)
        {
            // Collect file references from fields for \FILES section
            var fileBuffers = CollectFileBuffers(cs, fieldSectionStart, end);
            sb.AppendLine("\\FILES");
            foreach (var (name, count) in fileBuffers)
                sb.AppendLine($"{name}{name} {count,2}");
            sb.AppendLine("\\INCLUDE");
            sb.AppendLine("\\KEY");
        }
        sb.AppendLine("\\\\");
    }
}
