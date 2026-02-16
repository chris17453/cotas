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

    public RunFileDecompiler(RunFileReader run)
    {
        _run = run;
        _spec = new SpecDecoder(run);
        BuildLabelMap();
    }

    /// <summary>
    /// Decompile the entire .RUN file to TAS source code.
    /// </summary>
    public string Decompile()
    {
        var sb = new StringBuilder();

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
            if (instr.CommandNumber >= 0xFE00) // END marker
                break;

            // Emit label if this instruction is a label target
            if (_labelsByInstrIndex.TryGetValue(i, out var label))
                sb.AppendLine(label + ":");

            // Adjust indent BEFORE for closing constructs
            AdjustIndentBefore(instr.CommandNumber);

            string line = DecompileInstruction(instr, i);
            if (!string.IsNullOrEmpty(line))
            {
                sb.Append(new string(' ', _indent * 3));
                sb.AppendLine(line);
            }

            // Adjust indent AFTER for opening constructs
            AdjustIndentAfter(instr.CommandNumber);
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
        // CASE has label address(4) + values
        if (spec.Length < 4) return "CASE ???";

        var parts = new List<string>();
        int pos = 4;
        while (pos + 5 <= spec.Length)
        {
            char typ = (char)spec[pos];
            if (typ == '\0') break;
            string val = _spec.ResolveSpecParam(spec, pos);
            if (!string.IsNullOrEmpty(val))
                parts.Add(val);
            pos += 5;
        }
        return parts.Count > 0 ? $"CASE {string.Join(", ", parts)}" : "CASE";
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
        // Conditional: expr_typ(1) + expr_loc(4)
        if (spec.Length >= 5)
        {
            string expr = _spec.ResolveSpecParam(spec, 0);
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

        string keys = _spec.ResolveSpecParam(spec, 0);
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

        return $"TRAP {keys}, {action}";
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
            case TasOpcode.ENDIF:
            case TasOpcode.ENDW:
            case TasOpcode.NEXT:
            case TasOpcode.OTHERWISE:
            case TasOpcode.ENDC:
            case TasOpcode.ENDS:
            case TasOpcode.BRACE_CLOSE:
                if (_indent > 0) _indent--;
                break;
            case TasOpcode.CASE:
                // CASE stays at same level as SELECT
                break;
        }
    }

    private void AdjustIndentAfter(ushort op)
    {
        switch (op)
        {
            case TasOpcode.IF:
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

            if (field.ArrayCount > 1)
                sb.AppendLine($"DEFINE {field.Name}, {typeName}, {field.DisplaySize}, {field.ArrayCount}");
            else
                sb.AppendLine($"DEFINE {field.Name}, {typeName}, {field.DisplaySize}");
        }

        if (_run.Fields.Count > 0) sb.AppendLine();
    }
}
