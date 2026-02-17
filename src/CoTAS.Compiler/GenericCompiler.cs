// Table-driven command compiler — replaces 54 hand-coded Emit* methods.
// Mirrors the Pascal compiler's CompSpecLine/DoComps architecture.

using CoTAS.Parser;
using CoTAS.Parser.Ast;

namespace CoTAS.Compiler;

/// <summary>
/// Generic table-driven command compiler. Uses CommandTable to look up
/// command definitions and compile them using a small set of primitives.
/// </summary>
public sealed class GenericCompiler
{
    private readonly SpecBuilder _spec;
    private readonly ConstantPool _constants;
    private readonly FieldTable _fields;
    private readonly LabelTable _labels;
    private readonly Func<ExpressionEncoder> _createEncoder;
    private readonly Func<List<Token>, Expression> _parseTokensAsExpr;

    // Working state for current command
    private byte[] _specLine = null!;
    private List<Token> _tokens = null!;
    private int _nextPos;
    private CommandDef _currentCmd = default;

    /// <summary>Use TAS 5.1 (TAS32) spec sizes when true, TAS 6.0+ when false.</summary>
    public bool UseTas51Sizes { get; set; } = true;

    public GenericCompiler(
        SpecBuilder spec,
        ConstantPool constants,
        FieldTable fields,
        LabelTable labels,
        Func<ExpressionEncoder> createEncoder,
        Func<List<Token>, Expression> parseTokensAsExpr)
    {
        _spec = spec;
        _constants = constants;
        _fields = fields;
        _labels = labels;
        _createEncoder = createEncoder;
        _parseTokensAsExpr = parseTokensAsExpr;
    }

    /// <summary>
    /// Compile a generic command using the command table.
    /// Returns (specOffset, specSize) for EmitInstruction, or null if command not found.
    /// </summary>
    public (int SpecOffset, int SpecSize)? CompileCommand(GenericCommandStmt gen)
    {
        string name = gen.CommandName.ToUpperInvariant();
        if (!CommandTable.Commands.TryGetValue(name, out var cmd))
            return null;

        if (cmd.IsSpecial)
            return null; // Control flow commands handled separately

        return CompileSpecLine(cmd, gen.Tokens);
    }

    /// <summary>
    /// Compile a command's spec line using its table definition.
    /// Mirrors Pascal's CompSpecLine.
    /// </summary>
    public (int SpecOffset, int SpecSize) CompileSpecLine(CommandDef cmd, List<Token> tokens)
    {
        int effectiveSize = cmd.GetSpecSize(UseTas51Sizes);
        _specLine = new byte[Math.Max(effectiveSize, 256)];
        _tokens = tokens;
        _nextPos = 0;
        _currentCmd = cmd;

        int specOff = _spec.BeginSpec();

        if (cmd.Options.Length > 0)
        {
            int optIdx = 0;

            // Step 1: If first option has OptionIndex=0, it's the positional (first) argument
            if (cmd.Options[0].OptionIndex == 0)
            {
                DoCompile(cmd.Options[0]);
                optIdx = 1;
            }

            // Step 2: Walk remaining tokens, matching option keywords
            while (_nextPos < _tokens.Count)
            {
                var tok = _tokens[_nextPos];
                if (tok.Type is TokenType.Comma or TokenType.Newline or TokenType.Eof
                    or TokenType.Semicolon)
                {
                    _nextPos++;
                    continue;
                }

                // Try to match token as an option keyword for THIS command
                var matchedOpt = FindOptionForToken(cmd, tok);
                if (matchedOpt.HasValue)
                {
                    _nextPos++; // consume the keyword
                    DoCompile(matchedOpt.Value);
                }
                else
                {
                    // Not a keyword — if we haven't consumed the first positional arg
                    // and there's an OptionIndex=0 entry, it was already handled above.
                    // Otherwise skip unrecognized tokens.
                    _nextPos++;
                }
            }
        }

        ApplyDefaults(cmd);

        if (effectiveSize > 0)
            _spec.WriteRaw(_specLine.AsSpan(0, effectiveSize));

        int specSize = _spec.GetSpecSize(specOff);
        return (specOff, specSize);
    }

    /// <summary>
    /// Find the option definition matching a token for the given command.
    /// Matches directly against the command's option list using OptionTable.Keywords names.
    /// </summary>
    private static OptionDef? FindOptionForToken(CommandDef cmd, Token tok)
    {
        string val = tok.Value?.ToUpperInvariant() ?? "";
        if (val.Length == 0) return null;

        for (int i = 0; i < cmd.Options.Length; i++)
        {
            int idx = cmd.Options[i].OptionIndex;
            if (idx > 0 && idx <= OptionTable.Keywords.Length &&
                string.Equals(OptionTable.Keywords[idx - 1], val, StringComparison.OrdinalIgnoreCase))
            {
                return cmd.Options[i];
            }
        }
        return null;
    }

    /// <summary>
    /// Execute a compilation primitive. Mirrors Pascal's DoComps.
    /// </summary>
    private void DoCompile(OptionDef opt)
    {
        switch (opt.Action)
        {
            case CompileAction.PE: // parse_expr — compile expression
                CompileExprAt(opt.SpecOffset);
                break;

            case CompileAction.PF: // parse_fexpr — compile field expression
                CompileFieldExprAt(opt.SpecOffset);
                break;

            case CompileAction.CF: // cmp_fld — compile field reference
                CompileFieldAt(opt.SpecOffset);
                break;

            case CompileAction.LL: // label_lookup
                CompileLabelAt(opt.SpecOffset);
                break;

            case CompileAction.CFile: // open file by name
            case CompileAction.CFile2: // file ref in other commands
                CompileFileAt(opt.SpecOffset);
                break;

            case CompileAction.ChkYn: // Y/N flag, default Y
                SetSpecChar(opt.SpecOffset, 'Y');
                break;

            case CompileAction.ChkNy: // Y/N flag, default N
                SetSpecChar(opt.SpecOffset, 'Y'); // presence of keyword means YES
                break;

            case CompileAction.ChkChr: // check character from list
                CompileCheckCharAt(opt.SpecOffset, opt.CharList);
                break;

            case CompileAction.ChkChr1: // set single char and consume keyword
                if (opt.CharList is { Length: > 0 })
                    SetSpecChar(opt.SpecOffset, opt.CharList[0]);
                _nextPos--; // Pascal does dec(NextPos) — keyword itself IS the value
                break;

            case CompileAction.ChkChr2: // set single char from next token
                CompileCheckChar2At(opt.SpecOffset);
                break;

            case CompileAction.CFAll: // compile field/const/expr all
                CompileFieldAllAt(opt.SpecOffset);
                break;

            case CompileAction.CFAllRfo: // compile field, real fields only
                CompileFieldAllAt(opt.SpecOffset);
                break;

            case CompileAction.CFAllNoExp: // compile field list, no expressions
                CompileFieldAllAt(opt.SpecOffset);
                break;

            case CompileAction.CAt: // compile AT col,row
                CompileAtAt(opt.SpecOffset);
                break;

            case CompileAction.SetY: // set 'Y'
                SetSpecChar(opt.SpecOffset, 'Y');
                _nextPos--; // keyword presence = flag set, no value consumed
                break;

            case CompileAction.SetN: // set 'N'
                SetSpecChar(opt.SpecOffset, 'N');
                _nextPos--; // keyword presence = flag set, no value consumed
                break;

            case CompileAction.CScope: // compile scope (A/R/N/F)
                CompileScopeAt(opt.SpecOffset);
                break;

            case CompileAction.CKey: // compile key reference
                CompileKeyAt(opt.SpecOffset);
                break;

            case CompileAction.CUfc: // use first char of option keyword
                CompileUfcAt(opt.SpecOffset);
                break;

            case CompileAction.CUfcCf: // use first char + compile following field
                CompileUfcCfAt(opt.SpecOffset);
                break;

            case CompileAction.IfLL: // if cmd_buf<>0 then label lookup
                CompileLabelAt(opt.SpecOffset);
                break;

            case CompileAction.LUdf: // label or UDF
                CompileLabelOrUdfAt(opt.SpecOffset);
                break;

            case CompileAction.ChkElbl: // check for NO_ERR label
                CompileNoErrLabelAt(opt.SpecOffset);
                break;

            case CompileAction.ChkTrap: // trap name
                CompileTrapAt(opt.SpecOffset);
                break;

            case CompileAction.ChkMem: // memory buffer
                CompileExprAt(opt.SpecOffset);
                break;

            case CompileAction.ChkFFld: // field or file in FIND
                CompileFileOrFieldAt(opt.SpecOffset);
                break;

            case CompileAction.ChkRelFld: // related field in FIND
                CompileFieldAt(opt.SpecOffset);
                break;

            case CompileAction.CTrace: // trace type
                CompileExprAt(opt.SpecOffset);
                break;

            case CompileAction.CFMount: // mount screen from WINDEF
                CompileExprAt(opt.SpecOffset);
                break;

            case CompileAction.CSchema: // schema name → buffer number
                CompileExprAt(opt.SpecOffset);
                break;

            case CompileAction.CTops: // trap options verbose
                CompileTrapOptionAt(opt);
                break;

            case CompileAction.CUpdaVal: // update array wtd+val
                CompileExprAt(opt.SpecOffset);
                break;

            case CompileAction.CSlvRel: // compile slave field (must be key)
                CompileFieldAt(opt.SpecOffset);
                break;

            case CompileAction.Cc1PE: // chk_chr1 + parse_expr
                if (opt.CharList is { Length: > 0 })
                    SetSpecChar(opt.SpecOffset, opt.CharList[0]);
                CompileExprAt(opt.SpecOffset + 5); // expr goes at next 5-byte position
                break;

            case CompileAction.ChkChrFt: // special find type with warning
                CompileCheckCharAt(opt.SpecOffset, opt.CharList);
                break;

            case CompileAction.CCaVal: // drop box clr/add value
                CompileExprAt(opt.SpecOffset);
                break;

            default:
                // Unknown action — skip
                break;
        }
    }

    /// <summary>Apply default values for unset spec positions.</summary>
    private void ApplyDefaults(CommandDef cmd)
    {
        foreach (var dflt in cmd.Defaults)
        {
            if (dflt.SpecOffset >= _specLine.Length || dflt.SpecOffset < 0)
                continue;
            // Only apply if not already set
            if (_specLine[dflt.SpecOffset] != 0)
                continue;

            switch (dflt.Action)
            {
                case CompileAction.ChkYn:
                    _specLine[dflt.SpecOffset] = (byte)'Y';
                    break;
                case CompileAction.ChkNy:
                    _specLine[dflt.SpecOffset] = (byte)'N';
                    break;
                case CompileAction.ChkChr:
                case CompileAction.ChkChr1:
                case CompileAction.ChkChrFt:
                    if (dflt.CharList is { Length: > 0 })
                        _specLine[dflt.SpecOffset] = (byte)dflt.CharList[0];
                    break;
            }
        }
    }

    // ════════════════════════════════════════════════════════════════════════
    //  Compilation Primitives — each writes a 5-byte TParseResult at offset
    // ════════════════════════════════════════════════════════════════════════

    /// <summary>Set a single character at a spec offset.</summary>
    private void SetSpecChar(int offset, char ch)
    {
        if (offset >= 0 && offset < _specLine.Length)
            _specLine[offset] = (byte)ch;
    }

    /// <summary>Write a 5-byte param (type + 4-byte location) into the spec line.</summary>
    private void WriteParamAt(int offset, char type, int location)
    {
        if (offset + 4 >= _specLine.Length) return;
        _specLine[offset] = (byte)type;
        _specLine[offset + 1] = (byte)(location & 0xFF);
        _specLine[offset + 2] = (byte)((location >> 8) & 0xFF);
        _specLine[offset + 3] = (byte)((location >> 16) & 0xFF);
        _specLine[offset + 4] = (byte)((location >> 24) & 0xFF);
    }

    /// <summary>Compile an expression and write at spec offset.</summary>
    private void CompileExprAt(int offset)
    {
        // Collect tokens until next keyword or end
        var exprTokens = CollectValueTokens();
        if (exprTokens.Count == 0)
        {
            WriteParamAt(offset, '#', 0); // null param
            return;
        }

        var expr = _parseTokensAsExpr(exprTokens);
        var encoder = _createEncoder();
        var (type, loc) = encoder.CompileToParam(expr);
        WriteParamAt(offset, type, loc);
    }

    /// <summary>Compile a field expression at spec offset (similar to expr but field-oriented).</summary>
    private void CompileFieldExprAt(int offset)
    {
        // Same as CompileExprAt — ExpressionEncoder handles field vs expr
        CompileExprAt(offset);
    }

    /// <summary>Compile a field reference at spec offset.</summary>
    private void CompileFieldAt(int offset)
    {
        if (_nextPos >= _tokens.Count) return;
        var tok = _tokens[_nextPos];

        // Handle @ prefix for indirect fields
        if (tok.Type == TokenType.AtSign && _nextPos + 1 < _tokens.Count)
        {
            _nextPos++;
            tok = _tokens[_nextPos];
            string fieldName = "@" + tok.Value;
            _nextPos++;
            int fidx = _fields.FindField(tok.Value);
            if (fidx >= 0)
                WriteParamAt(offset, 'M', _fields.GetFieldSpecOffset(fidx));
            else
            {
                int co = _constants.AddString(tok.Value);
                WriteParamAt(offset, 'M', co);
            }
            return;
        }

        if (tok.Type == TokenType.Identifier)
        {
            string name = tok.Value;
            _nextPos++;
            int fieldIdx = _fields.FindField(name);
            if (fieldIdx >= 0)
                WriteParamAt(offset, 'F', _fields.GetFieldSpecOffset(fieldIdx));
            else
            {
                // Not a known field — treat as string constant
                int constOff = _constants.AddString(name);
                WriteParamAt(offset, 'C', constOff);
            }
        }
        else
        {
            // Fall back to expression compilation
            CompileExprAt(offset);
        }
    }

    /// <summary>
    /// Compile field/const/expr list. Pascal CmpFldAll: compiles each comma-separated
    /// part as an expression, packs all TParseResults into a blob saved to the constant
    /// pool with a null terminator, and writes a C reference to the blob in the spec.
    /// </summary>
    private void CompileFieldAllAt(int offset)
    {
        // Collect ALL tokens until next keyword or end (including commas)
        var allTokens = CollectAllValueTokens();
        if (allTokens.Count == 0)
        {
            WriteParamAt(offset, '#', 0);
            return;
        }

        // Split on commas and compile each part separately
        var parts = new List<(char Type, int Loc)>();
        var partTokens = new List<Token>();
        foreach (var tok in allTokens)
        {
            if (tok.Type == TokenType.Comma)
            {
                if (partTokens.Count > 0)
                {
                    var expr = _parseTokensAsExpr(partTokens);
                    var enc = _createEncoder();
                    parts.Add(enc.CompileToParam(expr));
                    partTokens.Clear();
                }
            }
            else
                partTokens.Add(tok);
        }
        if (partTokens.Count > 0)
        {
            var expr = _parseTokensAsExpr(partTokens);
            var enc = _createEncoder();
            parts.Add(enc.CompileToParam(expr));
        }

        // Pack into blob: each part is 5 bytes (type + int32 loc) + null terminator
        var blob = new byte[parts.Count * 5 + 1];
        for (int i = 0; i < parts.Count; i++)
        {
            blob[i * 5] = (byte)parts[i].Type;
            BitConverter.TryWriteBytes(blob.AsSpan(i * 5 + 1), parts[i].Loc);
        }
        // blob[parts.Count * 5] is already 0 (null terminator)

        int constOff = _constants.AddRaw(new byte[] { (byte)'A', 0, (byte)(blob.Length & 0xFF), (byte)((blob.Length >> 8) & 0xFF) });
        _constants.AddRaw(blob);
        WriteParamAt(offset, 'C', constOff);
    }

    /// <summary>
    /// Collect ALL tokens (including commas) until the next keyword or end.
    /// Used by c_f_all which needs to split comma-separated expressions itself.
    /// </summary>
    private List<Token> CollectAllValueTokens()
    {
        var result = new List<Token>();
        int parenDepth = 0;

        // Skip leading whitespace
        while (_nextPos < _tokens.Count &&
               _tokens[_nextPos].Type is TokenType.Semicolon)
            _nextPos++;

        while (_nextPos < _tokens.Count)
        {
            var tok = _tokens[_nextPos];
            if (tok.Type is TokenType.Eof or TokenType.Newline) break;

            if (tok.Type == TokenType.LeftParen) parenDepth++;
            if (tok.Type == TokenType.RightParen) parenDepth--;

            if (parenDepth == 0 && FindOptionForToken(_currentCmd, tok).HasValue)
                break;

            result.Add(tok);
            _nextPos++;
        }
        return result;
    }

    /// <summary>
    /// Compile trap option (GOTO/GOSUB/IGNR/DFLT/RET_FALSE/RET_TRUE).
    /// Pascal CompTrapOp: the keyword determines the action char at SpecPos,
    /// and GOTO/GOSUB also compile a label at SpecPos+1.
    /// </summary>
    private void CompileTrapOptionAt(OptionDef opt)
    {
        // The keyword was already consumed. Map option index to action char.
        string keyword = OptionTable.Keywords[opt.OptionIndex - 1];
        char actionChar = keyword.ToUpperInvariant() switch
        {
            "GOTO" => 'G',
            "GOSUB" => 'S',
            "IGNR" => 'I',
            "DFLT" => 'D',
            "RET_FALSE" => 'F',
            "RET_TRUE" => 'T',
            _ => 'D'
        };
        SetSpecChar(opt.SpecOffset, actionChar);

        // GOTO and GOSUB need a label reference at offset+1
        if (actionChar is 'G' or 'S')
        {
            if (_nextPos < _tokens.Count)
            {
                var tok = _tokens[_nextPos];
                _nextPos++;
                int labelIdx = _labels.GetOrCreateRef(tok.Value);
                // Write label index as 4 bytes at SpecOffset+1
                BitConverter.TryWriteBytes(_specLine.AsSpan(opt.SpecOffset + 1), labelIdx);
            }
        }
    }

    /// <summary>Compile a label reference at spec offset.</summary>
    private void CompileLabelAt(int offset)
    {
        if (_nextPos >= _tokens.Count) return;
        var tok = _tokens[_nextPos];
        _nextPos++;

        int labelIdx = _labels.FindLabel(tok.Value);
        if (labelIdx >= 0)
            WriteParamAt(offset, 'G', labelIdx);
        else
        {
            // Label not found — write as unresolved (will be patched later)
            // Use label name as string constant for now
            int co = _constants.AddString(tok.Value);
            WriteParamAt(offset, 'G', co);
        }
    }

    /// <summary>Compile a label or UDF reference.</summary>
    private void CompileLabelOrUdfAt(int offset)
    {
        if (_nextPos >= _tokens.Count) return;
        var tok = _tokens[_nextPos];

        // Check for @ prefix (UDF indicator)
        if (tok.Type == TokenType.AtSign && _nextPos + 1 < _tokens.Count)
        {
            _nextPos++; // skip @
            tok = _tokens[_nextPos];
            _nextPos++;
            // Treat as UDF name — compile as string constant
            int co = _constants.AddString(tok.Value);
            WriteParamAt(offset, 'S', co);
            return;
        }

        // Otherwise treat as label
        CompileLabelAt(offset);
    }

    /// <summary>Compile NO_ERR label or error label reference.</summary>
    private void CompileNoErrLabelAt(int offset)
    {
        if (_nextPos >= _tokens.Count) return;
        var tok = _tokens[_nextPos];

        if (tok.Value.Equals("NO_ERR", StringComparison.OrdinalIgnoreCase))
        {
            // Special: NO_ERR means suppress error display
            SetSpecChar(offset, 'E');
            _nextPos++;
            return;
        }

        // Otherwise it's a label
        CompileLabelAt(offset);
    }

    /// <summary>Compile file reference (by name or number).</summary>
    private void CompileFileAt(int offset)
    {
        if (_nextPos >= _tokens.Count) return;
        var tok = _tokens[_nextPos];

        // Handle @filenum syntax
        if (tok.Type == TokenType.AtSign && _nextPos + 1 < _tokens.Count)
        {
            _nextPos++; // skip @
            CompileExprAt(offset);
            return;
        }

        // File name — compile as expression (could be string or field)
        CompileExprAt(offset);
    }

    /// <summary>Compile file or field (FIND command ambiguity).</summary>
    private void CompileFileOrFieldAt(int offset)
    {
        CompileExprAt(offset);
    }

    /// <summary>Compile AT col,row — two values at consecutive 5-byte positions.</summary>
    private void CompileAtAt(int offset)
    {
        // col at offset, row at offset+5
        CompileExprAt(offset);
        // Check for comma or semicolon separator
        if (_nextPos < _tokens.Count && 
            _tokens[_nextPos].Type is TokenType.Comma or TokenType.Semicolon)
            _nextPos++;
        CompileExprAt(offset + 5);
    }

    /// <summary>Compile scope option (A/R/N/F + optional value).</summary>
    private void CompileScopeAt(int offset)
    {
        if (_nextPos >= _tokens.Count) return;
        var tok = _tokens[_nextPos];
        string upper = tok.Value.ToUpperInvariant();

        if (upper.Length >= 1 && "ARNF".Contains(upper[0]))
        {
            SetSpecChar(offset, upper[0]);
            _nextPos++;
            // N and F can have a following value expression
            if ((upper[0] == 'N' || upper[0] == 'F') && _nextPos < _tokens.Count)
            {
                CompileExprAt(offset + 1);
            }
        }
        else
        {
            // Not a scope char — might be an expression
            CompileExprAt(offset + 1);
        }
    }

    /// <summary>Compile key reference.</summary>
    private void CompileKeyAt(int offset)
    {
        CompileExprAt(offset);
    }

    /// <summary>Compile check char — match next token against valid char list.</summary>
    private void CompileCheckCharAt(int offset, string? charList)
    {
        if (_nextPos >= _tokens.Count) return;
        var tok = _tokens[_nextPos];
        string upper = tok.Value.ToUpperInvariant();

        if (charList != null && upper.Length >= 1 && charList.Contains(upper[0]))
        {
            SetSpecChar(offset, upper[0]);
            _nextPos++;
        }
        else
        {
            // Default to first char in list
            if (charList is { Length: > 0 })
                SetSpecChar(offset, charList[0]);
            // Don't consume token — it might be something else
        }
    }

    /// <summary>Compile chk_chr2 — parse next token as a single char.</summary>
    private void CompileCheckChar2At(int offset)
    {
        if (_nextPos >= _tokens.Count) return;
        var tok = _tokens[_nextPos];
        if (tok.Value.Length >= 1)
            SetSpecChar(offset, tok.Value[0]);
        _nextPos++;
    }

    /// <summary>Use first char of the option keyword (already consumed).</summary>
    private void CompileUfcAt(int offset)
    {
        // The keyword that matched is at _nextPos - 1 (already consumed by caller)
        if (_nextPos > 0 && _nextPos - 1 < _tokens.Count)
        {
            var keyword = _tokens[_nextPos - 1].Value;
            if (keyword.Length > 0)
                SetSpecChar(offset, char.ToUpper(keyword[0]));
        }
    }

    /// <summary>Use first char + compile following field.</summary>
    private void CompileUfcCfAt(int offset)
    {
        CompileUfcAt(offset);
        CompileFieldAt(offset + 1); // field at next position
    }

    /// <summary>Compile trap name reference.</summary>
    private void CompileTrapAt(int offset)
    {
        // Collect trap key names (comma-separated)
        var exprTokens = CollectValueTokens();
        if (exprTokens.Count == 0)
        {
            WriteParamAt(offset, '#', 0);
            return;
        }

        // Convert key names to byte codes
        var keyCodes = new List<byte>();
        foreach (var tok in exprTokens)
        {
            if (tok.Type == TokenType.Comma) continue;
            byte code = GetTrapKeyCode(tok.Value);
            if (code != 0) keyCodes.Add(code);
        }

        // Save as blob: key bytes + null terminator, wrapped in A header
        var blob = new byte[keyCodes.Count + 1]; // +1 null terminator
        for (int i = 0; i < keyCodes.Count; i++)
            blob[i] = keyCodes[i];

        int constOff = _constants.AddRaw(new byte[] { (byte)'A', 0, (byte)(blob.Length & 0xFF), (byte)((blob.Length >> 8) & 0xFF) });
        _constants.AddRaw(blob);
        WriteParamAt(offset, 'C', constOff);
    }

    private static byte GetTrapKeyCode(string name) => name.ToUpperInvariant() switch
    {
        "F1" => 0x01, "F2" => 0x02, "F3" => 0x03, "F4" => 0x04, "F5" => 0x05,
        "F6" => 0x06, "F7" => 0x07, "F8" => 0x08, "F9" => 0x09, "F10" => 0x0A,
        "ESC" => 0x0B, "INT" => 0x0C, "T_ESC" => 0x0D,
        "UPAR" => 0x0E, "DNAR" => 0x0F, "LT_A" => 0x10, "RT_A" => 0x11,
        "LT_A_AS" => 0x12, "RT_A_AS" => 0x13, "HOME" => 0x14, "END" => 0x15,
        "PG_UP" => 0x16, "PG_DN" => 0x17, "INSRT" => 0x18, "DEL_KEY" => 0x19,
        "WD_LT" => 0x1A, "WD_RT" => 0x1B, "TAB" => 0x1C, "BCK_TAB" => 0x1D,
        "RSRCH" => 0x1E, "L_EXIT" => 0x1F, "RLCK" => 0x20, "FERR" => 0x21,
        "PERR" => 0x22, "PG_BRK" => 0x23,
        "SF1" => 0x24, "SF2" => 0x25, "SF3" => 0x26, "SF4" => 0x27, "SF5" => 0x28,
        "SF6" => 0x29, "SF7" => 0x2A, "SF8" => 0x2B, "SF9" => 0x2C, "SF10" => 0x2D,
        "CTL_F1" => 0x2E, "CTL_F2" => 0x2F, "CTL_F3" => 0x30, "CTL_F4" => 0x31,
        "CTL_F5" => 0x32, "CTL_F6" => 0x33, "CTL_F7" => 0x34, "CTL_F8" => 0x35,
        "CTL_F9" => 0x36, "CTL_F10" => 0x37,
        "CTL_PG_UP" => 0x38, "CTL_PG_DN" => 0x39,
        "ALT_F1" => 0x3A, "ALT_F2" => 0x3B, "ALT_F3" => 0x3C,
        "MOUSE_MOV" => 0x56, "MOUSE_LBD" => 0x57,
        _ => 0
    };

    // ════════════════════════════════════════════════════════════════════════
    //  Token Collection Helpers
    // ════════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Collect tokens for a value expression — everything until the next
    /// recognized option keyword or end of tokens.
    /// </summary>
    private List<Token> CollectValueTokens()
    {
        var result = new List<Token>();
        int parenDepth = 0;

        // Skip leading commas/whitespace before the value
        while (_nextPos < _tokens.Count &&
               _tokens[_nextPos].Type is TokenType.Comma or TokenType.Semicolon)
            _nextPos++;

        while (_nextPos < _tokens.Count)
        {
            var tok = _tokens[_nextPos];

            if (tok.Type is TokenType.Eof or TokenType.Newline)
                break;

            if (tok.Type == TokenType.LeftParen) parenDepth++;
            if (tok.Type == TokenType.RightParen) parenDepth--;

            if (parenDepth == 0)
            {
                if (tok.Type == TokenType.Comma)
                    break;
                if (FindOptionForToken(_currentCmd, tok).HasValue)
                    break;
            }

            result.Add(tok);
            _nextPos++;
        }

        return result;
    }
}
