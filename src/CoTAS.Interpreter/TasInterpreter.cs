using CoTAS.Parser;
using CoTAS.Parser.Ast;
using CoTAS.Interpreter.Commands;
using CoTAS.Storage;

namespace CoTAS.Interpreter;

public sealed class TasInterpreter
{
    private readonly IUIBridge _ui;
    private readonly FieldManager _fields = new();
    private readonly ExpressionEvaluator _eval;
    private readonly Dictionary<string, int> _labels = new(StringComparer.OrdinalIgnoreCase);
    private readonly Stack<int> _callStack = new();
    private readonly CommandRegistry _commands;
    private readonly TrapManager _traps = new();
    private readonly StorageEngine? _storage;

    // UDF/UDC definitions: name → (statementIndex, parameterNames)
    private readonly Dictionary<string, UdfDefinition> _udfs = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, UdfDefinition> _udcs = new(StringComparer.OrdinalIgnoreCase);

    // UDF return value (set by RET expr)
    private TasValue? _lastReturnValue;

    // Track whether we're inside a UDF call (for RET handling)
    private int _udfCallDepth;

    private List<Statement> _statements = [];
    private int _pc;
    private bool _halt;
    private bool _jumped; // set by goto/gosub/ret to signal PC was changed

    // Break/continue signals for loops
    private bool _breakRequested;
    private bool _continueRequested;

    public TasInterpreter(IUIBridge ui) : this(ui, CommandRegistry.CreateDefault()) { }

    public TasInterpreter(IUIBridge ui, CommandRegistry commands, StorageEngine? storage = null)
    {
        _ui = ui;
        _commands = commands;
        _storage = storage;
        _eval = new ExpressionEvaluator(_fields);
        // Wire up UDF callback so expression evaluator can call user-defined functions
        _eval.UdfCallback = CallUdf;
    }

    /// <summary>Expose FieldManager for testing.</summary>
    public FieldManager Fields => _fields;

    public async Task ExecuteAsync(TasProgram program)
    {
        _statements = program.Statements;
        _pc = 0;
        _halt = false;

        // First pass: collect labels and UDF/UDC definitions
        CollectLabelsAndDefinitions();

        // Execution loop
        while (_pc < _statements.Count && !_halt)
        {
            _jumped = false;
            await RunStatementAsync(_statements[_pc]);
            if (!_jumped && !_halt)
                _pc++;
        }
    }

    /// <summary>
    /// First pass: collect labels, process all DEFINE statements, and detect FUNC/CMD definitions.
    /// In TAS, DEFINE is a compile-time directive — ALL fields are registered during compilation
    /// regardless of control flow (gotos, etc.). This matches TAS compiler behavior where
    /// "each field used in any command or expression is checked to make sure it has been defined"
    /// at compile time, not at runtime.
    /// </summary>
    private void CollectLabelsAndDefinitions()
    {
        for (int i = 0; i < _statements.Count; i++)
        {
            if (_statements[i] is LabelStmt label)
            {
                _labels[label.Name] = i;
            }
            else if (_statements[i] is DefineStmt def)
            {
                // Process DEFINE at compile time — register all fields unconditionally
                ExecuteDefine(def);
            }
            else if (_statements[i] is GenericCommandStmt cmd)
            {
                if (cmd.CommandName.Equals("FUNC", StringComparison.OrdinalIgnoreCase) && cmd.Tokens.Count >= 1)
                {
                    string name = cmd.Tokens[0].Value;
                    var paramNames = new List<string>();
                    for (int t = 1; t < cmd.Tokens.Count; t++)
                    {
                        string tv = cmd.Tokens[t].Value;
                        if (tv != "," && !string.IsNullOrWhiteSpace(tv))
                            paramNames.Add(tv);
                    }
                    _udfs[name] = new UdfDefinition(name, i, paramNames);
                    // Also register as a label so GOSUB can reach it
                    _labels[name] = i;
                }
                else if (cmd.CommandName.Equals("CMD", StringComparison.OrdinalIgnoreCase) && cmd.Tokens.Count >= 1)
                {
                    string name = cmd.Tokens[0].Value;
                    var paramNames = new List<string>();
                    for (int t = 1; t < cmd.Tokens.Count; t++)
                    {
                        string tv = cmd.Tokens[t].Value;
                        if (tv != "," && !string.IsNullOrWhiteSpace(tv))
                            paramNames.Add(tv);
                    }
                    _udcs[name] = new UdfDefinition(name, i, paramNames);
                    // Also register as a label
                    _labels[name] = i;
                }
            }
        }
    }

    /// <summary>
    /// Called by ExpressionEvaluator when it encounters an unknown function.
    /// Checks if it's a UDF and executes it.
    /// </summary>
    private TasValue? CallUdf(string name, List<TasValue> args)
    {
        if (!_udfs.TryGetValue(name, out var udf))
            return null; // Not a UDF

        // Save current state
        int savedPc = _pc;
        var savedJumped = _jumped;
        _udfCallDepth++;

        // Set parameters
        var savedParamValues = new Dictionary<string, TasValue>();
        for (int i = 0; i < udf.Parameters.Count; i++)
        {
            string paramName = udf.Parameters[i];
            // Save old value if exists
            if (_fields.IsDefined(paramName))
                savedParamValues[paramName] = _fields.Get(paramName).Clone();
            // Set parameter to argument value
            if (i < args.Count)
                _fields.Set(paramName, args[i]);
        }

        // Push return address and jump to UDF body (statement after FUNC definition)
        _callStack.Push(savedPc + 1); // will be popped by RET
        _pc = udf.StatementIndex + 1; // skip the FUNC definition line itself
        _lastReturnValue = null;

        // Execute until RET or end
        try
        {
            while (_pc < _statements.Count && !_halt)
            {
                _jumped = false;
                var stmt = _statements[_pc];

                if (stmt is ReturnStmt ret)
                {
                    // Evaluate return expression if present
                    if (ret.ReturnValue != null)
                        _lastReturnValue = _eval.Evaluate(ret.ReturnValue);

                    // Pop the call stack
                    if (_callStack.Count > 0)
                        _callStack.Pop();
                    break;
                }

                // Execute the statement synchronously
                RunStatementAsync(stmt).GetAwaiter().GetResult();

                if (!_jumped && !_halt)
                    _pc++;
            }
        }
        finally
        {
            _udfCallDepth--;

            // Restore parameters
            foreach (var (paramName, savedVal) in savedParamValues)
                _fields.Set(paramName, savedVal);

            // Restore PC
            _pc = savedPc;
            _jumped = savedJumped;
        }

        return _lastReturnValue ?? new TasValue(TasType.Alpha, "", 0);
    }

    /// <summary>
    /// Execute a statement. Does NOT modify _pc unless it's a jump (goto/gosub/ret).
    /// The main loop handles advancing _pc.
    /// </summary>
    private async Task RunStatementAsync(Statement stmt)
    {
        switch (stmt)
        {
            case PreprocessorStmt:
            case LabelStmt:
                break;

            case DefineStmt def:
                ExecuteDefine(def);
                break;

            case AssignmentStmt assign:
                ExecuteAssignment(assign);
                break;

            case SayStmt say:
                await ExecuteSayAsync(say);
                break;

            case MessageStmt msg:
                await _ui.MessageAsync(_eval.Evaluate(msg.Text).AsString());
                break;

            case ClearScreenStmt:
                await _ui.ClearScreenAsync();
                break;

            case GotoStmt gto:
                if (!_labels.TryGetValue(gto.Label, out int gotoTarget))
                    throw new InterpreterException($"Undefined label: {gto.Label}");
                _pc = gotoTarget;
                _jumped = true;
                break;

            case GosubStmt gosub:
                if (!_labels.TryGetValue(gosub.Label, out int gosubTarget))
                    throw new InterpreterException($"Undefined label: {gosub.Label}");
                _callStack.Push(_pc + 1);
                _pc = gosubTarget;
                _jumped = true;
                break;

            case ReturnStmt ret:
                // Evaluate return value if present
                if (ret.ReturnValue != null)
                    _lastReturnValue = _eval.Evaluate(ret.ReturnValue);
                if (_callStack.Count == 0)
                {
                    _halt = true; // RET at top level = quit
                    return;
                }
                _pc = _callStack.Pop();
                _jumped = true;
                break;

            case IfThenStmt ifThen:
                if (_eval.Evaluate(ifThen.Condition).AsLogical())
                    await RunStatementAsync(ifThen.ThenBranch);
                break;

            case IfBlockStmt ifBlock:
                await ExecuteIfBlockAsync(ifBlock);
                break;

            case WhileStmt whileStmt:
                await ExecuteWhileAsync(whileStmt);
                break;

            case ForStmt forStmt:
                await ExecuteForAsync(forStmt);
                break;

            case SelectStmt selectStmt:
                await ExecuteSelectAsync(selectStmt);
                break;

            case ScanStmt scanStmt:
                await ExecuteScanAsync(scanStmt);
                break;

            case ExitStmt:
                _breakRequested = true;
                break;

            case LoopStmt:
                _continueRequested = true;
                break;

            case QuitStmt:
                _halt = true;
                break;

            case ExpressionStmt exprStmt:
                _eval.Evaluate(exprStmt.Expr);
                break;

            case GenericCommandStmt cmd:
                await ExecuteGenericCommandAsync(cmd);
                break;
        }
    }

    private void ExecuteDefine(DefineStmt def)
    {
        foreach (var name in def.FieldNames)
        {
            _fields.Define(
                name,
                def.FieldType,
                def.Size ?? 10,
                def.Decimals ?? 0,
                def.ArraySize ?? 0);
        }
    }

    private void ExecuteAssignment(AssignmentStmt assign)
    {
        var value = _eval.Evaluate(assign.Value);
        if (assign.Index != null)
        {
            int index = (int)_eval.Evaluate(assign.Index).AsNumeric();
            _fields.SetArrayElement(assign.Target, index, value);
        }
        else
        {
            _fields.Set(assign.Target, value);
        }
    }

    private async Task ExecuteSayAsync(SayStmt say)
    {
        string text = _eval.Evaluate(say.Text).AsString();
        int row = say.Row != null ? (int)_eval.Evaluate(say.Row).AsNumeric() : 0;
        int col = say.Col != null ? (int)_eval.Evaluate(say.Col).AsNumeric() : 0;
        await _ui.SayAsync(text, row, col);
    }

    private async Task ExecuteIfBlockAsync(IfBlockStmt ifBlock)
    {
        var cond = _eval.Evaluate(ifBlock.Condition);
        var block = cond.AsLogical() ? ifBlock.ThenBlock : ifBlock.ElseBlock;

        if (block != null)
        {
            foreach (var s in block)
            {
                await RunStatementAsync(s);
                if (_halt || _jumped || _breakRequested || _continueRequested) return;
            }
        }
    }

    private async Task ExecuteWhileAsync(WhileStmt whileStmt)
    {
        int iterations = 0;
        const int maxIterations = 1_000_000;

        while (!_halt)
        {
            var cond = _eval.Evaluate(whileStmt.Condition);
            if (!cond.AsLogical()) break;

            foreach (var s in whileStmt.Body)
            {
                await RunStatementAsync(s);
                if (_halt || _jumped) return;
                if (_breakRequested || _continueRequested) break;
            }

            if (_breakRequested)
            {
                _breakRequested = false;
                break;
            }
            if (_continueRequested)
            {
                _continueRequested = false;
                // continue to next iteration
            }

            if (++iterations > maxIterations)
                throw new InterpreterException("While loop exceeded maximum iterations (infinite loop protection)");
        }
    }

    private async Task ExecuteForAsync(ForStmt forStmt)
    {
        double start = _eval.Evaluate(forStmt.Start).AsNumeric();
        double stop = _eval.Evaluate(forStmt.Stop).AsNumeric();
        double step = _eval.Evaluate(forStmt.Step).AsNumeric();

        if (step == 0)
            throw new InterpreterException("FOR loop step cannot be zero");

        // Initialize counter
        _fields.Set(forStmt.Counter, new TasValue(TasType.Numeric, start));

        int iterations = 0;
        const int maxIterations = 1_000_000;

        while (!_halt)
        {
            double current = _fields.Get(forStmt.Counter).AsNumeric();

            // Check termination condition
            if (step > 0 && current > stop) break;
            if (step < 0 && current < stop) break;

            foreach (var s in forStmt.Body)
            {
                await RunStatementAsync(s);
                if (_halt || _jumped) return;
                if (_breakRequested || _continueRequested) break;
            }

            if (_breakRequested)
            {
                _breakRequested = false;
                break;
            }
            if (_continueRequested)
            {
                _continueRequested = false;
            }

            // Increment counter
            double next = _fields.Get(forStmt.Counter).AsNumeric() + step;
            _fields.Set(forStmt.Counter, new TasValue(TasType.Numeric, next));

            if (++iterations > maxIterations)
                throw new InterpreterException("FOR loop exceeded maximum iterations (infinite loop protection)");
        }
    }

    private async Task ExecuteSelectAsync(SelectStmt selectStmt)
    {
        var selector = _eval.Evaluate(selectStmt.Selector);

        foreach (var (caseValue, body) in selectStmt.Cases)
        {
            bool match;
            if (caseValue == null)
            {
                // OTHERWISE case — always matches
                match = true;
            }
            else
            {
                var caseVal = _eval.Evaluate(caseValue);
                // Compare: string comparison if either is alpha, numeric otherwise
                if (selector.Type == TasType.Alpha || caseVal.Type == TasType.Alpha)
                    match = string.Equals(selector.AsString().TrimEnd(), caseVal.AsString().TrimEnd(), StringComparison.OrdinalIgnoreCase);
                else
                    match = selector.AsNumeric() == caseVal.AsNumeric();
            }

            if (match)
            {
                foreach (var s in body)
                {
                    await RunStatementAsync(s);
                    if (_halt || _jumped || _breakRequested || _continueRequested) return;
                }
                break; // Only execute first matching case
            }
        }
    }

    /// <summary>
    /// Execute a SCAN loop over file records.
    /// Syntax: SCAN @handle KEY keyname START startval [WHILE condition] [FOR condition] [NLOCK] [SCOPE R|G]
    /// The Options tokens contain the raw SCAN parameters; Body contains the loop statements.
    /// </summary>
    private async Task ExecuteScanAsync(ScanStmt scanStmt)
    {
        if (_storage == null)
            return; // No storage engine — skip SCAN

        var tokens = scanStmt.Options;

        // Parse SCAN options from tokens
        int fileNumber = 0;
        int keyIndex = 1;
        string? startKey = null;
        string? scopeKey = null;
        Expression? whileExpr = null;
        Expression? forExpr = null;

        for (int i = 0; i < tokens.Count; i++)
        {
            string tv = tokens[i].Value.ToUpperInvariant();

            // @handle — file handle reference (field containing file number)
            if (tv.StartsWith("@") && i == 0)
            {
                string handleField = tv[1..]; // strip @
                if (_fields.IsDefined(handleField))
                    fileNumber = (int)_fields.Get(handleField).AsNumeric();
                else if (int.TryParse(handleField, out var n))
                    fileNumber = n;
            }
            // FNUM n
            else if (tv == "FNUM" && i + 1 < tokens.Count)
            {
                i++;
                string fnumVal = tokens[i].Value;
                if (_fields.IsDefined(fnumVal))
                    fileNumber = (int)_fields.Get(fnumVal).AsNumeric();
                else
                    int.TryParse(fnumVal, out fileNumber);
            }
            // KEY keyname or KEY @n
            else if (tv == "KEY" && i + 1 < tokens.Count)
            {
                i++;
                string keyVal = tokens[i].Value;
                if (keyVal.StartsWith("@"))
                {
                    // KEY @indexnumber
                    if (int.TryParse(keyVal[1..], out var ki))
                        keyIndex = ki;
                }
                else if (_fields.IsDefined(keyVal))
                {
                    // KEY fieldname — resolve index from the field name
                    // For now, use index 1 (primary)
                    keyIndex = 1;
                }
                else if (int.TryParse(keyVal, out var ki2))
                {
                    keyIndex = ki2;
                }
            }
            // START value1[,value2,...] — collect start key values
            else if (tv == "START" && i + 1 < tokens.Count)
            {
                var startParts = new List<string>();
                i++;
                while (i < tokens.Count)
                {
                    string sv = tokens[i].Value.ToUpperInvariant();
                    if (sv == "WHILE" || sv == "FOR" || sv == "NLOCK" || sv == "SCOPE")
                        break;
                    if (sv != ",")
                    {
                        // Resolve field references for start key values
                        string val = tokens[i].Value;
                        if (_fields.IsDefined(val))
                            startParts.Add(_fields.Get(val).AsString().TrimEnd());
                        else
                            startParts.Add(val.Trim('\'', '"'));
                    }
                    i++;
                }
                i--; // back up so the outer loop re-reads the keyword
                startKey = string.Join("", startParts);
            }
            // WHILE — the rest until FOR/NLOCK/SCOPE is a boolean expression
            else if (tv == "WHILE")
            {
                // Collect tokens until we hit FOR, NLOCK, SCOPE, or end
                var exprTokens = new List<Token>();
                i++;
                while (i < tokens.Count)
                {
                    string wv = tokens[i].Value.ToUpperInvariant();
                    if (wv == "FOR" || wv == "NLOCK" || wv == "SCOPE")
                        break;
                    exprTokens.Add(tokens[i]);
                    i++;
                }
                i--;
                if (exprTokens.Count > 0)
                {
                    // Parse the expression
                    try
                    {
                        var parser = new TasParser(exprTokens);
                        whileExpr = parser.ParseExpressionPublic();
                    }
                    catch { /* ignore parse errors in WHILE clause */ }
                }
            }
            // FOR — filter expression
            else if (tv == "FOR")
            {
                var exprTokens = new List<Token>();
                i++;
                while (i < tokens.Count)
                {
                    string fv = tokens[i].Value.ToUpperInvariant();
                    if (fv == "WHILE" || fv == "NLOCK" || fv == "SCOPE")
                        break;
                    exprTokens.Add(tokens[i]);
                    i++;
                }
                i--;
                if (exprTokens.Count > 0)
                {
                    try
                    {
                        var parser = new TasParser(exprTokens);
                        forExpr = parser.ParseExpressionPublic();
                    }
                    catch { /* ignore parse errors in FOR clause */ }
                }
            }
            // SCOPE R|G
            else if (tv == "SCOPE" && i + 1 < tokens.Count)
            {
                i++;
                scopeKey = startKey; // scope uses the start key
            }
        }

        if (fileNumber == 0) return;

        FileHandle handle;
        try { handle = _storage.GetFile(fileNumber); }
        catch { return; } // file not open

        // Open a cursor for iteration
        Microsoft.Data.SqlClient.SqlDataReader? reader = null;
        try
        {
            reader = await _storage.OpenScanCursorAsync(handle, keyIndex, startKey, scopeKey);

            int iterations = 0;
            const int maxIterations = 1_000_000;

            while (await reader.ReadAsync() && !_halt)
            {
                // Load record into buffer and populate fields
                handle.Buffer.LoadFromReader(reader, handle.Schema);
                handle.HasRecord = true;
                handle.IsEof = false;
                handle.LastError = 0;
                FindvCommand.PopulateFields(_fields, handle);

                // Evaluate WHILE condition — if false, stop scanning
                if (whileExpr != null)
                {
                    try
                    {
                        if (!_eval.Evaluate(whileExpr).AsLogical())
                            break;
                    }
                    catch { break; }
                }

                // Evaluate FOR condition — if false, skip this record
                if (forExpr != null)
                {
                    try
                    {
                        if (!_eval.Evaluate(forExpr).AsLogical())
                        {
                            if (++iterations > maxIterations)
                                throw new InterpreterException("SCAN exceeded maximum iterations");
                            continue;
                        }
                    }
                    catch (InterpreterException) { throw; }
                    catch { continue; }
                }

                // Execute the body
                foreach (var s in scanStmt.Body)
                {
                    await RunStatementAsync(s);
                    if (_halt || _jumped) { reader.Close(); return; }
                    if (_breakRequested || _continueRequested) break;
                }

                if (_breakRequested)
                {
                    _breakRequested = false;
                    break;
                }
                if (_continueRequested)
                {
                    _continueRequested = false;
                }

                if (++iterations > maxIterations)
                    throw new InterpreterException("SCAN exceeded maximum iterations");
            }

            // EOF reached
            handle.IsEof = true;
            handle.HasRecord = false;
        }
        finally
        {
            if (reader != null && !reader.IsClosed)
                reader.Close();
        }
    }

    private async Task ExecuteGenericCommandAsync(GenericCommandStmt cmd)
    {
        // Skip FUNC/CMD definition lines — they're just markers
        if (cmd.CommandName.Equals("FUNC", StringComparison.OrdinalIgnoreCase) ||
            cmd.CommandName.Equals("CMD", StringComparison.OrdinalIgnoreCase))
        {
            // Skip past the body until RET is found
            SkipPastReturn();
            return;
        }

        // Check if it's a UDC (user-defined command)
        if (_udcs.TryGetValue(cmd.CommandName, out var udc))
        {
            await ExecuteUdcAsync(udc, cmd.Tokens);
            return;
        }

        // Check the command registry
        if (_commands.TryGetHandler(cmd.CommandName, out var handler))
        {
            var ctx = new CommandContext
            {
                Tokens = cmd.Tokens,
                Fields = _fields,
                Evaluator = _eval,
                UI = _ui,
                Traps = _traps,
                Line = cmd.Line,
            };
            await handler.ExecuteAsync(ctx);
            return;
        }

        // Check if it's a UDC that wasn't in the explicit registry
        // (some UDCs are defined with CMD but called without being in the _udcs dict
        //  because they're defined later in the program — check labels too)
        if (_labels.TryGetValue(cmd.CommandName, out _))
        {
            // Could be a label-based subroutine call without GOSUB
            // In TAS, UDCs can be called just by name
            // For safety, only treat as UDC if we have an explicit definition
        }

        // Unknown commands are silently ignored (common in TAS - many are display/environment commands)
    }

    /// <summary>
    /// Skip forward past the body of a FUNC/CMD definition to the RET statement.
    /// This is called when execution flows into a FUNC/CMD definition line
    /// (not called as a function/command).
    /// </summary>
    private void SkipPastReturn()
    {
        int depth = 0;
        while (_pc < _statements.Count - 1)
        {
            _pc++;
            var s = _statements[_pc];
            if (s is ReturnStmt)
            {
                // Found the matching RET - _pc will be incremented by the main loop
                return;
            }
            // Track nested FUNC/CMD to handle nested definitions
            if (s is GenericCommandStmt cmd &&
                (cmd.CommandName.Equals("FUNC", StringComparison.OrdinalIgnoreCase) ||
                 cmd.CommandName.Equals("CMD", StringComparison.OrdinalIgnoreCase)))
            {
                depth++;
            }
        }
        // If no RET found, just continue
    }

    /// <summary>
    /// Execute a User Defined Command by setting parameters and jumping to the definition.
    /// </summary>
    private Task ExecuteUdcAsync(UdfDefinition udc, List<Token> argTokens)
    {
        // Parse arguments from tokens (comma-separated values)
        var args = new List<TasValue>();
        var currentArg = new List<string>();

        foreach (var token in argTokens)
        {
            if (token.Value == ",")
            {
                if (currentArg.Count > 0)
                {
                    string argStr = string.Join(" ", currentArg);
                    args.Add(ParseArgValue(argStr));
                    currentArg.Clear();
                }
            }
            else
            {
                currentArg.Add(token.Value);
            }
        }
        if (currentArg.Count > 0)
        {
            string argStr = string.Join(" ", currentArg);
            args.Add(ParseArgValue(argStr));
        }

        // Set parameters
        for (int i = 0; i < udc.Parameters.Count && i < args.Count; i++)
        {
            _fields.Set(udc.Parameters[i], args[i]);
        }

        // GOSUB to the UDC body
        _callStack.Push(_pc + 1);
        _pc = udc.StatementIndex + 1; // skip the CMD definition line
        _jumped = true;
        return Task.CompletedTask;
    }

    private TasValue ParseArgValue(string s)
    {
        s = s.Trim();
        // Try as field reference first
        if (_fields.IsDefined(s))
            return _fields.Get(s);
        // String literal
        if (s.StartsWith("'") && s.EndsWith("'"))
            return new TasValue(TasType.Alpha, s[1..^1], s.Length - 2);
        if (s.StartsWith("\"") && s.EndsWith("\""))
            return new TasValue(TasType.Alpha, s[1..^1], s.Length - 2);
        // Numeric
        if (double.TryParse(s, out var d))
            return new TasValue(TasType.Numeric, d);
        // Logical
        if (s.Equals(".T.", StringComparison.OrdinalIgnoreCase))
            return new TasValue(TasType.Logical, true);
        if (s.Equals(".F.", StringComparison.OrdinalIgnoreCase))
            return new TasValue(TasType.Logical, false);
        // Default: treat as string
        return new TasValue(TasType.Alpha, s, s.Length);
    }
}

/// <summary>
/// Represents a User Defined Function or Command definition.
/// </summary>
internal sealed record UdfDefinition(string Name, int StatementIndex, List<string> Parameters);
