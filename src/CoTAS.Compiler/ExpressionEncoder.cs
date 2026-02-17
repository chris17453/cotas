using CoTAS.Parser.Ast;

namespace CoTAS.Compiler;

/// <summary>
/// Compiles infix expression AST nodes to TAS RPN bytecode.
/// Reverse of ExpressionDecoder.
///
/// Expression format in constant segment:
///   type(1) + dec(1) + displaySize(2) + 0xFD(1) + tempBase(4) + [operations...] + 0xFF
///
/// Operation types:
///   0x00: binary op — 00 + operator(1) + lhs(5) + rhs(5) + result(4) = 16 bytes
///   0x01: 0-arg function — 01 + funcNum(2) + result(4) = 7 bytes
///   0x02: 1-arg function — 02 + funcNum(2) + arg(5) + result(4) = 12 bytes
///   0x03+: N-arg function — (N+1) + funcNum(2) + args(5*N) + result(4)
///   0x14+: UDF call — (argCount+20) + labelIdx(4) + tempBase(4) + args(5*N) + result(4)
///   0xB4: array access — B4 + field(5) + indexOffset(4) + result(4) = 14 bytes
///   0x0A-0x0F: standalone comparison — op(1) + lhsTemp(4) + rhsTemp(4) + resultTemp(4) = 13 bytes
/// </summary>
public sealed class ExpressionEncoder
{
    private readonly FieldTable _fields;
    private readonly ConstantPool _constants;
    private readonly LabelTable _labels;
    private readonly MemoryStream _ops = new();
    private int _nextTempOffset;
    private readonly int _tempBase;

    /// <summary>Overlay offset added to field spec offsets in expression operands.</summary>
    public int OverlayFieldOffset { get; set; }

    /// <summary>Overlay offset added to constant pool offsets in expression operands.</summary>
    public int OverlayConstOffset { get; set; }

    // Track what each temp offset holds (for referenced by later ops)
    private readonly Dictionary<int, char> _tempTypes = [];

    // Flag: true when compiling a node that is the left child of a '+' BinaryExpr
    private bool _insidePlusChain;

    /// <summary>Whether to use TAS 5.1 format (1-byte funcNum, UDF base=10) vs TAS 6.0+ (2-byte funcNum, UDF base=20).</summary>
    public bool IsTas51 { get; set; }

    public ExpressionEncoder(FieldTable fields, ConstantPool constants, LabelTable labels, int tempBase)
    {
        _fields = fields;
        _constants = constants;
        _labels = labels;
        _tempBase = tempBase;
        _nextTempOffset = tempBase;
    }

    /// <summary>
    /// Compile an expression AST node to RPN bytecode and add it to the constant pool.
    /// Returns the byte offset in the constant segment.
    /// </summary>
    public int Compile(Expression expr)
    {
        _ops.SetLength(0);
        _nextTempOffset = _tempBase;
        _tempTypes.Clear();

        // Compile the expression tree into RPN operations
        var result = CompileNode(expr);

        // Determine result type
        char resultType = InferType(expr);
        byte decimals = InferDecimals(expr);

        return _constants.AddExpression(resultType, decimals, _ops.ToArray(), _nextTempOffset);
    }

    /// <summary>
    /// Compile an expression and return a 5-byte spec param reference to it.
    /// If the expression is simple (field ref, literal, etc.) returns a direct param.
    /// If complex, compiles to RPN and returns an 'X' param.
    /// </summary>
    public (char Type, int Location) CompileToParam(Expression expr)
    {
        switch (expr)
        {
            case IdentifierExpr id:
            {
                // Indirect field reference: @name → param type 'M'
                if (id.Name.StartsWith("@"))
                {
                    string realName = id.Name.Substring(1);
                    int fieldIdx = _fields.FindField(realName);
                    if (fieldIdx >= 0)
                        return ('M', _fields.GetFieldSpecOffset(fieldIdx));
                    // Unknown indirect — treat as constant string
                    int constOff = _constants.AddString(realName);
                    return ('C', constOff);
                }

                int fIdx = _fields.FindField(id.Name);
                if (fIdx >= 0)
                    return ('F', _fields.GetFieldSpecOffset(fIdx));
                // Unknown identifier — treat as constant string
                int cOff = _constants.AddString(id.Name);
                return ('C', cOff);
            }

            case LiteralExpr lit:
                return CompileLiteral(lit);

            case ArrayAccessExpr arr:
            {
                // Handle FIELD[N] syntax from decompiler (direct field index)
                if (arr.Name.Equals("FIELD", StringComparison.OrdinalIgnoreCase) && arr.Index is LiteralExpr fldLit && fldLit.Value is int directIdx)
                    return ('F', directIdx * _fields.FieldSpecSize);

                // Handle @FIELD[N] — indirect field reference (macro), param type 'M'
                if (arr.Name.Equals("@FIELD", StringComparison.OrdinalIgnoreCase) && arr.Index is LiteralExpr mfldLit && mfldLit.Value is int macroIdx)
                    return ('M', macroIdx * _fields.FieldSpecSize);

                // Handle FIELD[N][M] — array element M of field N (from parser FIELD_IDX:N convention)
                if (arr.Name.StartsWith("FIELD_IDX:", StringComparison.OrdinalIgnoreCase))
                {
                    int fieldIndex = int.Parse(arr.Name.Substring(10));
                    int fieldOff = fieldIndex * _fields.FieldSpecSize;
                    var indexParam = CompileToParam(arr.Index);
                    int arrayRefOff = _constants.AddArrayRef(
                        (byte)'F', fieldOff,
                        (byte)indexParam.Type, indexParam.Location);
                    return ('Y', arrayRefOff);
                }

                // Handle @FIELD_IDX:N — indirect array (macro array), param type 'q'
                if (arr.Name.StartsWith("@FIELD_IDX:", StringComparison.OrdinalIgnoreCase))
                {
                    int fieldIndex = int.Parse(arr.Name.Substring(11));
                    return ('q', fieldIndex * _fields.FieldSpecSize);
                }

                int fieldIdx = _fields.FindField(arr.Name);
                if (fieldIdx < 0)
                    throw new InvalidOperationException($"Array field not found: {arr.Name}");

                var indexParam2 = CompileToParam(arr.Index);
                int arrayRefOff2 = _constants.AddArrayRef(
                    (byte)'F', _fields.GetFieldSpecOffset(fieldIdx),
                    (byte)indexParam2.Type, indexParam2.Location);
                return ('Y', arrayRefOff2);
            }

            case UnaryExpr { Operator: "-", Operand: LiteralExpr { Value: int iv } }:
                return ('N', -iv);
            case UnaryExpr { Operator: "-", Operand: LiteralExpr { Value: long lv } }:
                return ('N', (int)-lv);
            case UnaryExpr { Operator: "-", Operand: LiteralExpr { Value: double dv } }
                when dv == Math.Floor(dv) && -dv >= int.MinValue && -dv <= int.MaxValue:
                return ('N', (int)-dv);

            case BinaryExpr:
            case UnaryExpr:
            case FunctionCallExpr:
            {
                int exprOff = Compile(expr);
                return ('X', exprOff);
            }

            default:
                throw new InvalidOperationException($"Cannot compile expression type: {expr.GetType().Name}");
        }
    }

    private OperandRef CompileNode(Expression expr)
    {
        switch (expr)
        {
            case LiteralExpr lit:
            {
                var (type, loc) = CompileNodeLiteral(lit);
                return new OperandRef(type, loc);
            }

            case IdentifierExpr id:
            {
                // Indirect field reference: @name → param type 'M'
                if (id.Name.StartsWith("@"))
                {
                    string realName = id.Name.Substring(1);
                    int fIdx = _fields.FindField(realName);
                    if (fIdx >= 0)
                        return new OperandRef('M', _fields.GetFieldSpecOffset(fIdx));
                }

                int fieldIdx = _fields.FindField(id.Name);
                if (fieldIdx >= 0)
                    return new OperandRef('F', _fields.GetFieldSpecOffset(fieldIdx));
                int constOff = _constants.AddString(id.Name);
                return new OperandRef('C', constOff);
            }

            case ArrayAccessExpr arr:
            {
                // Handle FIELD[N] syntax from decompiler (direct field index)
                if (arr.Name.Equals("FIELD", StringComparison.OrdinalIgnoreCase) && arr.Index is LiteralExpr fldLit2 && fldLit2.Value is int directIdx2)
                    return new OperandRef('F', directIdx2 * _fields.FieldSpecSize);

                // Handle @FIELD[N] — indirect field reference (macro), param type 'M'
                if (arr.Name.Equals("@FIELD", StringComparison.OrdinalIgnoreCase) && arr.Index is LiteralExpr mfldLit2 && mfldLit2.Value is int macroIdx2)
                    return new OperandRef('M', macroIdx2 * _fields.FieldSpecSize);

                // Handle FIELD[N][M] — array element M of field N (FIELD_IDX:N convention)
                if (arr.Name.StartsWith("FIELD_IDX:", StringComparison.OrdinalIgnoreCase))
                {
                    int fldIndex = int.Parse(arr.Name.Substring(10));
                    int fldOff = fldIndex * _fields.FieldSpecSize;
                    var idxRef = CompileNode(arr.Index);
                    int resOff = AllocTemp();
                    _ops.WriteByte(0xB4);
                    WriteOperand(new OperandRef('F', fldOff));
                    WriteInt32(fldOff);
                    WriteInt32(resOff);
                    return new OperandRef('F', resOff);
                }

                // Handle @FIELD_IDX:N — indirect array (macro array), param type 'q'
                if (arr.Name.StartsWith("@FIELD_IDX:", StringComparison.OrdinalIgnoreCase))
                {
                    int fldIndex = int.Parse(arr.Name.Substring(11));
                    return new OperandRef('q', fldIndex * _fields.FieldSpecSize);
                }

                int fieldIdx = _fields.FindField(arr.Name);
                if (fieldIdx < 0)
                    throw new InvalidOperationException($"Array field not found: {arr.Name}");

                var indexRef = CompileNode(arr.Index);
                int resultOff = AllocTemp();

                // B4 + field(5) + indexOffset(4) + result(4) = 14 bytes
                _ops.WriteByte(0xB4);
                WriteOperand(new OperandRef('F', _fields.GetFieldSpecOffset(fieldIdx)));
                WriteInt32(_fields.GetFieldSpecOffset(indexRef.Location / _fields.FieldSpecSize < _fields.Count
                    ? indexRef.Location : 0));
                WriteInt32(resultOff);

                return new OperandRef('F', resultOff);
            }

            case UnaryExpr unary:
            {
                var operand = CompileNode(unary.Operand);
                byte opByte = unary.Operator.ToUpperInvariant() switch
                {
                    "NOT" or ".NOT." or ".N." => 0x0E,
                    "-" => 0x02, // negate: 0 - operand
                    _ => throw new InvalidOperationException($"Unknown unary operator: {unary.Operator}")
                };

                if (opByte == 0x02) // Negate: emit 0 - operand
                {
                    var zeroRef = new OperandRef('N', 0);
                    int resultOff = AllocTemp();
                    EmitBinaryOp(opByte, zeroRef, operand, resultOff);
                    return new OperandRef('F', resultOff);
                }
                else // NOT: binary op format — 00 0E const_zero operand result (16 bytes)
                {
                    // TAS encodes .NOT. X as BinaryOp(0x0E, const_zero, X, result)
                    // LHS is always the integer zero constant at pool offset 0
                    var zeroRef = new OperandRef('C', 0);
                    int resultOff = AllocTemp();
                    EmitBinaryOp(opByte, zeroRef, operand, resultOff);
                    return new OperandRef('F', resultOff);
                }
            }

            case BinaryExpr bin:
            {
                string normOp = bin.Operator.Trim().ToUpperInvariant();
                if (normOp is "AND" or ".AND." or ".A." or "OR" or ".OR." or ".O.")
                    return CompileLogicalOp(bin);

                // For + chains: inner + nodes use CONCAT (0x03), outermost uses ADD (0x01)
                bool savedFlag = _insidePlusChain;
                if (bin.Operator == "+")
                    _insidePlusChain = bin.Left is BinaryExpr lb && lb.Operator == "+";
                var lhs = CompileNode(bin.Left);
                _insidePlusChain = savedFlag;

                var rhs = CompileNode(bin.Right);
                byte opByte = GetBinaryOpByte(bin.Operator);

                // Inner (non-root) + uses CONCAT (0x03), outermost uses ADD (0x01)
                if (bin.Operator == "+" && _insidePlusChain)
                    opByte = 0x03;

                int resultOff = AllocTemp();
                EmitBinaryOp(opByte, lhs, rhs, resultOff);
                return new OperandRef('F', resultOff);
            }

            case FunctionCallExpr func:
            {
                return CompileFunctionCall(func);
            }

            default:
                throw new InvalidOperationException($"Cannot compile node: {expr.GetType().Name}");
        }
    }

    /// <summary>
    /// Compile AND/OR logical operations matching original TAS operation ordering.
    /// The original TAS compiler emits operations in precedence order:
    /// 1. Function calls and arithmetic (highest precedence leaves)
    /// 2. Comparisons
    /// 3. Logical operators
    /// This means for AND(EQ(MID,BK), EQ(field,ADD(B,X))):
    ///   MID first, then ADD, then EQ1, then EQ2, then AND.
    /// </summary>
    private OperandRef CompileLogicalOp(BinaryExpr andOr)
    {
        // Collect all comparison children
        var comparisons = new List<BinaryExpr>();
        CollectComparisonChildren(andOr, comparisons);

        if (comparisons.Count < 2)
        {
            // Not a simple AND/OR of comparisons — fall back to standard compilation
            var lhs = CompileNode(andOr.Left);
            var rhs = CompileNode(andOr.Right);
            byte opByte = GetBinaryOpByte(andOr.Operator);
            int resultOff = AllocTemp();
            EmitBinaryOp(opByte, lhs, rhs, resultOff);
            return new OperandRef('F', resultOff);
        }

        // Phase 1: compile all LHS operands left-to-right
        var lhsRefs = new OperandRef[comparisons.Count];
        var rhsRefs = new OperandRef[comparisons.Count];
        for (int i = 0; i < comparisons.Count; i++)
            lhsRefs[i] = CompileNode(comparisons[i].Left);
        // Phase 1b: compile all RHS operands right-to-left (matches original TAS ordering)
        for (int i = comparisons.Count - 1; i >= 0; i--)
            rhsRefs[i] = CompileNode(comparisons[i].Right);

        // Phase 2: emit comparisons
        var results = new List<OperandRef>();
        for (int i = 0; i < comparisons.Count; i++)
        {
            byte cmpOp = GetBinaryOpByte(comparisons[i].Operator);
            int resultOff = AllocTemp();
            EmitBinaryOp(cmpOp, lhsRefs[i], rhsRefs[i], resultOff);
            results.Add(new OperandRef('F', resultOff));
        }

        // Phase 3: chain AND/OR operations
        byte logOp = GetBinaryOpByte(andOr.Operator);
        var current = results[0];
        for (int i = 1; i < results.Count; i++)
        {
            int resultOff = AllocTemp();
            EmitBinaryOp(logOp, current, results[i], resultOff);
            current = new OperandRef('F', resultOff);
        }

        return current;
    }

    private void CollectComparisonChildren(BinaryExpr bin, List<BinaryExpr> result)
    {
        string normOp = bin.Operator.Trim().ToUpperInvariant();
        bool isLogical = normOp is "AND" or ".AND." or ".A." or "OR" or ".OR." or ".O.";
        if (!isLogical)
        {
            result.Add(bin);
            return;
        }
        if (bin.Left is BinaryExpr leftBin) CollectComparisonChildren(leftBin, result);
        else result.Add(new BinaryExpr(bin.Left, "=", new LiteralExpr(true, 0), 0)); // wrap non-binary
        if (bin.Right is BinaryExpr rightBin) CollectComparisonChildren(rightBin, result);
        else result.Add(new BinaryExpr(bin.Right, "=", new LiteralExpr(true, 0), 0));
    }

    private OperandRef CompileFunctionCall(FunctionCallExpr func)
    {
        // UDF call: opType = argCount + udfBase (TAS 5.1: +10, TAS 6.0+: +20)
        // Format: opType(1) + labelIdx(4) + tempBase(4) + args(5*N) + result(4)
        int labelIdx = _labels.FindLabel(func.Name);
        if (labelIdx >= 0)
        {
            var args = func.Arguments.Select(a => CompileNode(a)).ToList();
            int resultOff = AllocTemp();

            int udfBase = IsTas51 ? 10 : 20;
            _ops.WriteByte((byte)(args.Count + udfBase));
            WriteInt32(labelIdx);
            int tempCtrHldr = _nextTempOffset;
            WriteInt32(tempCtrHldr);
            foreach (var arg in args)
                WriteOperand(arg);
            WriteInt32(resultOff);

            return new OperandRef('F', resultOff);
        }

        // Built-in function
        byte funcNum = GetFunctionNumber(func.Name);
        int argCount = func.Arguments.Count;
        var compiledArgs = func.Arguments.Select(a => CompileNode(a)).ToList();
        int result = AllocTemp();

        // opType = argCount + 1 (e.g., 0-arg = 0x01, 1-arg = 0x02, etc.)
        _ops.WriteByte((byte)(argCount + 1));
        // Function number — 1 byte for TAS 5.1
        _ops.WriteByte(funcNum);
        foreach (var arg in compiledArgs)
            WriteOperand(arg);
        WriteInt32(result);

        return new OperandRef('F', result);
    }

    /// <summary>
    /// Check if a + chain contains any string literal, making it a string concatenation.
    /// </summary>
    private static bool IsStringConcatChain(BinaryExpr bin)
    {
        if (bin.Operator != "+") return false;
        foreach (var operand in FlattenAddChain(bin))
        {
            if (operand is LiteralExpr lit && lit.Value is string)
                return true;
        }
        return false;
    }

    /// <summary>
    /// Flatten a left-associative chain of + operations into a list of operands.
    /// ((A + B) + C) → [A, B, C]
    /// </summary>
    private static List<Expression> FlattenAddChain(BinaryExpr bin)
    {
        var result = new List<Expression>();
        void Collect(Expression expr)
        {
            if (expr is BinaryExpr b && b.Operator == "+")
            {
                Collect(b.Left);
                result.Add(b.Right);
            }
            else
            {
                result.Add(expr);
            }
        }
        Collect(bin);
        return result;
    }

    /// <summary>
    /// Compile a string concatenation chain right-to-left, matching TAS behavior.
    /// [A, B, C] → (B concat C) → temp, (A + temp) → result
    /// Uses 0x03 for the innermost pair, 0x01 for the rest.
    /// </summary>
    private OperandRef CompileConcatChainRightToLeft(List<Expression> operands)
    {
        // Start with the rightmost two operands
        int n = operands.Count;
        var right = CompileNode(operands[n - 1]);
        var secondRight = CompileNode(operands[n - 2]);
        int resultOff = AllocTemp();
        // Innermost pair uses 0x03 (string concat)
        EmitBinaryOp(0x03, secondRight, right, resultOff);
        var current = new OperandRef('F', resultOff);

        // Process remaining operands right-to-left
        for (int i = n - 3; i >= 0; i--)
        {
            var operand = CompileNode(operands[i]);
            resultOff = AllocTemp();
            // Outer operations use 0x01 (add)
            EmitBinaryOp(0x01, operand, current, resultOff);
            current = new OperandRef('F', resultOff);
        }

        return current;
    }

    private void EmitBinaryOp(byte opByte, OperandRef lhs, OperandRef rhs, int resultOff)
    {
        // 0x00 + operator(1) + lhs(5) + rhs(5) + result(4) = 16 bytes
        _ops.WriteByte(0x00);
        _ops.WriteByte(opByte);
        WriteOperand(lhs);
        WriteOperand(rhs);
        WriteInt32(resultOff);
    }

    private void WriteOperand(OperandRef op)
    {
        _ops.WriteByte((byte)op.Type);
        int loc = op.Location;
        if (op.Type == 'F') loc += OverlayFieldOffset;
        else if (op.Type == 'C' || op.Type == 'X' || op.Type == 'Y' || op.Type == 'M')
            loc += OverlayConstOffset;
        WriteInt32(loc);
    }

    private void WriteInt32(int value)
    {
        Span<byte> buf = stackalloc byte[4];
        BitConverter.TryWriteBytes(buf, value);
        _ops.Write(buf);
    }

    private int AllocTemp()
    {
        int off = _nextTempOffset;
        _nextTempOffset += _fields.FieldSpecSize; // Each temp is one field spec slot
        return off;
    }

    private (char Type, int Location) CompileLiteral(LiteralExpr lit)
    {
        switch (lit.Value)
        {
            case string s:
            {
                int off = _constants.AddString(s);
                return ('C', off);
            }
            case int i:
            {
                // Small integers use inline numeric type 'N'
                return ('N', i);
            }
            case long l:
            {
                return ('N', (int)l);
            }
            case double d:
            {
                // If it's a whole number, use N type
                if (d == Math.Floor(d) && d >= int.MinValue && d <= int.MaxValue)
                    return ('N', (int)d);
                int off = _constants.AddNumeric(d, 2);
                return ('C', off);
            }
            case bool b:
            {
                int off = _constants.AddLogical(b);
                return ('C', off);
            }
            default:
            {
                int off = _constants.AddString(lit.Value?.ToString() ?? "");
                return ('C', off);
            }
        }
    }

    /// <summary>
    /// Compile a literal for use inside expression RPN bytecode.
    /// Integer literals are stored in the constant pool (as I entries) and
    /// referenced via C type, matching the original TAS compiler behavior.
    /// </summary>
    private (char Type, int Location) CompileNodeLiteral(LiteralExpr lit)
    {
        switch (lit.Value)
        {
            case int i:
            {
                int off = _constants.AddInteger(i);
                return ('C', off);
            }
            case long l:
            {
                int off = _constants.AddInteger((int)l);
                return ('C', off);
            }
            case double d when d == Math.Floor(d) && d >= int.MinValue && d <= int.MaxValue:
            {
                int off = _constants.AddInteger((int)d);
                return ('C', off);
            }
            default:
                return CompileLiteral(lit);
        }
    }

    private static char InferType(Expression expr)
    {
        // TAS 5.1 always uses 'A' (alpha) for expression constant header type,
        // regardless of the actual result type. The interpreter determines types at runtime.
        return 'A';
    }

    private static byte InferDecimals(Expression expr)
    {
        return expr switch
        {
            LiteralExpr lit when lit.Value is double => 2,
            _ => 0
        };
    }

    private static bool IsComparisonOp(string op) =>
        op is "=" or "<>" or "<" or ">" or "<=" or ">=" or
             "and" or "or" or "not" or
             ".AND." or ".OR." or ".NOT." or
             ".A." or ".O." or ".N.";

    private static char InferFunctionReturnType(string funcName)
    {
        string upper = funcName.ToUpperInvariant();
        return upper switch
        {
            // String functions
            "LEN" or "ASC" or "FNUM" or "RCN" or "CREC" or "DOM" or "DOW" or "MNTH" or
            "YEAR" or "NUMFLDS" or "NUMLBLS" or "FLDFDNUM" => 'I',
            // Numeric functions
            "ABS" or "ROUND" or "SQRT" or "SIGN" or "MOD" or "CEIL" or "FLOOR" or
            "RNDM" or "PI" or "EXP" or "LOG" or "LOG10" or "SIN" or "COS" or "TAN" or
            "ASIN" or "ACOS" or "ATAN" or "ATAN2" or "VAL" or "INT" => 'N',
            // Logical functions
            "EOF" or "BOF" or "NULL" or "ISAL" or "ISUP" or "ISLO" or "ISNUM" or
            "FLERR" or "ESC" or "ENTER" or "ASK" or "LCKD" or "OPEN" or "AVAIL" or "PSTAT" => 'L',
            // Date/time functions
            "CTOD" or "RTOD" or "FLDATE" => 'D',
            "CTOT" or "RTOT" or "FLTIME" => 'T',
            // Default to alpha
            _ => 'A'
        };
    }

    /// <summary>
    /// Map binary operator string to TAS RPN operator byte.
    /// </summary>
    public static byte GetBinaryOpByte(string op)
    {
        string norm = op.Trim().ToUpperInvariant();
        return norm switch
        {
            "+" => 0x01,  // numeric add (TAS 5.1); string concat uses 0x03
            "-" => 0x02,
            "*" => 0x04,
            "/" => 0x05,
            "^" => 0x06,
            "=" => 0x07,
            "<" => 0x08,
            ">" => 0x09,
            "<>" => 0x0A,
            ">=" => 0x0B,
            "<=" => 0x0C,
            "AND" or ".AND." or ".A." => 0x0D,
            "NOT" or ".NOT." or ".N." => 0x0E,
            "OR" or ".OR." or ".O." => 0x0F,
            _ => throw new InvalidOperationException($"Unknown binary operator: {op}")
        };
    }

    /// <summary>
    /// Reverse lookup: function name → function number byte.
    /// Complete mapping of ALL TAS built-in functions.
    /// </summary>
    public static byte GetFunctionNumber(string name)
    {
        string upper = name.ToUpperInvariant();
        if (_functionNumbers.TryGetValue(upper, out byte num))
            return num;
        return 0; // Unknown function — will be treated as UDF
    }

    /// <summary>
    /// Infer the spec flag byte for a function's return type.
    /// 'S' = string/alpha, 'N' = numeric, 'L' = logical, 'D' = date, 'T' = time.
    /// </summary>
    public static char InferFunctionReturnTypeFlag(string funcName)
    {
        char t = InferFunctionReturnType(funcName);
        return t switch
        {
            'A' => 'S',
            'I' => 'N',
            'N' => 'N',
            'L' => 'L',
            'D' => 'D',
            'T' => 'T',
            _ => 'S'
        };
    }

    /// <summary>
    /// Check if a function name is a known built-in.
    /// </summary>
    public static bool IsBuiltinFunction(string name)
    {
        return _functionNumbers.ContainsKey(name.ToUpperInvariant());
    }

    // Auto-generated reverse mapping of ExpressionDecoder._functionNames.
    // Values are sequential function IDs (NOT hex opcodes).
    private static readonly Dictionary<string, byte> _functionNumbers = new(StringComparer.OrdinalIgnoreCase)
    {
        ["NOP"] = 0,
        ["DATE"] = 1,
        ["COL"] = 2,
        ["ROW"] = 3,
        ["PCOL"] = 4,
        ["PROW"] = 5,
        ["SQRT"] = 6,
        ["DSPCE"] = 7,
        ["INKEY"] = 8,
        ["TIME"] = 9,
        ["CO"] = 10,
        ["FILL"] = 11,
        ["JUST"] = 12,
        ["TRIM"] = 13,
        ["UP"] = 14,
        ["LOW"] = 15,
        ["PROP"] = 16,
        ["CHR"] = 17,
        ["ASC"] = 18,
        ["GFL"] = 19,
        ["SIZE"] = 20,
        ["FTYP"] = 21,
        ["DOW"] = 22,
        ["CDOW"] = 23,
        ["MNTH"] = 24,
        ["CMNTH"] = 25,
        ["YEAR"] = 26,
        ["LOC"] = 27,
        ["MID"] = 28,
        ["FFLD"] = 29,
        ["FARRAY"] = 30,
        ["DTOC"] = 31,
        ["CTOD"] = 32,
        ["DTOR"] = 33,
        ["RTOD"] = 34,
        ["IIF"] = 35,
        ["ISAL"] = 36,
        ["ISUP"] = 37,
        ["ISLO"] = 38,
        ["MAX"] = 39,
        ["MIN"] = 40,
        ["XFORM"] = 41,
        ["VAL"] = 42,
        ["STR"] = 43,
        ["RNDM"] = 44,
        ["ISCLR"] = 45,
        ["FLSZE"] = 46,
        ["EOF"] = 47,
        ["BOF"] = 48,
        ["LROW"] = 49,
        ["LCKD"] = 50,
        ["ESC"] = 51,
        ["GSCHR"] = 52,
        ["FLERR"] = 53,
        ["PWHR"] = 54,
        ["CFLT"] = 55,
        ["CINT"] = 56,
        ["CBYT"] = 57,
        ["CREC"] = 58,
        ["CCH"] = 59,
        ["CPATH"] = 60,
        ["IFCR"] = 61,
        ["RCN"] = 62,
        ["TRC"] = 63,
        ["PERR"] = 64,
        ["ALOCARY"] = 65,
        ["AVAIL"] = 66,
        ["ASK"] = 67,
        ["ETYP"] = 68,
        ["LSTCHR"] = 69,
        ["RENF"] = 70,
        ["WRAP"] = 71,
        ["IFNA"] = 72,
        ["LOG"] = 73,
        ["LOG10"] = 74,
        ["TPATH"] = 75,
        ["CCE"] = 76,
        ["FCHR"] = 77,
        ["CC"] = 78,
        ["PSTAT"] = 79,
        ["FNUM"] = 80,
        ["TTOC"] = 81,
        ["CTOT"] = 82,
        ["TTOR"] = 83,
        ["RTOT"] = 84,
        ["EXP"] = 85,
        ["CCF"] = 86,
        ["CCR"] = 87,
        ["FTOT"] = 88,
        ["TTOF"] = 89,
        ["RTP"] = 90,
        ["ALC_FLD"] = 91,
        ["FFILE"] = 92,
        ["PSET"] = 93,
        ["AEV"] = 94,
        ["LCHR"] = 95,
        ["MOD"] = 96,
        ["FLDATE"] = 97,
        ["DELF"] = 98,
        ["MID_REC"] = 99,
        ["DPATH"] = 100,
        ["DIFF"] = 101,
        ["SNDX"] = 102,
        ["PI"] = 103,
        ["ACOS"] = 104,
        ["ASIN"] = 105,
        ["ATAN"] = 106,
        ["COS"] = 107,
        ["SIN"] = 108,
        ["TAN"] = 109,
        ["ATAN2"] = 110,
        ["ABS"] = 111,
        ["CEIL"] = 112,
        ["FLOOR"] = 113,
        ["OS"] = 114,
        ["VER"] = 115,
        ["DMY"] = 116,
        ["DTOS"] = 117,
        ["NUMFLDS"] = 118,
        ["FLDNME"] = 119,
        ["GETENV"] = 120,
        ["INT"] = 121,
        ["LIKE"] = 122,
        ["MDY"] = 123,
        ["RSIZE"] = 124,
        ["MEM"] = 125,
        ["ROUND"] = 126,
        ["SIGN"] = 127,
        ["VARREAD"] = 128,
        ["NULL"] = 129,
        ["ELOC"] = 130,
        ["ENTER"] = 131,
        ["MOUSE_ACT"] = 132,
        ["ACS"] = 133,
        ["HEX"] = 134,
        ["LTOC"] = 135,
        ["CTOL"] = 136,
        ["CLNUM"] = 137,
        ["ALOC"] = 138,
        ["FLTIME"] = 139,
        ["WRAPO"] = 140,
        ["WRAPL"] = 141,
        ["WRAPS"] = 142,
        ["FLDFNUM"] = 143,
        ["GFLD"] = 144,
        ["SYSOPS"] = 145,
        ["CRSU"] = 146,
        ["EXEC"] = 147,
        ["OPEN"] = 148,
        ["DOM"] = 149,
        ["NUMLBLS"] = 150,
        ["LBLNME"] = 151,
        ["LBL_LNE"] = 152,
        ["PRGNME"] = 153,
        ["PRGLNE"] = 154,
        ["PRG_OFST"] = 155,
        ["MOUSE_ROW"] = 156,
        ["MOUSE_COL"] = 157,
        ["TEST"] = 158,
        ["ISNUM"] = 159,
        ["RETVAL"] = 160,
        ["TRAP_LABEL"] = 161,
        ["TRAP_DO"] = 162,
        ["REC_PTR"] = 163,
        ["GET_REC"] = 164,
        ["CUR_PRG"] = 165,
        ["RECORD_CHR"] = 166,
        ["OFFSET3"] = 167,
        ["MEMO_COL"] = 168,
        ["MEMO_ROW"] = 169,
        ["LISTF_CHRS"] = 170,
        ["MOUSE_ON"] = 171,
        ["SERIAL_PORT"] = 172,
        ["REC_CHANGED"] = 173,
        ["COMP_BIN"] = 174,
        ["ACTIVE"] = 175,
        ["WINDOWS"] = 176,
        ["PRINT_CANCEL"] = 177,
        ["PRINTER_NAME"] = 178,
        ["SROK"] = 179,
        ["DATE_TYPE"] = 180,
        ["DATE_SEPARATOR"] = 181,
        ["LOAD_OVL"] = 182,
        ["CHK_PSC"] = 183,
        ["LOAD_OBJ"] = 184,
        ["LPATH"] = 185,
        ["WINDOW_PTR"] = 186,
        ["MAKE_DIR"] = 187,
        ["COPY_FILE"] = 188,
        ["EDIT"] = 189,
        ["GET_WCOLOR"] = 190,
        ["REGEDIT"] = 191,
        ["SETUP_PRINTER"] = 192,
        ["CHOOSE_COLOR"] = 193,
        ["WQUIT"] = 194,
        ["GET_ELEM_NUM"] = 195,
        ["CLICKED_ON"] = 196,
        ["WLASER_PRT"] = 197,
        ["MAX_ROWS"] = 198,
        ["MAX_COLS"] = 199,
        ["DIRECT"] = 200,
        ["DSROK"] = 201,
        ["WHICH_SIZE"] = 202,
        ["NUM_USERS"] = 203,
        ["END_DATE"] = 204,
        ["GET_OBJ_PROP"] = 205,
        ["LOAD_MODAL_FORM"] = 206,
        ["REG_CODE"] = 207,
        ["GET_FORM_NAME"] = 208,
        ["GET_PATH"] = 209,
        ["RESET_FIELD"] = 210,
        ["STRINGS"] = 211,
        ["CREATE_DBF"] = 212,
        ["CONVERT_TO_DBF"] = 213,
        ["LOAD_PRG"] = 214,
        ["CALL_PRG"] = 215,
        ["FORM_PTR"] = 216,
        ["IS_PRG_LOADED"] = 217,
        ["REMOVE_PRG"] = 218,
        ["DUAL_LIST_EXEC"] = 219,
        ["FILE_EXISTS"] = 220,
        ["TEXT_WIDTH"] = 221,
        ["GET_RUN_PRG"] = 222,
        ["STATUS_BAR"] = 223,
        ["GET_FILE"] = 224,
        ["XPATH"] = 225,
        ["WHOAMI"] = 226,
        ["PARSEFILE"] = 227,
        ["OPEN_FILE_NAME"] = 228,
        ["MAKE_PATH"] = 229,
        ["LAST_FILE"] = 230,
        ["LAST_OBJ"] = 231,
        ["VALID_CHECK"] = 232,
        ["GRID_VALID_CHECK"] = 233,
        ["SET_OBJ_PROP"] = 234,
        ["SETENV"] = 235,
        ["USECODEBASE"] = 236,
        ["CRLF"] = 237,
        ["GET_COMP_NAME"] = 238,
        ["GET_USER_NAME"] = 239,
        ["GET_BUFF_NAME"] = 240,
        ["REC_PERCENTAGE"] = 241,
        ["GET_INI_PATH"] = 242,
        ["EMAIL"] = 243,
        ["TLLFC"] = 244,
        ["RESTRUCTURE_DBF"] = 245,
        ["IFDUPCB"] = 246,
        ["DFM_TO_TXT"] = 247,
        ["TXT_TO_DFM"] = 248,
        ["ADD_OBJECT"] = 249,
        ["ENCRYPT"] = 250,
        ["PACK_DBF"] = 251,
        ["REINDEX_DBF"] = 252,
        ["LOAD_DLL"] = 253,
        ["DLLFC"] = 254,
        ["REMOVE_DLL"] = 255,
        // Functions > 255 are TAS 6.0+ only (2-byte funcNum)
    };

    private record OperandRef(char Type, int Location);
}
