namespace CoTAS.Interpreter;

public sealed class FieldManager
{
    private readonly Dictionary<string, TasValue> _fields = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, TasValue[]> _arrays = new(StringComparer.OrdinalIgnoreCase);

    // Field value stack for PUSHF/POPF
    private readonly Stack<Dictionary<string, TasValue>> _fieldStack = new();

    // Metadata: format strings, pictures, justification
    private readonly Dictionary<string, string> _formats = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, string> _pictures = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, string> _justification = new(StringComparer.OrdinalIgnoreCase);

    // System state
    public string CompanyCode { get; set; } = "01";
    public string ProgramName { get; set; } = "";
    public int ProgramLine { get; set; }
    public string LastVarRead { get; set; } = "";
    public bool LastEscPressed { get; set; }
    public bool LastEnterPressed { get; set; } = true;
    public int LastKeyPressed { get; set; }
    public int LastErrorType { get; set; }
    public bool LastAskResult { get; set; } = true;
    public TasValue? LastReturnValue { get; set; }

    // Screen state
    public int CursorRow { get; set; } = 1;
    public int CursorCol { get; set; } = 1;
    public int PrinterRow { get; set; } = 1;
    public int PrinterCol { get; set; } = 1;
    public bool PrinterOn { get; set; }
    public string PrintToFile { get; set; } = "";
    public StreamWriter? PrintWriter { get; set; }

    // Flags
    public bool TraceEnabled { get; set; }
    public bool ClockEnabled { get; set; }
    public bool InterruptsEnabled { get; set; } = true;
    public bool NoRestart { get; set; }
    public bool NoRedisplay { get; set; }
    public bool NoValidMessage { get; set; }
    public bool ForceEntry { get; set; }
    public string ForceField { get; set; } = "";
    public bool ReenterRequested { get; set; }
    public string DateFormat { get; set; } = "MM/DD/YY";
    public bool GrayMode { get; set; }
    public bool ReverseVideo { get; set; }
    public bool KeyboardUpper { get; set; }
    public string OwnerName { get; set; } = "";
    public string AutoRunProgram { get; set; } = "";
    public bool NoClear { get; set; }
    public bool NoFieldDefault { get; set; }
    public bool NoComma { get; set; }
    public bool NoZero { get; set; }
    public bool PrintAll { get; set; }

    // Screen buffer (25x80)
    private readonly char[,] _screenBuffer = new char[25, 80];
    private readonly char[,] _savedScreen = new char[25, 80];

    public FieldManager()
    {
        ClearScreenBuffer();
    }

    public void Define(string name, string? typeCode, int size, int decimals, int arraySize)
    {
        if (arraySize > 0)
        {
            var arr = new TasValue[arraySize];
            for (int i = 0; i < arraySize; i++)
                arr[i] = TasValue.DefaultForType(typeCode, size, decimals);
            _arrays[name] = arr;
        }
        else
        {
            _fields[name] = TasValue.DefaultForType(typeCode, size, decimals);
        }
    }

    public TasValue Get(string name)
    {
        if (_fields.TryGetValue(name, out var val))
            return val;
        // TAS Runtime Error 19: field is unallocated
        throw new InterpreterException($"Undefined field: {name} (field not defined or file not opened)");
    }

    public TasValue GetArrayElement(string name, int index)
    {
        if (_arrays.TryGetValue(name, out var arr))
        {
            if (index < 1 || index > arr.Length)
                throw new InterpreterException($"Array index {index} out of bounds for {name}[1..{arr.Length}]");
            return arr[index - 1]; // TAS arrays are 1-based
        }
        throw new InterpreterException($"Undefined array: {name}");
    }

    public int GetArraySize(string name)
    {
        if (_arrays.TryGetValue(name, out var arr))
            return arr.Length;
        return 0;
    }

    public void Set(string name, TasValue value)
    {
        if (_fields.ContainsKey(name))
        {
            var existing = _fields[name];
            // Coerce to match the defined type
            existing.Value = CoerceValue(value, existing.Type);
        }
        else
        {
            // Auto-define if not defined (common in TAS)
            _fields[name] = value.Clone();
        }
    }

    public void SetArrayElement(string name, int index, TasValue value)
    {
        if (_arrays.TryGetValue(name, out var arr))
        {
            if (index < 1 || index > arr.Length)
                throw new InterpreterException($"Array index {index} out of bounds for {name}[1..{arr.Length}]");
            arr[index - 1].Value = CoerceValue(value, arr[index - 1].Type);
            return;
        }
        throw new InterpreterException($"Undefined array: {name}");
    }

    public bool IsDefined(string name) => _fields.ContainsKey(name) || _arrays.ContainsKey(name);
    public bool IsArray(string name) => _arrays.ContainsKey(name);

    /// <summary>Remove a field or array definition.</summary>
    public void Remove(string name)
    {
        _fields.Remove(name);
        _arrays.Remove(name);
        _formats.Remove(name);
        _pictures.Remove(name);
        _justification.Remove(name);
    }

    /// <summary>Redefine a field's type and size without changing its current value.</summary>
    public void Redefine(string name, string? typeCode, int size, int decimals)
    {
        object currentVal = _fields.TryGetValue(name, out var existing) ? existing.Value : "";
        var newVal = TasValue.DefaultForType(typeCode, size, decimals);
        // Try to preserve the old value coerced to the new type
        try
        {
            if (existing != null)
                newVal.Value = CoerceValue(existing, newVal.Type);
        }
        catch { /* use default */ }
        _fields[name] = newVal;
    }

    // --- PUSHF/POPF field stack ---

    /// <summary>Push specified field values onto the stack.</summary>
    public void PushFields(IEnumerable<string> fieldNames)
    {
        var snapshot = new Dictionary<string, TasValue>(StringComparer.OrdinalIgnoreCase);
        foreach (var name in fieldNames)
        {
            if (_fields.TryGetValue(name, out var val))
                snapshot[name] = val.Clone();
        }
        _fieldStack.Push(snapshot);
    }

    /// <summary>Pop field values from the stack.</summary>
    public void PopFields()
    {
        if (_fieldStack.Count == 0) return;
        var snapshot = _fieldStack.Pop();
        foreach (var (name, val) in snapshot)
            _fields[name] = val;
    }

    // --- Format/Picture/Justify metadata ---

    public void SetFormat(string fieldName, string format) => _formats[fieldName] = format;
    public string GetFormat(string fieldName) => _formats.TryGetValue(fieldName, out var f) ? f : "";

    public void SetPicture(string fieldName, string picture) => _pictures[fieldName] = picture;
    public string GetPicture(string fieldName) => _pictures.TryGetValue(fieldName, out var p) ? p : "";

    public void SetJustification(string fieldName, string just) => _justification[fieldName] = just;
    public string GetJustification(string fieldName) => _justification.TryGetValue(fieldName, out var j) ? j : "L";

    /// <summary>Justify a field value in place.</summary>
    public void JustifyField(string fieldName, string alignment)
    {
        if (!_fields.TryGetValue(fieldName, out var val)) return;
        string s = val.AsString();
        int size = val.Size > 0 ? val.Size : s.Length;
        string result = alignment.ToUpper() switch
        {
            "R" => s.TrimEnd().PadLeft(size),
            "C" => s.Trim().PadLeft((size + s.Trim().Length) / 2).PadRight(size),
            _ => s.TrimEnd().PadRight(size),
        };
        if (result.Length > size) result = result[..size];
        _fields[fieldName] = new TasValue(val.Type, result, size, val.Decimals);
        _justification[fieldName] = alignment.ToUpper();
    }

    // --- Array operations ---

    /// <summary>Sort an array in place, optionally descending.</summary>
    public void SortArray(string name, bool descending = false)
    {
        if (!_arrays.TryGetValue(name, out var arr)) return;
        Array.Sort(arr, (a, b) =>
        {
            int cmp;
            if (a.Type == TasType.Alpha || b.Type == TasType.Alpha)
                cmp = string.Compare(a.AsString().TrimEnd(), b.AsString().TrimEnd(), StringComparison.OrdinalIgnoreCase);
            else
                cmp = a.AsNumeric().CompareTo(b.AsNumeric());
            return descending ? -cmp : cmp;
        });
    }

    /// <summary>Remove an array element at index (1-based), shifting remaining elements down.</summary>
    public void RemoveArrayElement(string name, int index)
    {
        if (!_arrays.TryGetValue(name, out var arr)) return;
        if (index < 1 || index > arr.Length) return;
        // Shift elements down
        for (int i = index - 1; i < arr.Length - 1; i++)
            arr[i] = arr[i + 1];
        // Clear last element
        arr[^1] = TasValue.DefaultForType(arr[0].Type switch
        {
            TasType.Alpha => "A",
            TasType.Numeric => "N",
            TasType.Integer => "I",
            TasType.Logical => "L",
            _ => "A"
        }, arr[0].Size, arr[0].Decimals);
    }

    /// <summary>Deallocate an array.</summary>
    public void DeallocateArray(string name)
    {
        _arrays.Remove(name);
    }

    /// <summary>Get all field names (for enumeration).</summary>
    public IEnumerable<string> GetFieldNames() => _fields.Keys;

    /// <summary>Get all array names.</summary>
    public IEnumerable<string> GetArrayNames() => _arrays.Keys;

    /// <summary>Get field count.</summary>
    public int FieldCount => _fields.Count;

    // --- Screen buffer ---

    public void ClearScreenBuffer()
    {
        for (int r = 0; r < 25; r++)
            for (int c = 0; c < 80; c++)
                _screenBuffer[r, c] = ' ';
    }

    public void WriteToScreen(int row, int col, string text)
    {
        if (row < 1 || row > 25) return;
        for (int i = 0; i < text.Length && col + i <= 80; i++)
            _screenBuffer[row - 1, col - 1 + i] = text[i];
        CursorRow = row;
        CursorCol = Math.Min(col + text.Length, 80);
    }

    public char GetScreenChar(int row, int col)
    {
        if (row < 1 || row > 25 || col < 1 || col > 80) return ' ';
        return _screenBuffer[row - 1, col - 1];
    }

    public void ClearScreenLine(int row)
    {
        if (row < 1 || row > 25) return;
        for (int c = 0; c < 80; c++)
            _screenBuffer[row - 1, c] = ' ';
    }

    public bool IsScreenClear()
    {
        for (int r = 0; r < 25; r++)
            for (int c = 0; c < 80; c++)
                if (_screenBuffer[r, c] != ' ') return false;
        return true;
    }

    public void SaveScreen()
    {
        Array.Copy(_screenBuffer, _savedScreen, _screenBuffer.Length);
    }

    public void RestoreScreen()
    {
        Array.Copy(_savedScreen, _screenBuffer, _savedScreen.Length);
    }

    private static object CoerceValue(TasValue source, TasType targetType)
    {
        return targetType switch
        {
            TasType.Alpha => source.AsString(),
            TasType.Numeric => source.AsNumeric(),
            TasType.Integer => source.AsInteger(),
            TasType.Logical => source.AsLogical(),
            _ => source.Value,
        };
    }
}
