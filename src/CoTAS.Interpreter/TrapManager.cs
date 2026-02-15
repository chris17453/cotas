namespace CoTAS.Interpreter;

public enum TrapAction
{
    Default,
    Goto,
    Gosub,
    Ignore,
}

public sealed class TrapDefinition
{
    public TrapAction Action { get; set; }
    public string? Label { get; set; }
}

/// <summary>
/// Manages trap definitions for function keys and ESC.
/// Supports push/pop stack for nested trap contexts.
/// </summary>
public sealed class TrapManager
{
    private readonly Dictionary<string, TrapDefinition> _traps = new(StringComparer.OrdinalIgnoreCase);
    private readonly Stack<Dictionary<string, TrapDefinition>> _stack = new();

    public void SetTrap(string key, TrapAction action, string? label)
    {
        _traps[key] = new TrapDefinition { Action = action, Label = label };
    }

    public TrapDefinition? GetTrap(string key)
    {
        return _traps.TryGetValue(key, out var trap) ? trap : null;
    }

    public void Push()
    {
        var snapshot = new Dictionary<string, TrapDefinition>(StringComparer.OrdinalIgnoreCase);
        foreach (var kvp in _traps)
            snapshot[kvp.Key] = new TrapDefinition { Action = kvp.Value.Action, Label = kvp.Value.Label };
        _stack.Push(snapshot);
    }

    public void Pop()
    {
        if (_stack.Count == 0) return;
        var snapshot = _stack.Pop();
        _traps.Clear();
        foreach (var kvp in snapshot)
            _traps[kvp.Key] = kvp.Value;
    }

    public void ClearAll()
    {
        _traps.Clear();
    }
}
