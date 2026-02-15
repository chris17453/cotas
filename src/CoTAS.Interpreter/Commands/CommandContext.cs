using CoTAS.Parser;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// Context passed to command handlers with everything they need to execute.
/// </summary>
public sealed class CommandContext
{
    public required List<Token> Tokens { get; init; }
    public required FieldManager Fields { get; init; }
    public required ExpressionEvaluator Evaluator { get; init; }
    public required IUIBridge UI { get; init; }
    public required TrapManager Traps { get; init; }
    public required int Line { get; init; }

    /// <summary>
    /// Get token value at index, or null if out of range.
    /// </summary>
    public string? TokenAt(int index) =>
        index >= 0 && index < Tokens.Count ? Tokens[index].Value : null;

    /// <summary>
    /// Find the index of a token with the given value (case-insensitive).
    /// Returns -1 if not found.
    /// </summary>
    public int FindToken(string value)
    {
        for (int i = 0; i < Tokens.Count; i++)
            if (Tokens[i].Value.Equals(value, StringComparison.OrdinalIgnoreCase))
                return i;
        return -1;
    }

    /// <summary>
    /// Check if any token matches the given value (case-insensitive).
    /// </summary>
    public bool HasToken(string value) => FindToken(value) >= 0;

    /// <summary>
    /// Join all token values with spaces (useful for getting the raw text of arguments).
    /// </summary>
    public string JoinTokens(int startIndex = 0)
    {
        if (startIndex >= Tokens.Count) return "";
        return string.Join(" ", Tokens.Skip(startIndex).Select(t => t.Value));
    }
}
