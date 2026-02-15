namespace CoTAS.Interpreter.Commands;

/// <summary>
/// INSRT fieldname,position,insertstring - Insert characters into a field at position.
/// </summary>
public sealed class InsrtCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // INSRT fieldname,pos,string or INSRT fieldname pos string
        if (ctx.Tokens.Count < 2) return Task.CompletedTask;

        string fieldName = ctx.Tokens[0].Value;
        if (!ctx.Fields.IsDefined(fieldName)) return Task.CompletedTask;

        var val = ctx.Fields.Get(fieldName);
        string s = val.AsString();

        // Parse position (skip commas)
        int tokenIdx = 1;
        if (tokenIdx < ctx.Tokens.Count && ctx.Tokens[tokenIdx].Value == ",") tokenIdx++;
        if (tokenIdx >= ctx.Tokens.Count) return Task.CompletedTask;

        if (!int.TryParse(ctx.Tokens[tokenIdx].Value, out int pos)) return Task.CompletedTask;
        tokenIdx++;

        // Parse insert string (skip comma)
        if (tokenIdx < ctx.Tokens.Count && ctx.Tokens[tokenIdx].Value == ",") tokenIdx++;
        if (tokenIdx >= ctx.Tokens.Count) return Task.CompletedTask;

        string insertStr = ctx.Tokens[tokenIdx].Value.Trim('\'', '"');

        // 1-based position
        if (pos < 1) pos = 1;
        if (pos > s.Length + 1) pos = s.Length + 1;

        string result = s[..(pos - 1)] + insertStr + s[(pos - 1)..];
        // Truncate to field size
        if (val.Size > 0 && result.Length > val.Size)
            result = result[..val.Size];

        ctx.Fields.Set(fieldName, new TasValue(val.Type, result, val.Size));
        return Task.CompletedTask;
    }
}

/// <summary>
/// DELC fieldname,position,count - Delete characters from a field.
/// </summary>
public sealed class DelcCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // DELC fieldname,pos,count
        if (ctx.Tokens.Count < 2) return Task.CompletedTask;

        string fieldName = ctx.Tokens[0].Value;
        if (!ctx.Fields.IsDefined(fieldName)) return Task.CompletedTask;

        var val = ctx.Fields.Get(fieldName);
        string s = val.AsString();

        // Parse position (skip commas)
        int tokenIdx = 1;
        if (tokenIdx < ctx.Tokens.Count && ctx.Tokens[tokenIdx].Value == ",") tokenIdx++;
        if (tokenIdx >= ctx.Tokens.Count) return Task.CompletedTask;

        if (!int.TryParse(ctx.Tokens[tokenIdx].Value, out int pos)) return Task.CompletedTask;
        tokenIdx++;

        // Parse count (skip comma)
        if (tokenIdx < ctx.Tokens.Count && ctx.Tokens[tokenIdx].Value == ",") tokenIdx++;
        int count = 1;
        if (tokenIdx < ctx.Tokens.Count)
            int.TryParse(ctx.Tokens[tokenIdx].Value, out count);

        // 1-based position
        if (pos < 1 || pos > s.Length) return Task.CompletedTask;
        int endPos = Math.Min(pos - 1 + count, s.Length);

        string result = s[..(pos - 1)] + s[endPos..];
        // Pad to field size
        if (val.Size > 0)
            result = result.PadRight(val.Size)[..val.Size];

        ctx.Fields.Set(fieldName, new TasValue(val.Type, result, val.Size));
        return Task.CompletedTask;
    }
}

/// <summary>
/// MID fieldname,pos,len = value - Assign to a portion of a string field.
/// </summary>
public sealed class MidCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // MID fieldname,pos,len = value
        if (ctx.Tokens.Count < 3) return Task.CompletedTask;

        string fieldName = ctx.Tokens[0].Value;
        if (!ctx.Fields.IsDefined(fieldName)) return Task.CompletedTask;

        var val = ctx.Fields.Get(fieldName);
        string s = val.AsString();

        // Parse pos (skip commas)
        int tokenIdx = 1;
        if (tokenIdx < ctx.Tokens.Count && ctx.Tokens[tokenIdx].Value == ",") tokenIdx++;
        if (tokenIdx >= ctx.Tokens.Count) return Task.CompletedTask;
        if (!int.TryParse(ctx.Tokens[tokenIdx].Value, out int pos)) return Task.CompletedTask;
        tokenIdx++;

        // Parse len (skip comma)
        if (tokenIdx < ctx.Tokens.Count && ctx.Tokens[tokenIdx].Value == ",") tokenIdx++;
        int len = 1;
        if (tokenIdx < ctx.Tokens.Count)
            int.TryParse(ctx.Tokens[tokenIdx].Value, out len);
        tokenIdx++;

        // Find = sign
        while (tokenIdx < ctx.Tokens.Count && ctx.Tokens[tokenIdx].Value != "=") tokenIdx++;
        tokenIdx++; // skip =

        if (tokenIdx >= ctx.Tokens.Count) return Task.CompletedTask;
        string newValue = ctx.Tokens[tokenIdx].Value.Trim('\'', '"');

        // 1-based position
        if (pos < 1) pos = 1;
        if (pos > s.Length) return Task.CompletedTask;

        // Ensure the new value fits in the len
        if (newValue.Length > len)
            newValue = newValue[..len];
        else if (newValue.Length < len)
            newValue = newValue.PadRight(len);

        char[] chars = s.PadRight(val.Size > 0 ? val.Size : s.Length).ToCharArray();
        for (int i = 0; i < newValue.Length && pos - 1 + i < chars.Length; i++)
            chars[pos - 1 + i] = newValue[i];

        string result = new string(chars);
        ctx.Fields.Set(fieldName, new TasValue(val.Type, result, val.Size));
        return Task.CompletedTask;
    }
}

/// <summary>
/// ADD fieldname,value or ADD value TO fieldname - Add a value to a numeric field.
/// </summary>
public sealed class AddCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // ADD value TO fieldname  or  ADD fieldname,value
        if (ctx.Tokens.Count < 2) return Task.CompletedTask;

        int toIdx = ctx.FindToken("TO");
        string fieldName;
        double addValue;

        if (toIdx > 0 && toIdx + 1 < ctx.Tokens.Count)
        {
            // ADD value TO fieldname
            fieldName = ctx.Tokens[toIdx + 1].Value;
            double.TryParse(ctx.Tokens[0].Value, out addValue);
        }
        else
        {
            // ADD fieldname,value
            fieldName = ctx.Tokens[0].Value;
            int valIdx = 1;
            if (valIdx < ctx.Tokens.Count && ctx.Tokens[valIdx].Value == ",") valIdx++;
            if (valIdx >= ctx.Tokens.Count) return Task.CompletedTask;
            double.TryParse(ctx.Tokens[valIdx].Value, out addValue);
        }

        if (!ctx.Fields.IsDefined(fieldName)) return Task.CompletedTask;
        var current = ctx.Fields.Get(fieldName);
        double result = current.AsNumeric() + addValue;
        ctx.Fields.Set(fieldName, new TasValue(current.Type, result, current.Size, current.Decimals));
        return Task.CompletedTask;
    }
}
