namespace CoTAS.Interpreter.Commands;

/// <summary>
/// RLCK @handle - Lock the current record in a file.
/// </summary>
public sealed class RecordLockCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count >= 1)
        {
            string handleRef = ctx.Tokens[0].Value;
            ctx.Fields.Set($"__FILE_{handleRef}_LOCKED", new TasValue(TasType.Logical, true));
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// ULKALL - Unlock all locked records across all open files.
/// </summary>
public sealed class UnlockAllCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // Clear all lock flags
        foreach (var name in ctx.Fields.GetFieldNames().ToList())
        {
            if (name.StartsWith("__FILE_") && name.EndsWith("_LOCKED"))
                ctx.Fields.Set(name, new TasValue(TasType.Logical, false));
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// FILELOC @handle - Get/set file location (record pointer).
/// </summary>
public sealed class FileLocCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // FILELOC @handle [= position]
        if (ctx.Tokens.Count >= 1)
        {
            string handleRef = ctx.Tokens[0].Value;
            int eqIdx = ctx.FindToken("=");
            if (eqIdx >= 0 && eqIdx + 1 < ctx.Tokens.Count)
            {
                // Set file location
                if (int.TryParse(ctx.Tokens[eqIdx + 1].Value, out int pos))
                    ctx.Fields.Set($"__FILE_{handleRef}_LOC", new TasValue(TasType.Integer, pos));
            }
            else
            {
                // Just store the handle reference for the function to read
                ctx.Fields.Set("__FILELOC_HANDLE", new TasValue(TasType.Alpha, handleRef, 20));
            }
        }
        return Task.CompletedTask;
    }
}
