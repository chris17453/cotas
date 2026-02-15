namespace CoTAS.Interpreter.Commands;

/// <summary>
/// KBDUP ON/OFF - Set keyboard to uppercase mode.
/// </summary>
public sealed class KbdupCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.KeyboardUpper = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
        else
            ctx.Fields.KeyboardUpper = true; // KBDUP alone means ON
        return Task.CompletedTask;
    }
}

/// <summary>
/// AUTODEC field1,field2,... - Set auto-decrement list for data entry fields.
/// </summary>
public sealed class AutoDecCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        var names = new List<string>();
        foreach (var t in ctx.Tokens)
            if (t.Value != "," && !string.IsNullOrWhiteSpace(t.Value))
                names.Add(t.Value);
        ctx.Fields.Set("__AUTODEC_LIST", new TasValue(TasType.Alpha, string.Join(",", names), 200));
        return Task.CompletedTask;
    }
}

/// <summary>
/// AUTOENTER field1,field2,... - Set auto-enter list for data entry fields.
/// </summary>
public sealed class AutoEnterCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        var names = new List<string>();
        foreach (var t in ctx.Tokens)
            if (t.Value != "," && !string.IsNullOrWhiteSpace(t.Value))
                names.Add(t.Value);
        ctx.Fields.Set("__AUTOENTER_LIST", new TasValue(TasType.Alpha, string.Join(",", names), 200));
        return Task.CompletedTask;
    }
}

/// <summary>
/// AUTOINC field1,field2,... - Set auto-increment list for data entry fields.
/// </summary>
public sealed class AutoIncCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        var names = new List<string>();
        foreach (var t in ctx.Tokens)
            if (t.Value != "," && !string.IsNullOrWhiteSpace(t.Value))
                names.Add(t.Value);
        ctx.Fields.Set("__AUTOINC_LIST", new TasValue(TasType.Alpha, string.Join(",", names), 200));
        return Task.CompletedTask;
    }
}

/// <summary>
/// NOCLR - Set no-clear flag for file opens.
/// </summary>
public sealed class NoclrCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.NoClear = true;
        return Task.CompletedTask;
    }
}

/// <summary>
/// NOFD - Set no-field-default flag.
/// </summary>
public sealed class NofdCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.NoFieldDefault = true;
        return Task.CompletedTask;
    }
}

/// <summary>
/// NOCMA - Suppress commas in numeric display.
/// </summary>
public sealed class NocmaCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.NoComma = true;
        return Task.CompletedTask;
    }
}

/// <summary>
/// NOZERO - Suppress zero display.
/// </summary>
public sealed class NozeroCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.NoZero = true;
        return Task.CompletedTask;
    }
}
