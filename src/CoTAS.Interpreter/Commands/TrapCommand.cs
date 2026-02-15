namespace CoTAS.Interpreter.Commands;

/// <summary>
/// TRAP key action [label]
/// Defines trap behavior for function keys and ESC.
/// Examples: TRAP ESC GOSUB MYEXIT, TRAP F2 GOTO SAVE, TRAP ESC IGNR
/// </summary>
public sealed class TrapCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count < 2)
            throw new InterpreterException("TRAP requires key and action");

        string key = ctx.Tokens[0].Value.ToUpperInvariant();
        string action = ctx.Tokens[1].Value.ToUpperInvariant();
        string? label = ctx.Tokens.Count >= 3 ? ctx.Tokens[2].Value : null;

        var trapAction = action switch
        {
            "GOTO" => TrapAction.Goto,
            "GOSUB" => TrapAction.Gosub,
            "IGNR" => TrapAction.Ignore,
            "DFLT" => TrapAction.Default,
            _ => TrapAction.Ignore,
        };

        ctx.Traps.SetTrap(key, trapAction, label);
        return Task.CompletedTask;
    }
}

public sealed class PushTrapCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Traps.Push();
        return Task.CompletedTask;
    }
}

public sealed class PopTrapCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Traps.Pop();
        return Task.CompletedTask;
    }
}

public sealed class XtrapCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // XTRAP clears all traps
        ctx.Traps.ClearAll();
        return Task.CompletedTask;
    }
}
