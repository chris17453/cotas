namespace CoTAS.Interpreter.Commands;

/// <summary>
/// UPAR - Up arrow behavior control. In data entry, moves to previous field.
/// </summary>
public sealed class UpArrowCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // UPAR [fieldname] - set up-arrow target field or enable up-arrow navigation
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.Set("__UPAR_TARGET", new TasValue(TasType.Alpha, ctx.Tokens[0].Value, 40));
        else
            ctx.Fields.Set("__UPAR_ENABLED", new TasValue(TasType.Logical, true));
        return Task.CompletedTask;
    }
}

/// <summary>
/// DNAR - Down arrow behavior control. In data entry, moves to next field.
/// </summary>
public sealed class DownArrowCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.Set("__DNAR_TARGET", new TasValue(TasType.Alpha, ctx.Tokens[0].Value, 40));
        else
            ctx.Fields.Set("__DNAR_ENABLED", new TasValue(TasType.Logical, true));
        return Task.CompletedTask;
    }
}

/// <summary>
/// FEXIT - Forced exit from data entry field.
/// </summary>
public sealed class FexitCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // FEXIT sets a flag so the data entry loop exits after the current field
        ctx.Fields.Set("__FEXIT", new TasValue(TasType.Logical, true));
        return Task.CompletedTask;
    }
}

/// <summary>
/// SEXIT - Exit from a SCAN loop (also handled in AST for SCAN blocks).
/// </summary>
public sealed class SexitCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SEXIT signals the interpreter's SCAN loop to break
        ctx.Fields.Set("__SEXIT", new TasValue(TasType.Logical, true));
        return Task.CompletedTask;
    }
}

/// <summary>
/// SLOOP - Continue to next iteration in SCAN loop (also handled in AST).
/// </summary>
public sealed class SloopCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SLOOP signals the interpreter's SCAN loop to continue
        ctx.Fields.Set("__SLOOP", new TasValue(TasType.Logical, true));
        return Task.CompletedTask;
    }
}

/// <summary>
/// SET_LINE n - Set the current line number for execution.
/// </summary>
public sealed class SetLineCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SET_LINE n - jump to line number (interpreter will handle this)
        if (ctx.Tokens.Count >= 1 && int.TryParse(ctx.Tokens[0].Value, out int line))
            ctx.Fields.Set("__SET_LINE", new TasValue(TasType.Integer, line));
        return Task.CompletedTask;
    }
}
