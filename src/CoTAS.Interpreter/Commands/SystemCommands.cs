namespace CoTAS.Interpreter.Commands;

/// <summary>
/// MOUSE ON/OFF - Enable/disable mouse support.
/// </summary>
public sealed class MouseCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count >= 1)
        {
            bool on = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
            ctx.Fields.Set("__MOUSE_ENABLED", new TasValue(TasType.Logical, on));
        }
        else
        {
            ctx.Fields.Set("__MOUSE_ENABLED", new TasValue(TasType.Logical, true));
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// POINTER ON/OFF or POINTER type - Set mouse pointer style.
/// </summary>
public sealed class PointerCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.Set("__POINTER", new TasValue(TasType.Alpha, ctx.Tokens[0].Value.ToUpper(), 20));
        return Task.CompletedTask;
    }
}

/// <summary>
/// ERROR type - Set/trigger error handling.
/// </summary>
public sealed class ErrorCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count >= 1)
        {
            string errType = ctx.Tokens[0].Value.ToUpper();
            ctx.Fields.LastErrorType = errType switch
            {
                "CLEAR" => 0,
                _ => int.TryParse(errType, out int n) ? n : 1
            };
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// CLRPE - Clear program error flag.
/// </summary>
public sealed class ClearProgramErrorCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.LastErrorType = 0;
        return Task.CompletedTask;
    }
}

/// <summary>
/// COMPILE program - Compile a TAS source program.
/// In web mode this is a no-op since programs are interpreted directly.
/// </summary>
public sealed class CompileCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // In interpreted mode, COMPILE is a no-op
        // Store the program name in case something checks it
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.Set("__COMPILE_PROGRAM", new TasValue(TasType.Alpha, ctx.Tokens[0].Value, 40));
        return Task.CompletedTask;
    }
}

/// <summary>
/// SELECT filename - File selection command (different from SELECT/CASE).
/// Opens a file selection dialog or sets the active file.
/// </summary>
public sealed class FileSelectCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SELECT as a command (not SELECT/CASE which is parsed as AST)
        // This is the file-select command
        if (ctx.Tokens.Count >= 1)
        {
            string filename = ctx.Tokens[0].Value.Trim('\'', '"');
            ctx.Fields.Set("__SELECT_FILE", new TasValue(TasType.Alpha, filename, 80));
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// CLRSF - Clear screen fields (reset all displayed field values on screen).
/// </summary>
public sealed class ClearScreenFieldsCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // CLRSF clears all on-screen field displays
        // Reset the screen buffer areas where fields are displayed
        ctx.Fields.ClearScreenBuffer();
        await ctx.UI.ClearScreenAsync();
    }
}

/// <summary>
/// SCRN definition - Define a screen layout for data entry.
/// </summary>
public sealed class ScreenDefCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SCRN name or SCRN row,col,text - store screen definition
        if (ctx.Tokens.Count >= 1)
        {
            string spec = ctx.JoinTokens();
            ctx.Fields.Set("__SCRN_DEF", new TasValue(TasType.Alpha, spec, 200));
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// SORT3 - Sort 3.0 command (external sort operation).
/// </summary>
public sealed class Sort3Command : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SORT3 filename,key,order - external file sort
        if (ctx.Tokens.Count >= 1)
        {
            string spec = ctx.JoinTokens();
            ctx.Fields.Set("__SORT3", new TasValue(TasType.Alpha, spec, 200));
        }
        return Task.CompletedTask;
    }
}
