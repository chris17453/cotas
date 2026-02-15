namespace CoTAS.Interpreter.Commands;

public sealed class BellCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // BELL - send bell character to terminal
        await ctx.UI.SayAsync("\a", 0, 0);
    }
}

public sealed class SoundCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SOUND freq,duration - play a sound (not meaningful in web mode)
        // Just acknowledge
        return Task.CompletedTask;
    }
}

public sealed class TraceCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // TRACE ON/OFF - toggle execution trace
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.TraceEnabled = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
        return Task.CompletedTask;
    }
}

public sealed class ClockCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // CLOCK ON/OFF - toggle clock display
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.ClockEnabled = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
        return Task.CompletedTask;
    }
}

public sealed class OnCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // ON expr GOTO label1,label2,... or ON expr GOSUB label1,label2,...
        // This is computed branching - needs interpreter support
        // Store the ON command data for the interpreter to handle
        if (ctx.Tokens.Count >= 1)
        {
            ctx.Fields.Set("__ON_EXPR", new TasValue(TasType.Alpha, ctx.JoinTokens(), 80));
        }
        return Task.CompletedTask;
    }
}

public sealed class ChainCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // CHAIN program - chain to another program (store program name)
        if (ctx.Tokens.Count >= 1)
        {
            string program = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            ctx.Fields.Set("__CHAIN_PROGRAM", new TasValue(TasType.Alpha, program, 40));
        }
        return Task.CompletedTask;
    }
}

public sealed class RunCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // RUN program - run another program (store program name)
        if (ctx.Tokens.Count >= 1)
        {
            string program = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            ctx.Fields.Set("__RUN_PROGRAM", new TasValue(TasType.Alpha, program, 40));
        }
        return Task.CompletedTask;
    }
}

public sealed class NoRestartCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.NoRestart = true;
        return Task.CompletedTask;
    }
}

public sealed class NoRedisplayCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.NoRedisplay = true;
        return Task.CompletedTask;
    }
}

public sealed class CompanyCodeCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // CO code - set company code
        if (ctx.Tokens.Count >= 1)
        {
            string code = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            ctx.Fields.CompanyCode = code;
        }
        return Task.CompletedTask;
    }
}

public sealed class OwnerCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // OWNER name - set program owner
        if (ctx.Tokens.Count >= 1)
        {
            ctx.Fields.OwnerName = ctx.JoinTokens().Trim().Trim('\'', '"');
        }
        return Task.CompletedTask;
    }
}

public sealed class ParameterCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // PARAMETER field1,field2,... - define program parameters
        // Store parameter names for use when program is chained/run
        var paramNames = new List<string>();
        foreach (var token in ctx.Tokens)
        {
            if (token.Value != ",")
                paramNames.Add(token.Value);
        }
        ctx.Fields.Set("__PARAMETERS", new TasValue(TasType.Alpha, string.Join(",", paramNames), 80));
        return Task.CompletedTask;
    }
}

public sealed class InterruptCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // INT ON/OFF - enable/disable interrupts
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.InterruptsEnabled = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
        return Task.CompletedTask;
    }
}

public sealed class AutoRunCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // AUTO_RUN program - set auto-run program name
        if (ctx.Tokens.Count >= 1)
        {
            ctx.Fields.AutoRunProgram = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
        }
        return Task.CompletedTask;
    }
}

public sealed class MenuCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // MENU prompt - display a menu prompt
        if (ctx.Tokens.Count >= 1)
        {
            string prompt = ctx.JoinTokens();
            await ctx.UI.MessageAsync($"[MENU] {prompt}");
        }
    }
}

public sealed class NMenuCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // NMENU options - display new-style menu
        if (ctx.Tokens.Count >= 1)
        {
            string options = ctx.JoinTokens();
            await ctx.UI.MessageAsync($"[NMENU] {options}");
        }
    }
}

public sealed class ListCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // LIST/RDLIST options - display list of items
        if (ctx.Tokens.Count >= 1)
        {
            string options = ctx.JoinTokens();
            await ctx.UI.MessageAsync($"[LIST] {options}");
        }
    }
}

public sealed class ExportCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // EXPORT filename [options] - export data to file
        if (ctx.Tokens.Count >= 1)
        {
            string filename = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            ctx.Fields.Set("__EXPORT_FILE", new TasValue(TasType.Alpha, filename, 80));
        }
        return Task.CompletedTask;
    }
}

public sealed class ImportCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // IMPORT filename [options] - import data from file
        if (ctx.Tokens.Count >= 1)
        {
            string filename = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            ctx.Fields.Set("__IMPORT_FILE", new TasValue(TasType.Alpha, filename, 80));
        }
        return Task.CompletedTask;
    }
}

public sealed class DateCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // DATE format - set date display format
        if (ctx.Tokens.Count >= 1)
        {
            ctx.Fields.DateFormat = ctx.JoinTokens().Trim().Trim('\'', '"').ToUpper();
        }
        return Task.CompletedTask;
    }
}

public sealed class TimeCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // TIME ON/OFF - toggle time display; TIME SET - set time format
        if (ctx.Tokens.Count >= 1)
        {
            ctx.Fields.ClockEnabled = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
        }
        return Task.CompletedTask;
    }
}
