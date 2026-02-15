namespace CoTAS.Interpreter.Commands;

/// <summary>
/// HP2 - Half page advance (print 33 blank lines, roughly half a page).
/// </summary>
public sealed class HalfPageCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // HP2 advances printer to mid-page
        ctx.Fields.PrinterRow += 33;
        if (ctx.Fields.PrintWriter != null)
        {
            for (int i = 0; i < 33; i++)
                ctx.Fields.PrintWriter.WriteLine();
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// RPTFMT format - Set report format (page length, margins, etc.).
/// </summary>
public sealed class ReportFormatCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // RPTFMT len,width,top,bot,left,right
        if (ctx.Tokens.Count >= 1)
        {
            ctx.Fields.Set("__RPTFMT", new TasValue(TasType.Alpha, ctx.JoinTokens(), 80));
            // Parse page length if present
            if (int.TryParse(ctx.Tokens[0].Value, out int pageLen))
                ctx.Fields.Set("__RPTFMT_PAGELEN", new TasValue(TasType.Integer, pageLen));
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// PRTALL ON/OFF - Print all fields mode.
/// </summary>
public sealed class PrintAllCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.PrintAll = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
        else
            ctx.Fields.PrintAll = true;
        return Task.CompletedTask;
    }
}

/// <summary>
/// CLSPF - Close print-to-disk file.
/// </summary>
public sealed class ClosePrintFileCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Fields.PrintWriter != null)
        {
            ctx.Fields.PrintWriter.Dispose();
            ctx.Fields.PrintWriter = null;
        }
        ctx.Fields.PrintToFile = "";
        return Task.CompletedTask;
    }
}

/// <summary>
/// PRSET setting - Printer setup/configuration string.
/// </summary>
public sealed class PrinterSetCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // PRSET sends control codes to printer
        if (ctx.Tokens.Count >= 1)
        {
            string setting = ctx.JoinTokens();
            ctx.Fields.Set("__PRSET", new TasValue(TasType.Alpha, setting, 80));
            // If printing to file, write the setup string
            if (ctx.Fields.PrintWriter != null)
                ctx.Fields.PrintWriter.Write(setting);
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// PRNUM n - Set printer number (select printer device).
/// </summary>
public sealed class PrinterNumCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count >= 1 && int.TryParse(ctx.Tokens[0].Value, out int num))
            ctx.Fields.Set("__PRINTER_NUM", new TasValue(TasType.Integer, num));
        return Task.CompletedTask;
    }
}
