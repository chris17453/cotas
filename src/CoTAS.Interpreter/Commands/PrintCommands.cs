namespace CoTAS.Interpreter.Commands;

public sealed class PrintOnCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // PON - printer on (start sending output to printer/file)
        ctx.Fields.PrinterOn = true;
        ctx.Fields.PrinterRow = 1;
        ctx.Fields.PrinterCol = 1;
        return Task.CompletedTask;
    }
}

public sealed class PrintOffCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // POFF - printer off
        ctx.Fields.PrinterOn = false;
        // Close print-to-disk file if open
        if (ctx.Fields.PrintWriter != null)
        {
            ctx.Fields.PrintWriter.Dispose();
            ctx.Fields.PrintWriter = null;
            ctx.Fields.PrintToFile = "";
        }
        return Task.CompletedTask;
    }
}

public sealed class PrintTopOfFormCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // TOF - top of form / form feed
        ctx.Fields.PrinterRow = 1;
        ctx.Fields.PrinterCol = 1;
        if (ctx.Fields.PrintWriter != null)
            ctx.Fields.PrintWriter.Write('\f');
        return Task.CompletedTask;
    }
}

public sealed class PrintBlankLinesCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // P n - print n blank lines
        int count = 1;
        if (ctx.Tokens.Count >= 1 && int.TryParse(ctx.Tokens[0].Value, out var n))
            count = n;
        ctx.Fields.PrinterRow += count;
        if (ctx.Fields.PrintWriter != null)
        {
            for (int i = 0; i < count; i++)
                ctx.Fields.PrintWriter.WriteLine();
        }
        return Task.CompletedTask;
    }
}

public sealed class PrintBoxCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // PRTBOX row,col,rows,cols - print a box (in print mode, output box chars)
        // Acknowledge; actual rendering is print-mode specific
        return Task.CompletedTask;
    }
}

public sealed class PrintToCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // PRTO filename - redirect print to disk file
        if (ctx.Tokens.Count >= 1)
        {
            string filename = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            // Close previous file if open
            ctx.Fields.PrintWriter?.Dispose();
            try
            {
                ctx.Fields.PrintToFile = filename;
                ctx.Fields.PrintWriter = new StreamWriter(filename, false);
                ctx.Fields.PrinterOn = true;
            }
            catch
            {
                ctx.Fields.PrintToFile = "";
                ctx.Fields.PrintWriter = null;
            }
        }
        return Task.CompletedTask;
    }
}
