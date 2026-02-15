namespace CoTAS.Interpreter.Commands;

public sealed class ClearLineCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // CLRLNE row - clear a line on screen
        int row = ctx.Tokens.Count >= 1 && int.TryParse(ctx.Tokens[0].Value, out var r) ? r : ctx.Fields.CursorRow;
        ctx.Fields.ClearScreenLine(row);
        // Send blank line to UI
        await ctx.UI.SayAsync(new string(' ', 80), row, 1);
    }
}

public sealed class SaveScreenCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.SaveScreen();
        return Task.CompletedTask;
    }
}

public sealed class RestoreScreenCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        ctx.Fields.RestoreScreen();
        // Re-render the screen via UI
        await ctx.UI.ClearScreenAsync();
    }
}

public sealed class RedisplayCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // REDSP - redisplay screen (trigger a UI refresh)
        await ctx.UI.ClearScreenAsync();
    }
}

public sealed class WindowCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // WINDOW OPEN/CLOSE/DEFINE - store window definitions in system vars
        if (ctx.Tokens.Count < 1) return Task.CompletedTask;
        string action = ctx.Tokens[0].Value.ToUpper();
        if (action == "DEFINE" || action == "WINDEF")
        {
            // WINDOW DEFINE name,row,col,rows,cols
            if (ctx.Tokens.Count >= 2)
            {
                string name = ctx.Tokens[1].Value;
                ctx.Fields.Set($"__WIN_{name}", new TasValue(TasType.Alpha, ctx.JoinTokens(1), 80));
            }
        }
        else if (action == "OPEN" || action == "ACTIVATE")
        {
            if (ctx.Tokens.Count >= 2)
                ctx.Fields.Set("__WIN_ACTIVE", new TasValue(TasType.Alpha, ctx.Tokens[1].Value, 20));
        }
        else if (action == "CLOSE")
        {
            ctx.Fields.Set("__WIN_ACTIVE", new TasValue(TasType.Alpha, "", 20));
        }
        return Task.CompletedTask;
    }
}

public sealed class PaintCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // PAINT row,col,rows,cols,attr - paint screen area with character/attribute
        if (ctx.Tokens.Count < 4) return;
        int row = int.TryParse(ctx.Tokens[0].Value, out var r) ? r : 1;
        // Skip comma
        int colIdx = ctx.Tokens[1].Value == "," ? 2 : 1;
        int col = colIdx < ctx.Tokens.Count && int.TryParse(ctx.Tokens[colIdx].Value, out var c) ? c : 1;

        // Paint is a visual-only operation; write spaces to screen buffer
        string spaces = new string(' ', 80);
        await ctx.UI.SayAsync(spaces, row, col);
    }
}

public sealed class CursorCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // CURSOR ON/OFF or CURSOR row,col
        if (ctx.Tokens.Count < 1) return Task.CompletedTask;
        string first = ctx.Tokens[0].Value.ToUpper();
        if (first != "ON" && first != "OFF" && int.TryParse(first, out var row))
        {
            ctx.Fields.CursorRow = row;
            // Find col (skip comma)
            for (int i = 1; i < ctx.Tokens.Count; i++)
            {
                if (ctx.Tokens[i].Value != "," && int.TryParse(ctx.Tokens[i].Value, out var col))
                {
                    ctx.Fields.CursorCol = col;
                    break;
                }
            }
        }
        return Task.CompletedTask;
    }
}

public sealed class ScrollCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SCROLL direction,lines - scroll is a display-only operation
        // Store scroll request for UI
        return Task.CompletedTask;
    }
}

public sealed class ReverseCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // REV ON/OFF - toggle reverse video mode
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.ReverseVideo = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
        return Task.CompletedTask;
    }
}

public sealed class RowColorCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // ROW_COLOR row,fg,bg - store color for the row
        if (ctx.Tokens.Count >= 1 && int.TryParse(ctx.Tokens[0].Value, out var row))
        {
            string colorSpec = ctx.JoinTokens(1);
            ctx.Fields.Set($"__ROW_COLOR_{row}", new TasValue(TasType.Alpha, colorSpec, 20));
        }
        return Task.CompletedTask;
    }
}

public sealed class HotSpotCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // HOTSPOT row,col,rows,cols,key - store hotspot definition
        if (ctx.Tokens.Count >= 1)
        {
            string spec = ctx.JoinTokens();
            // Store hotspot definitions for web UI to consume
            ctx.Fields.Set("__HOTSPOT_LAST", new TasValue(TasType.Alpha, spec, 80));
        }
        return Task.CompletedTask;
    }
}

public sealed class ButtonCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // BUTTON row,col,text,key - display button text and store definition
        if (ctx.Tokens.Count >= 1)
        {
            string spec = ctx.JoinTokens();
            ctx.Fields.Set("__BUTTON_LAST", new TasValue(TasType.Alpha, spec, 80));
            // Try to extract row/col/text for display
            // Minimal: just show the text via SAY
        }
        await Task.CompletedTask;
    }
}

public sealed class CaptionCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // CAPTION text - set window caption
        if (ctx.Tokens.Count >= 1)
        {
            string text = ctx.JoinTokens();
            ctx.Fields.Set("__CAPTION", new TasValue(TasType.Alpha, text, 80));
        }
        return Task.CompletedTask;
    }
}

public sealed class GrayCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // GRAY ON/OFF - toggle gray mode
        if (ctx.Tokens.Count >= 1)
            ctx.Fields.GrayMode = ctx.Tokens[0].Value.Equals("ON", StringComparison.OrdinalIgnoreCase);
        return Task.CompletedTask;
    }
}
