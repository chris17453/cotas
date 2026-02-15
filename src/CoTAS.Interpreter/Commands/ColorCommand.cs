namespace CoTAS.Interpreter.Commands;

public sealed class ColorCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // COLOR fg,bg or COLOR ARRAY fieldname
        if (ctx.Tokens.Count < 1) return Task.CompletedTask;

        string first = ctx.Tokens[0].Value.ToUpper();
        if (first == "ARRAY" && ctx.Tokens.Count >= 2)
        {
            // COLOR ARRAY fieldname - store the color array field name
            ctx.Fields.Set("__COLOR_ARRAY", new TasValue(TasType.Alpha, ctx.Tokens[1].Value, 20));
        }
        else
        {
            // COLOR fg,bg or COLOR fg - store foreground/background
            ctx.Fields.Set("__COLOR_FG", new TasValue(TasType.Alpha, first, 10));
            // Look for background after comma
            for (int i = 1; i < ctx.Tokens.Count; i++)
            {
                if (ctx.Tokens[i].Value != ",")
                {
                    ctx.Fields.Set("__COLOR_BG", new TasValue(TasType.Alpha, ctx.Tokens[i].Value, 10));
                    break;
                }
            }
        }
        return Task.CompletedTask;
    }
}
