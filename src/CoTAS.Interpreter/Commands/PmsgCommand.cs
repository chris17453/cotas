using CoTAS.Parser;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// PMSG text [AT row,col]
/// Positioned message display.
/// </summary>
public sealed class PmsgCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        string text = "";
        int row = 0, col = 0;

        // Collect text from tokens (string literals and identifiers)
        for (int i = 0; i < ctx.Tokens.Count; i++)
        {
            if (ctx.Tokens[i].Value.Equals("AT", StringComparison.OrdinalIgnoreCase))
                break;
            if (ctx.Tokens[i].Type == TokenType.StringLiteral)
                text += ctx.Tokens[i].Value;
            else if (ctx.Tokens[i].Type == TokenType.Identifier && ctx.Fields.IsDefined(ctx.Tokens[i].Value))
                text += ctx.Fields.Get(ctx.Tokens[i].Value).AsString();
        }

        int atIdx = ctx.FindToken("AT");
        if (atIdx >= 0 && atIdx + 2 < ctx.Tokens.Count)
        {
            int.TryParse(ctx.Tokens[atIdx + 1].Value, out row);
            int colIdx = atIdx + 2;
            if (ctx.Tokens[colIdx].Type == TokenType.Comma && colIdx + 1 < ctx.Tokens.Count)
                colIdx++;
            int.TryParse(ctx.Tokens[colIdx].Value, out col);
        }

        await ctx.UI.SayAsync(text, row, col);
    }
}
