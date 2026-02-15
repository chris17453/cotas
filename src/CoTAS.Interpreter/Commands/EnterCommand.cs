using CoTAS.Parser;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// ENTER fieldname [AT row,col] [SIZE n]
/// Data entry for a field.
/// </summary>
public sealed class EnterCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count < 1)
            throw new InterpreterException("ENTER requires a field name");

        string fieldName = ctx.Tokens[0].Value;
        int row = 0, col = 0, size = 10;

        // Try to get field size from definition
        if (ctx.Fields.IsDefined(fieldName))
        {
            var field = ctx.Fields.Get(fieldName);
            size = field.Size > 0 ? field.Size : 10;
        }

        // Parse AT row,col
        int atIdx = ctx.FindToken("AT");
        if (atIdx >= 0 && atIdx + 2 < ctx.Tokens.Count)
        {
            int.TryParse(ctx.Tokens[atIdx + 1].Value, out row);
            // Skip comma
            int colIdx = atIdx + 2;
            if (ctx.Tokens[colIdx].Type == TokenType.Comma && colIdx + 1 < ctx.Tokens.Count)
                colIdx++;
            int.TryParse(ctx.Tokens[colIdx].Value, out col);
        }

        // Parse SIZE
        int sizeIdx = ctx.FindToken("SIZE");
        if (sizeIdx >= 0 && sizeIdx + 1 < ctx.Tokens.Count)
            int.TryParse(ctx.Tokens[sizeIdx + 1].Value, out size);

        string input = await ctx.UI.EnterAsync(fieldName, row, col, size);

        // Set the field value
        if (ctx.Fields.IsDefined(fieldName))
        {
            var existing = ctx.Fields.Get(fieldName);
            if (existing.Type == TasType.Numeric || existing.Type == TasType.Integer)
            {
                if (double.TryParse(input, out var num))
                    ctx.Fields.Set(fieldName, new TasValue(existing.Type, num, existing.Size, existing.Decimals));
            }
            else
            {
                ctx.Fields.Set(fieldName, new TasValue(existing.Type, input, existing.Size));
            }
        }
        else
        {
            ctx.Fields.Set(fieldName, new TasValue(TasType.Alpha, input, input.Length));
        }
    }
}
