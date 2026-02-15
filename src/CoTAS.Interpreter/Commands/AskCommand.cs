using CoTAS.Parser;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// ASK fieldname [prompt] [AT row,col]
/// Prompts user for Y/N input.
/// </summary>
public sealed class AskCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count < 1)
            throw new InterpreterException("ASK requires a field name");

        string fieldName = ctx.Tokens[0].Value;
        string prompt = fieldName;

        // Look for a string literal as prompt
        for (int i = 1; i < ctx.Tokens.Count; i++)
        {
            if (ctx.Tokens[i].Type == TokenType.StringLiteral)
            {
                prompt = ctx.Tokens[i].Value;
                break;
            }
        }

        bool result = await ctx.UI.AskAsync(prompt, false);
        ctx.Fields.Set(fieldName, new TasValue(TasType.Logical, result, 1));
    }
}
