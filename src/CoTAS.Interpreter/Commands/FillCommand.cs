namespace CoTAS.Interpreter.Commands;

/// <summary>
/// FILL fieldname value
/// Fills a field with a specified value/character.
/// </summary>
public sealed class FillCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count < 2)
            throw new InterpreterException("FILL requires a field name and value");

        string fieldName = ctx.Tokens[0].Value;
        string fillValue = ctx.Tokens[1].Value;

        if (ctx.Fields.IsDefined(fieldName))
        {
            var field = ctx.Fields.Get(fieldName);
            if (field.Type == TasType.Alpha && fillValue.Length == 1)
            {
                string filled = new string(fillValue[0], field.Size > 0 ? field.Size : 1);
                ctx.Fields.Set(fieldName, new TasValue(TasType.Alpha, filled, field.Size));
            }
            else
            {
                ctx.Fields.Set(fieldName, new TasValue(field.Type, fillValue, field.Size));
            }
        }

        return Task.CompletedTask;
    }
}
