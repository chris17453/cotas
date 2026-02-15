namespace CoTAS.Interpreter.Commands;

/// <summary>
/// GETLBL fieldname
/// Gets the label name at the current program position.
/// </summary>
public sealed class GetlblCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // GETLBL fieldname - get label name (stub: set to empty)
        if (ctx.Tokens.Count >= 1)
        {
            string fieldName = ctx.Tokens[0].Value;
            ctx.Fields.Set(fieldName, new TasValue(TasType.Alpha, "", 14));
        }
        return Task.CompletedTask;
    }
}
