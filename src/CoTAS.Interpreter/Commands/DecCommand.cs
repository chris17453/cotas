namespace CoTAS.Interpreter.Commands;

/// <summary>
/// DEC fieldname [amount]
/// Decrements a numeric field by 1 or by the specified amount.
/// </summary>
public sealed class DecCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count < 1)
            throw new InterpreterException("DEC requires a field name");

        string fieldName = ctx.Tokens[0].Value;
        double amount = 1;

        if (ctx.Tokens.Count >= 2)
            amount = double.TryParse(ctx.Tokens[1].Value, out var v) ? v : 1;

        var current = ctx.Fields.Get(fieldName);
        var newValue = new TasValue(current.Type, current.AsNumeric() - amount, current.Size, current.Decimals);
        ctx.Fields.Set(fieldName, newValue);

        return Task.CompletedTask;
    }
}
