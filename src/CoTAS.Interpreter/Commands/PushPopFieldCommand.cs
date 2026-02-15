namespace CoTAS.Interpreter.Commands;

public sealed class PushFieldCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // PUSHF fieldname[,fieldname2,...] - Push field values onto stack
        var fieldNames = new List<string>();
        foreach (var token in ctx.Tokens)
        {
            if (token.Value != "," && !string.IsNullOrWhiteSpace(token.Value))
                fieldNames.Add(token.Value);
        }
        if (fieldNames.Count > 0)
            ctx.Fields.PushFields(fieldNames);
        return Task.CompletedTask;
    }
}

public sealed class PopFieldCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // POPF - Pop field values from stack (restores last PUSHF)
        ctx.Fields.PopFields();
        return Task.CompletedTask;
    }
}
