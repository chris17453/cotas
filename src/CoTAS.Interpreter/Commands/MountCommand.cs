namespace CoTAS.Interpreter.Commands;

/// <summary>
/// MOUNT path
/// Sets the data path for file operations.
/// </summary>
public sealed class MountCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // MOUNT path - set data path
        // Store the mount path for file operations
        if (ctx.Tokens.Count >= 1)
        {
            string path = ctx.JoinTokens();
            // Store as a system variable that OPENV can reference
            ctx.Fields.Set("__MOUNT_PATH", new TasValue(TasType.Alpha, path.Trim().Trim('\'', '"'), 80));
        }
        return Task.CompletedTask;
    }
}
