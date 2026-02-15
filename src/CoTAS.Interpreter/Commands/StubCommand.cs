namespace CoTAS.Interpreter.Commands;

/// <summary>
/// Stub command that does nothing. Used for commands that are recognized
/// but not yet implemented (prevents "unknown command" errors).
/// </summary>
public sealed class StubCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx) => Task.CompletedTask;
}
