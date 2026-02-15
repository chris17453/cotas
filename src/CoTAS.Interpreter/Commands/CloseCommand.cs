using CoTAS.Storage;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// CLOSE FNUM n - Close an open file handle and release resources.
/// </summary>
public sealed class CloseCommand : ICommandHandler
{
    private readonly StorageEngine _storage;

    public CloseCommand(StorageEngine storage) => _storage = storage;

    public Task ExecuteAsync(CommandContext ctx)
    {
        int fileNumber = SaveCommand.ResolveFnum(ctx);
        _storage.CloseFile(fileNumber);
        return Task.CompletedTask;
    }
}
