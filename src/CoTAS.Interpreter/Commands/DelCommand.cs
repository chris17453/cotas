using CoTAS.Storage;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// DEL FNUM n - Delete the current record from the database.
/// </summary>
public sealed class DelCommand : ICommandHandler
{
    private readonly StorageEngine _storage;

    public DelCommand(StorageEngine storage) => _storage = storage;

    public async Task ExecuteAsync(CommandContext ctx)
    {
        int fileNumber = SaveCommand.ResolveFnum(ctx);
        var handle = _storage.GetFile(fileNumber);
        await _storage.DeleteAsync(handle);

        // Update status
        ctx.Fields.Set($"__FILE_{fileNumber}_ERR", new TasValue(TasType.Integer, handle.LastError));
    }
}
