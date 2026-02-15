using CoTAS.Storage;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// CLR FNUM n - Clear the record buffer for the specified file.
/// Resets all file fields in FieldManager to defaults.
/// </summary>
public sealed class ClrCommand : ICommandHandler
{
    private readonly StorageEngine _storage;

    public ClrCommand(StorageEngine storage) => _storage = storage;

    public Task ExecuteAsync(CommandContext ctx)
    {
        int fileNumber = SaveCommand.ResolveFnum(ctx);
        var handle = _storage.GetFile(fileNumber);
        _storage.ClearBuffer(handle);

        // Reset field values in FieldManager
        foreach (var field in handle.Schema.Fields)
        {
            ctx.Fields.Set(field.Name, TasValue.DefaultForType(field.TasTypeCode, field.NativeLength, 0));
        }

        // Update status
        ctx.Fields.Set($"__FILE_{fileNumber}_EOF", new TasValue(TasType.Logical, false));
        ctx.Fields.Set($"__FILE_{fileNumber}_ERR", new TasValue(TasType.Integer, 0));

        return Task.CompletedTask;
    }
}
