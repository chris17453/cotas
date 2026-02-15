using CoTAS.Storage;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// SAVE FNUM n [NOCNF] [NOCLR]
/// Writes the current record buffer to the database.
/// Copies FieldManager values back to record buffer before saving.
/// </summary>
public sealed class SaveCommand : ICommandHandler
{
    private readonly StorageEngine _storage;

    public SaveCommand(StorageEngine storage) => _storage = storage;

    public async Task ExecuteAsync(CommandContext ctx)
    {
        int fileNumber = ResolveFnum(ctx);
        var handle = _storage.GetFile(fileNumber);

        // Copy current field values back to record buffer before saving
        foreach (var field in handle.Schema.Fields)
        {
            if (ctx.Fields.IsDefined(field.Name))
            {
                var val = ctx.Fields.Get(field.Name);
                object dbVal = field.NativeType switch
                {
                    NativeType.String or NativeType.Variable => val.AsString(),
                    NativeType.Integer or NativeType.SmallInt => val.AsInteger(),
                    NativeType.Numeric => val.AsNumeric(),
                    NativeType.Date => val.AsString(),
                    _ => val.AsString(),
                };
                handle.Buffer.Set(field.Name, dbVal);
            }
        }

        await _storage.SaveAsync(handle);

        // Update status
        ctx.Fields.Set($"__FILE_{fileNumber}_ERR", new TasValue(TasType.Integer, 0));
    }

    internal static int ResolveFnum(CommandContext ctx)
    {
        int fnumIdx = ctx.FindToken("FNUM");
        if (fnumIdx >= 0 && fnumIdx + 1 < ctx.Tokens.Count)
            return FindvCommand.ResolveFileNumber(ctx.Fields, ctx.Tokens[fnumIdx + 1].Value);
        // Try first token as file number/reference
        if (ctx.Tokens.Count >= 1)
            return FindvCommand.ResolveFileNumber(ctx.Fields, ctx.Tokens[0].Value);
        return 0;
    }
}
