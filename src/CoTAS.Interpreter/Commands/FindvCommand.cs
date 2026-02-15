using CoTAS.Parser;
using CoTAS.Storage;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// FINDV type FNUM n KEY index VAL keyvalue [ERR label] [NLOCK]
/// Find types: M=match, G=greater/equal, F=first, L=last, N=next, P=previous
///
/// FNUM is typically a field variable holding the file number assigned by OPENV.
/// </summary>
public sealed class FindvCommand : ICommandHandler
{
    private readonly StorageEngine _storage;

    public FindvCommand(StorageEngine storage) => _storage = storage;

    public async Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count < 1)
            throw new InterpreterException("FINDV requires a find type");

        string findType = ctx.Tokens[0].Value.ToUpperInvariant();
        int fileNumber = 0;
        int keyIndex = 1;
        string? keyValue = null;
        string? errLabel = null;

        for (int i = 1; i < ctx.Tokens.Count; i++)
        {
            string tv = ctx.Tokens[i].Value.ToUpperInvariant();

            // FNUM fieldname_or_number
            if (tv == "FNUM" && i + 1 < ctx.Tokens.Count)
            {
                i++;
                fileNumber = ResolveFileNumber(ctx.Fields, ctx.Tokens[i].Value);
            }
            // KEY index_number or KEY @n
            else if (tv == "KEY" && i + 1 < ctx.Tokens.Count)
            {
                i++;
                string keyRef = ctx.Tokens[i].Value;
                if (keyRef.StartsWith("@") && int.TryParse(keyRef[1..], out var ki))
                    keyIndex = ki;
                else if (int.TryParse(keyRef, out var ki2))
                    keyIndex = ki2;
                // If it's a field name reference for a key name, use index 1
            }
            // VAL keyvalue (may be field reference, literal, or expression)
            else if (tv == "VAL" && i + 1 < ctx.Tokens.Count)
            {
                i++;
                keyValue = ResolveValue(ctx, i);
            }
            // ERR label
            else if (tv == "ERR" && i + 1 < ctx.Tokens.Count)
            {
                i++;
                errLabel = ctx.Tokens[i].Value;
            }
        }

        var handle = _storage.GetFile(fileNumber);
        await _storage.FindAsync(handle, findType, keyIndex, keyValue, errLabel);

        // Update file status system variables
        ctx.Fields.Set($"__FILE_{fileNumber}_EOF", new TasValue(TasType.Logical, handle.IsEof));
        ctx.Fields.Set($"__FILE_{fileNumber}_ERR", new TasValue(TasType.Integer, handle.LastError));

        // Populate FieldManager from record buffer
        if (handle.HasRecord)
        {
            PopulateFields(ctx.Fields, handle);
            ctx.Fields.Set($"__FILE_{fileNumber}_RCN", new TasValue(TasType.Integer, handle.CurrentRecordId));
        }
    }

    /// <summary>
    /// Resolve a FNUM token to an integer file number.
    /// The token may be a literal number or a field variable name.
    /// </summary>
    internal static int ResolveFileNumber(FieldManager fields, string token)
    {
        if (fields.IsDefined(token))
            return (int)fields.Get(token).AsNumeric();
        if (int.TryParse(token, out var n))
            return n;
        return 0;
    }

    /// <summary>
    /// Resolve a VAL token to a string key value.
    /// </summary>
    private static string ResolveValue(CommandContext ctx, int tokenIndex)
    {
        if (tokenIndex >= ctx.Tokens.Count) return "";
        var token = ctx.Tokens[tokenIndex];
        if (token.Type == TokenType.StringLiteral)
            return token.Value;
        if (ctx.Fields.IsDefined(token.Value))
            return ctx.Fields.Get(token.Value).AsString().TrimEnd();
        return token.Value;
    }

    /// <summary>
    /// Copy record buffer values into FieldManager fields.
    /// </summary>
    internal static void PopulateFields(FieldManager fields, FileHandle handle)
    {
        var values = handle.Buffer.GetAll();
        foreach (var field in handle.Schema.Fields)
        {
            if (values.TryGetValue(field.Name, out var val) && val != null)
            {
                var tasValue = field.NativeType switch
                {
                    NativeType.String or NativeType.Variable =>
                        new TasValue(TasType.Alpha, val.ToString() ?? "", field.NativeLength),
                    NativeType.Integer or NativeType.SmallInt =>
                        new TasValue(TasType.Integer, Convert.ToInt32(val)),
                    NativeType.Numeric =>
                        new TasValue(TasType.Numeric, Convert.ToDouble(val)),
                    NativeType.Date =>
                        new TasValue(TasType.Date, val.ToString() ?? "", 8),
                    _ => new TasValue(TasType.Alpha, val.ToString() ?? "", field.NativeLength),
                };
                fields.Set(field.Name, tasValue);
            }
        }
    }
}
