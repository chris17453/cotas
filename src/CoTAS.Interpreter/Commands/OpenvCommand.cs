using CoTAS.Parser;
using CoTAS.Storage;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// OPENV filename FNUM n [LOCK mode] [PATH path] [ERR label] [CREATE] [NLOCK]
/// Opens a TAS file by name and assigns a file number.
/// Registers all file fields in FieldManager.
///
/// In TAS, FNUM is typically a field variable (e.g., "bksymstr_hndl") that holds
/// an integer file handle. The storage engine assigns the next available number
/// if the field is 0, or uses the existing value.
///
/// The filename maps to a DDF .int definition → MSSQL table.
/// </summary>
public sealed class OpenvCommand : ICommandHandler
{
    private readonly StorageEngine _storage;

    public OpenvCommand(StorageEngine storage) => _storage = storage;

    public async Task ExecuteAsync(CommandContext ctx)
    {
        if (ctx.Tokens.Count < 1)
            throw new InterpreterException("OPENV requires a filename");

        // First token is filename (may be quoted)
        string fileName = ctx.Tokens[0].Value.Trim('\'', '"');
        int fileNumber = 0;
        string lockMode = "NONE";
        string? path = null;
        string? errLabel = null;

        for (int i = 1; i < ctx.Tokens.Count; i++)
        {
            string tv = ctx.Tokens[i].Value.ToUpperInvariant();

            // FNUM fieldname_or_number
            if (tv == "FNUM" && i + 1 < ctx.Tokens.Count)
            {
                i++;
                string fnumVal = ctx.Tokens[i].Value;
                // FNUM is usually a field reference like "bksymstr_hndl"
                if (ctx.Fields.IsDefined(fnumVal))
                {
                    int existing = (int)ctx.Fields.Get(fnumVal).AsNumeric();
                    if (existing > 0)
                    {
                        fileNumber = existing;
                    }
                    else
                    {
                        // Auto-assign next file number
                        fileNumber = GetNextFileNumber();
                        ctx.Fields.Set(fnumVal, new TasValue(TasType.Integer, fileNumber));
                    }
                }
                else if (int.TryParse(fnumVal, out var n))
                {
                    fileNumber = n;
                }
                else
                {
                    // Field not yet defined — define it and assign a number
                    fileNumber = GetNextFileNumber();
                    ctx.Fields.Define(fnumVal, "I", 5, 0, 0);
                    ctx.Fields.Set(fnumVal, new TasValue(TasType.Integer, fileNumber));
                }
            }
            // LOCK mode (N=none, R=read, F=full, or the keywords LOCK/NLOCK)
            else if (tv == "LOCK" && i + 1 < ctx.Tokens.Count)
            {
                i++;
                string mode = ctx.Tokens[i].Value.ToUpperInvariant();
                lockMode = mode switch
                {
                    "N" => "NONE",
                    "R" => "READ",
                    "F" => "FULL",
                    _ => mode,
                };
            }
            else if (tv == "NLOCK")
            {
                lockMode = "NONE";
            }
            // PATH directory
            else if (tv == "PATH" && i + 1 < ctx.Tokens.Count)
            {
                i++;
                path = ctx.Tokens[i].Value.Trim('\'', '"');
                // If path is a field reference, resolve it
                if (ctx.Fields.IsDefined(path))
                    path = ctx.Fields.Get(path).AsString().TrimEnd();
            }
            // ERR label
            else if (tv == "ERR" && i + 1 < ctx.Tokens.Count)
            {
                i++;
                errLabel = ctx.Tokens[i].Value;
            }
            // Skip commas and other tokens (CREATE, TYPE, SIZE, BUFF, etc.)
        }

        if (fileNumber == 0)
            fileNumber = GetNextFileNumber();

        try
        {
            var handle = await _storage.OpenFileAsync(fileName, fileNumber, lockMode);

            // Register all file fields in FieldManager
            foreach (var field in handle.Schema.Fields)
            {
                ctx.Fields.Define(field.Name, field.TasTypeCode, field.NativeLength, 0, 0);
            }

            // Set file status system variables
            ctx.Fields.Set($"__FILE_{fileNumber}_NAME", new TasValue(TasType.Alpha, fileName, 40));
            ctx.Fields.Set($"__FILE_{fileNumber}_EOF", new TasValue(TasType.Logical, false));
            ctx.Fields.Set($"__FILE_{fileNumber}_ERR", new TasValue(TasType.Integer, 0));
            ctx.Fields.Set($"__FILE_{fileNumber}_RCN", new TasValue(TasType.Integer, 0));
        }
        catch (StorageException)
        {
            // If ERR label specified, the interpreter will handle the jump
            // For now, set error status
            ctx.Fields.Set($"__FILE_{fileNumber}_ERR", new TasValue(TasType.Integer, 1));
        }
    }

    private int GetNextFileNumber()
    {
        // Find the next available file number starting from 1
        for (int i = 1; i <= 999; i++)
        {
            try { _storage.GetFile(i); }
            catch { return i; }
        }
        return 1;
    }
}
