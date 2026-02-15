namespace CoTAS.Interpreter.Commands;

public sealed class SrchCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SRCH @handle KEY keyexpr [NLOCK]
        // Alternative to FINDV for searching files
        // Without full storage integration, parse and store the search parameters
        if (ctx.Tokens.Count >= 1)
        {
            string handleRef = ctx.Tokens[0].Value;
            ctx.Fields.Set("__SRCH_HANDLE", new TasValue(TasType.Alpha, handleRef, 20));
            int keyIdx = ctx.FindToken("KEY");
            if (keyIdx >= 0 && keyIdx + 1 < ctx.Tokens.Count)
                ctx.Fields.Set("__SRCH_KEY", new TasValue(TasType.Alpha, ctx.Tokens[keyIdx + 1].Value, 40));
        }
        return Task.CompletedTask;
    }
}

public sealed class ReadRecordCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // READ @handle or READ NEXT/PREV @handle
        // Reads the current record from a file into the field manager
        if (ctx.Tokens.Count >= 1)
        {
            string first = ctx.Tokens[0].Value.ToUpper();
            ctx.Fields.Set("__READ_MODE", new TasValue(TasType.Alpha, first, 10));
        }
        return Task.CompletedTask;
    }
}

public sealed class WriteRecordCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // WRITE @handle - write record buffer to file
        if (ctx.Tokens.Count >= 1)
        {
            string handleRef = ctx.Tokens[0].Value;
            ctx.Fields.Set("__WRITE_HANDLE", new TasValue(TasType.Alpha, handleRef, 20));
        }
        return Task.CompletedTask;
    }
}

public sealed class SetActiveCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SET_ACTIVE @handle ON/OFF - set record active/inactive flag
        if (ctx.Tokens.Count >= 1)
        {
            string handleRef = ctx.Tokens[0].Value;
            bool active = !ctx.HasToken("OFF"); // default is ON
            ctx.Fields.Set("__SET_ACTIVE", new TasValue(TasType.Logical, active));
        }
        return Task.CompletedTask;
    }
}

public sealed class ReopenFileCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // REOPEN @handle [LOCK mode] - reopen a file with different lock mode
        if (ctx.Tokens.Count >= 1)
        {
            string handleRef = ctx.Tokens[0].Value;
            ctx.Fields.Set("__REOPEN_HANDLE", new TasValue(TasType.Alpha, handleRef, 20));
        }
        return Task.CompletedTask;
    }
}

public sealed class InitFileCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // INIT filename - initialize/create an empty TAS file
        if (ctx.Tokens.Count >= 1)
        {
            string filename = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            ctx.Fields.Set("__INIT_FILE", new TasValue(TasType.Alpha, filename, 80));
        }
        return Task.CompletedTask;
    }
}

public sealed class DeleteFileCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // DELF filename - delete file from disk
        if (ctx.Tokens.Count >= 1)
        {
            string filename = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            try { File.Delete(filename); } catch { /* ignore */ }
        }
        return Task.CompletedTask;
    }
}

public sealed class RenameFileCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // RENF oldname newname - rename file on disk
        if (ctx.Tokens.Count >= 2)
        {
            string oldName = ctx.Tokens[0].Value.Trim().Trim('\'', '"');
            // Skip comma if present
            int newIdx = ctx.Tokens[1].Value == "," ? 2 : 1;
            if (newIdx < ctx.Tokens.Count)
            {
                string newName = ctx.Tokens[newIdx].Value.Trim().Trim('\'', '"');
                try { File.Move(oldName, newName); } catch { /* ignore */ }
            }
        }
        return Task.CompletedTask;
    }
}

public sealed class FilterCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // FILTER @handle expression - set record filter for file reads
        if (ctx.Tokens.Count >= 1)
        {
            string filter = ctx.JoinTokens();
            ctx.Fields.Set("__FILTER", new TasValue(TasType.Alpha, filter, 200));
        }
        return Task.CompletedTask;
    }
}

public sealed class RelateCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // RELATE @handle1 TO @handle2 USING keyexpr - relate two files
        if (ctx.Tokens.Count >= 1)
        {
            string relate = ctx.JoinTokens();
            ctx.Fields.Set("__RELATE", new TasValue(TasType.Alpha, relate, 200));
        }
        return Task.CompletedTask;
    }
}

public sealed class TransactionCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // TRANSACTION BEGIN/END/ROLLBACK
        if (ctx.Tokens.Count >= 1)
        {
            string action = ctx.Tokens[0].Value.ToUpper();
            ctx.Fields.Set("__TRANSACTION", new TasValue(TasType.Alpha, action, 10));
        }
        return Task.CompletedTask;
    }
}
