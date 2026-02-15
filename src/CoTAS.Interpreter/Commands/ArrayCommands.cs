namespace CoTAS.Interpreter.Commands;

public sealed class ArrayReadCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // RDA arrayname FNUM handle KEY keyexpr [START n] [COUNT c]
        // Reads data from a file into an array
        // Without storage wired up, we acknowledge the command and set array from file handle state
        return Task.CompletedTask;
    }
}

public sealed class ArrayWriteCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // WRA arrayname FNUM handle KEY keyexpr
        // Writes array data to a file
        return Task.CompletedTask;
    }
}

public sealed class ArrayUpdateCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // UDA arrayname FNUM handle KEY keyexpr
        // Updates array data in a file
        return Task.CompletedTask;
    }
}

public sealed class ArraySortCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // SORT arrayname [DSC]
        if (ctx.Tokens.Count < 1) return Task.CompletedTask;
        string arrayName = ctx.Tokens[0].Value;
        bool descending = ctx.HasToken("DSC") || ctx.HasToken("DESC");
        ctx.Fields.SortArray(arrayName, descending);
        return Task.CompletedTask;
    }
}

public sealed class ArrayRemoveCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // RMVA arrayname,index - remove element at index
        if (ctx.Tokens.Count < 1) return Task.CompletedTask;
        string arrayName = ctx.Tokens[0].Value;
        int index = 1;
        // Find index after comma
        for (int i = 1; i < ctx.Tokens.Count; i++)
        {
            if (ctx.Tokens[i].Value != "," && int.TryParse(ctx.Tokens[i].Value, out var idx))
            {
                index = idx;
                break;
            }
        }
        ctx.Fields.RemoveArrayElement(arrayName, index);
        return Task.CompletedTask;
    }
}

public sealed class ArrayDisplayCommand : ICommandHandler
{
    public async Task ExecuteAsync(CommandContext ctx)
    {
        // DSPA arrayname [AT row,col] - display array fields on screen
        if (ctx.Tokens.Count < 1) return;
        string arrayName = ctx.Tokens[0].Value;
        int startRow = 1, startCol = 1;

        int atIdx = ctx.FindToken("AT");
        if (atIdx >= 0 && atIdx + 1 < ctx.Tokens.Count)
        {
            int.TryParse(ctx.Tokens[atIdx + 1].Value, out startRow);
            for (int i = atIdx + 2; i < ctx.Tokens.Count; i++)
            {
                if (ctx.Tokens[i].Value != "," && int.TryParse(ctx.Tokens[i].Value, out var col))
                {
                    startCol = col;
                    break;
                }
            }
        }

        int size = ctx.Fields.GetArraySize(arrayName);
        for (int i = 1; i <= size && startRow + i - 1 <= 25; i++)
        {
            try
            {
                var val = ctx.Fields.GetArrayElement(arrayName, i);
                await ctx.UI.SayAsync(val.AsString(), startRow + i - 1, startCol);
            }
            catch { break; }
        }
    }
}

public sealed class ArrayDeallocateCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // DLCA arrayname - deallocate array
        if (ctx.Tokens.Count >= 1)
        {
            string arrayName = ctx.Tokens[0].Value;
            ctx.Fields.DeallocateArray(arrayName);
        }
        return Task.CompletedTask;
    }
}
