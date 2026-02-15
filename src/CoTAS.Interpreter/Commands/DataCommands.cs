namespace CoTAS.Interpreter.Commands;

public sealed class FormatCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // FORMAT fieldname picture - set field display format
        if (ctx.Tokens.Count >= 2)
        {
            string fieldName = ctx.Tokens[0].Value;
            string format = ctx.JoinTokens(1);
            ctx.Fields.SetFormat(fieldName, format);
        }
        return Task.CompletedTask;
    }
}

public sealed class ReenterCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // REENT - signal that the current field should be re-entered
        ctx.Fields.ReenterRequested = true;
        return Task.CompletedTask;
    }
}

public sealed class ForceCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // FORCE fieldname - force data entry into specified field
        if (ctx.Tokens.Count >= 1)
        {
            string fieldName = ctx.Tokens[0].Value;
            ctx.Fields.ForceEntry = true;
            ctx.Fields.ForceField = fieldName;
        }
        return Task.CompletedTask;
    }
}

public sealed class PictureCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // PICTURE fieldname mask - set input picture/mask for a field
        if (ctx.Tokens.Count >= 2)
        {
            string fieldName = ctx.Tokens[0].Value;
            string picture = ctx.JoinTokens(1);
            ctx.Fields.SetPicture(fieldName, picture);
        }
        return Task.CompletedTask;
    }
}

public sealed class NoValidMsgCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // NOVLDMSG - suppress the next validation message
        ctx.Fields.NoValidMessage = true;
        return Task.CompletedTask;
    }
}

public sealed class TrimFieldCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // TRIM fieldname - trim trailing spaces from field
        if (ctx.Tokens.Count >= 1)
        {
            string fieldName = ctx.Tokens[0].Value;
            try
            {
                var val = ctx.Fields.Get(fieldName);
                string trimmed = val.AsString().TrimEnd();
                ctx.Fields.Set(fieldName, new TasValue(TasType.Alpha, trimmed, trimmed.Length));
            }
            catch { /* ignore if field doesn't exist */ }
        }
        return Task.CompletedTask;
    }
}

public sealed class UpcaseFieldCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // UPCASE fieldname - convert field to uppercase
        if (ctx.Tokens.Count >= 1)
        {
            string fieldName = ctx.Tokens[0].Value;
            try
            {
                var val = ctx.Fields.Get(fieldName);
                string upper = val.AsString().ToUpperInvariant();
                ctx.Fields.Set(fieldName, new TasValue(TasType.Alpha, upper, val.Size));
            }
            catch { /* ignore if field doesn't exist */ }
        }
        return Task.CompletedTask;
    }
}

public sealed class JustifyFieldCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // JUSTIFY fieldname L/R/C - justify field content in place
        if (ctx.Tokens.Count >= 2)
        {
            string fieldName = ctx.Tokens[0].Value;
            string alignment = ctx.Tokens[1].Value.ToUpper();
            if (alignment == "," && ctx.Tokens.Count >= 3)
                alignment = ctx.Tokens[2].Value.ToUpper();
            ctx.Fields.JustifyField(fieldName, alignment);
        }
        return Task.CompletedTask;
    }
}

public sealed class RedefineCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // REDEFINE fieldname TYPE x SIZE n [DEC d]
        if (ctx.Tokens.Count < 1) return Task.CompletedTask;
        string fieldName = ctx.Tokens[0].Value;
        string? typeCode = null;
        int size = 10;
        int decimals = 0;

        int typeIdx = ctx.FindToken("TYPE");
        if (typeIdx >= 0 && typeIdx + 1 < ctx.Tokens.Count)
            typeCode = ctx.Tokens[typeIdx + 1].Value;

        int sizeIdx = ctx.FindToken("SIZE");
        if (sizeIdx >= 0 && sizeIdx + 1 < ctx.Tokens.Count)
            int.TryParse(ctx.Tokens[sizeIdx + 1].Value, out size);

        int decIdx = ctx.FindToken("DEC");
        if (decIdx >= 0 && decIdx + 1 < ctx.Tokens.Count)
            int.TryParse(ctx.Tokens[decIdx + 1].Value, out decimals);

        ctx.Fields.Redefine(fieldName, typeCode, size, decimals);
        return Task.CompletedTask;
    }
}

public sealed class ReplaceCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // REPLACE fieldname WITH value
        if (ctx.Tokens.Count >= 3)
        {
            string fieldName = ctx.Tokens[0].Value;
            int withIdx = ctx.FindToken("WITH");
            if (withIdx >= 0 && withIdx + 1 < ctx.Tokens.Count)
            {
                // Try to evaluate the expression after WITH
                string valueStr = ctx.JoinTokens(withIdx + 1);
                // If it looks like a field reference, get the field value
                if (ctx.Fields.IsDefined(valueStr.Trim()))
                {
                    ctx.Fields.Set(fieldName, ctx.Fields.Get(valueStr.Trim()).Clone());
                }
                else
                {
                    ctx.Fields.Set(fieldName, new TasValue(TasType.Alpha, valueStr, valueStr.Length));
                }
            }
        }
        return Task.CompletedTask;
    }
}

public sealed class RemoveCommand : ICommandHandler
{
    public Task ExecuteAsync(CommandContext ctx)
    {
        // REMOVE fieldname - remove/deallocate field
        if (ctx.Tokens.Count >= 1)
        {
            string fieldName = ctx.Tokens[0].Value;
            ctx.Fields.Remove(fieldName);
        }
        return Task.CompletedTask;
    }
}
