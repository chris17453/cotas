namespace CoTAS.Interpreter.Commands;

public interface ICommandHandler
{
    Task ExecuteAsync(CommandContext ctx);
}
