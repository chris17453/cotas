using CoTAS.Interpreter;

namespace CoTAS.Bridge;

public sealed class ConsoleBridge : IUIBridge
{
    public Task ClearScreenAsync()
    {
        try { Console.Clear(); } catch { /* not supported in redirected output */ }
        return Task.CompletedTask;
    }

    public Task SayAsync(string text, int row, int col)
    {
        try
        {
            if (row > 0 && col > 0)
                Console.SetCursorPosition(col - 1, row - 1);
        }
        catch
        {
            // Cursor positioning not supported (redirected/piped output)
        }
        Console.Write(text);
        return Task.CompletedTask;
    }

    public Task MessageAsync(string text)
    {
        Console.WriteLine();
        Console.WriteLine(text);
        return Task.CompletedTask;
    }

    public Task<string> EnterAsync(string fieldName, int row, int col, int size)
    {
        try
        {
            if (row > 0 && col > 0)
                Console.SetCursorPosition(col - 1, row - 1);
        }
        catch { }
        string? input = Console.ReadLine();
        return Task.FromResult(input ?? "");
    }

    public Task<bool> AskAsync(string prompt, bool defaultValue)
    {
        Console.Write($"{prompt} (Y/N) [{(defaultValue ? "Y" : "N")}]: ");
        string? input = Console.ReadLine();
        if (string.IsNullOrWhiteSpace(input)) return Task.FromResult(defaultValue);
        return Task.FromResult(input.Trim().ToUpper().StartsWith('Y'));
    }
}
