namespace CoTAS.Interpreter;

public interface IUIBridge
{
    Task ClearScreenAsync();
    Task SayAsync(string text, int row, int col);
    Task MessageAsync(string text);
    Task<string> EnterAsync(string fieldName, int row, int col, int size);
    Task<bool> AskAsync(string prompt, bool defaultValue);
}
