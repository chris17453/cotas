using CoTAS.Interpreter;
using Microsoft.AspNetCore.SignalR;
using CoTAS.Web.Hubs;

namespace CoTAS.Web;

/// <summary>
/// IUIBridge implementation that renders to a virtual 25x80 screen buffer
/// and pushes updates via SignalR to the browser.
/// </summary>
public sealed class WebBridge : IUIBridge
{
    private readonly IHubContext<InterpreterHub> _hubContext;
    private readonly string _connectionId;
    private readonly char[,] _screen = new char[25, 80];
    private TaskCompletionSource<string>? _pendingInput;
    private readonly object _inputLock = new();

    public WebBridge(IHubContext<InterpreterHub> hubContext, string connectionId)
    {
        _hubContext = hubContext;
        _connectionId = connectionId;
        ClearScreenBuffer();
    }

    public async Task ClearScreenAsync()
    {
        ClearScreenBuffer();
        await _hubContext.Clients.Client(_connectionId)
            .SendAsync("ClearScreen");
    }

    public async Task SayAsync(string text, int row, int col)
    {
        // Write to screen buffer
        int r = Math.Max(0, row - 1);
        int c = Math.Max(0, col - 1);
        for (int i = 0; i < text.Length && c + i < 80; i++)
        {
            if (r < 25)
                _screen[r, c + i] = text[i];
        }

        await _hubContext.Clients.Client(_connectionId)
            .SendAsync("Say", text, row, col);
    }

    public async Task MessageAsync(string text)
    {
        await _hubContext.Clients.Client(_connectionId)
            .SendAsync("Message", text);
    }

    public async Task<string> EnterAsync(string fieldName, int row, int col, int size)
    {
        var tcs = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);
        lock (_inputLock)
        {
            _pendingInput = tcs;
        }

        await _hubContext.Clients.Client(_connectionId)
            .SendAsync("RequestInput", fieldName, row, col, size);

        // Wait for user to submit input via SignalR
        var result = await tcs.Task;

        lock (_inputLock)
        {
            _pendingInput = null;
        }

        // Write entered value to screen buffer
        int r = Math.Max(0, row - 1);
        int c = Math.Max(0, col - 1);
        for (int i = 0; i < result.Length && c + i < 80; i++)
        {
            if (r < 25)
                _screen[r, c + i] = result[i];
        }

        return result;
    }

    public async Task<bool> AskAsync(string prompt, bool defaultValue)
    {
        var tcs = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);
        lock (_inputLock)
        {
            _pendingInput = tcs;
        }

        await _hubContext.Clients.Client(_connectionId)
            .SendAsync("RequestAsk", prompt, defaultValue);

        var result = await tcs.Task;

        lock (_inputLock)
        {
            _pendingInput = null;
        }

        return result.Trim().ToUpper().StartsWith('Y');
    }

    /// <summary>
    /// Called by the SignalR hub when the user submits input.
    /// </summary>
    public void SubmitInput(string value)
    {
        lock (_inputLock)
        {
            _pendingInput?.TrySetResult(value);
        }
    }

    /// <summary>
    /// Cancel any pending input (e.g., when the user disconnects).
    /// </summary>
    public void Cancel()
    {
        lock (_inputLock)
        {
            _pendingInput?.TrySetCanceled();
        }
    }

    /// <summary>
    /// Get the full screen buffer as a string array (25 rows of 80 chars).
    /// </summary>
    public string[] GetScreenSnapshot()
    {
        var rows = new string[25];
        for (int r = 0; r < 25; r++)
        {
            var chars = new char[80];
            for (int c = 0; c < 80; c++)
                chars[c] = _screen[r, c];
            rows[r] = new string(chars);
        }
        return rows;
    }

    private void ClearScreenBuffer()
    {
        for (int r = 0; r < 25; r++)
            for (int c = 0; c < 80; c++)
                _screen[r, c] = ' ';
    }
}
