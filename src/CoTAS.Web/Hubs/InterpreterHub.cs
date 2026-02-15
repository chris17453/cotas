using Microsoft.AspNetCore.SignalR;
using CoTAS.Parser;
using CoTAS.Interpreter;
using CoTAS.Interpreter.Commands;
using CoTAS.Storage;
using System.Collections.Concurrent;

namespace CoTAS.Web.Hubs;

public sealed class InterpreterHub : Hub
{
    private readonly IHubContext<InterpreterHub> _hubContext;
    private readonly IConfiguration _config;
    private readonly ILogger<InterpreterHub> _logger;

    // Track active sessions: connectionId -> (bridge, cancellation)
    private static readonly ConcurrentDictionary<string, (WebBridge Bridge, CancellationTokenSource Cts)> _sessions = new();

    public InterpreterHub(IHubContext<InterpreterHub> hubContext, IConfiguration config, ILogger<InterpreterHub> logger)
    {
        _hubContext = hubContext;
        _config = config;
        _logger = logger;
    }

    private string UserCodeDir => _config["UserCodeDirectory"] ?? "user_code";

    private StorageConfig BuildStorageConfig()
    {
        var baseConn = _config["Storage:BaseConnectionString"]
            ?? _config.GetConnectionString("Default")
            ?? "";
        var ddfDir = _config["Storage:DdfDirectory"] ?? "db";
        var schema = _config["Storage:DefaultSchema"] ?? "dbo";

        // Build database map from config section
        var dbMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        var mapSection = _config.GetSection("Storage:DatabaseMap");
        foreach (var child in mapSection.GetChildren())
        {
            if (child.Value != null)
                dbMap[child.Key] = child.Value;
        }

        return new StorageConfig
        {
            BaseConnectionString = baseConn,
            DdfDirectory = ddfDir,
            DefaultSchema = schema,
            DatabaseMap = dbMap,
        };
    }

    /// <summary>
    /// Get list of available SRC files.
    /// </summary>
    public Task<string[]> GetPrograms()
    {
        var dir = UserCodeDir;
        _logger.LogInformation("GetPrograms: looking in {Dir}, exists={Exists}", dir, Directory.Exists(dir));

        if (!Directory.Exists(dir))
            return Task.FromResult(Array.Empty<string>());

        var files = Directory.GetFiles(dir, "*.SRC")
            .Select(Path.GetFileName)
            .Where(f => f != null)
            .Select(f => f!)
            .OrderBy(f => f)
            .ToArray();

        _logger.LogInformation("GetPrograms: found {Count} files", files.Length);
        return Task.FromResult(files);
    }

    /// <summary>
    /// Run a TAS program by filename.
    /// </summary>
    public async Task RunProgram(string fileName)
    {
        var connectionId = Context.ConnectionId;
        _logger.LogInformation("RunProgram: {File} for connection {ConnId}", fileName, connectionId);

        // Cancel any existing session for this connection
        StopExistingSession(connectionId);

        string filePath = Path.Combine(UserCodeDir, fileName);
        if (!File.Exists(filePath))
        {
            _logger.LogWarning("RunProgram: file not found at {Path}", filePath);
            await Clients.Caller.SendAsync("Error", $"File not found: {fileName} (looked at: {filePath})");
            return;
        }

        var bridge = new WebBridge(_hubContext, connectionId);
        var cts = new CancellationTokenSource();
        _sessions[connectionId] = (bridge, cts);

        await Clients.Caller.SendAsync("ProgramStarted", fileName);

        // Capture config values before Task.Run (hub instance may be disposed)
        var storageConfig = BuildStorageConfig();
        var capturedFilePath = filePath;
        var capturedHubContext = _hubContext;
        var capturedConfig = _config;

        _ = Task.Run(async () =>
        {
            try
            {
                string source = await File.ReadAllTextAsync(capturedFilePath, cts.Token);

                // Preprocess: resolve #lib and #inc includes
                // AutoLibs simulates the menu/parent program that normally loads shared libraries
                string sourceDir = Path.GetDirectoryName(Path.GetFullPath(capturedFilePath)) ?? ".";
                var preprocessor = new Preprocessor(sourceDir);
                source = preprocessor.Process(source, capturedFilePath);

                var lexer = new Lexer(source);
                var tokens = lexer.Tokenize();
                var parser = new TasParser(tokens);
                var program = parser.ParseProgram();

                StorageEngine? storage = null;
                if (!string.IsNullOrEmpty(storageConfig.BaseConnectionString))
                {
                    try
                    {
                        storage = new StorageEngine(storageConfig);
                    }
                    catch (Exception ex)
                    {
                        // Storage init failure is non-fatal — run without storage
                        await capturedHubContext.Clients.Client(connectionId)
                            .SendAsync("Message", $"Storage init skipped: {ex.Message}");
                    }
                }

                try
                {
                    var commands = CommandRegistry.CreateDefault(storage);
                    var interpreter = new TasInterpreter(bridge, commands);
                    await interpreter.ExecuteAsync(program);
                }
                finally
                {
                    storage?.Dispose();
                }

                await capturedHubContext.Clients.Client(connectionId)
                    .SendAsync("ProgramEnded", fileName);
            }
            catch (OperationCanceledException)
            {
                await capturedHubContext.Clients.Client(connectionId)
                    .SendAsync("ProgramStopped", fileName);
            }
            catch (Exception ex)
            {
                await capturedHubContext.Clients.Client(connectionId)
                    .SendAsync("Error", $"{ex.GetType().Name}: {ex.Message}");
            }
            finally
            {
                _sessions.TryRemove(connectionId, out _);
            }
        });
    }

    /// <summary>
    /// Stop the currently running program.
    /// </summary>
    public Task StopProgram()
    {
        StopExistingSession(Context.ConnectionId);
        return Task.CompletedTask;
    }

    /// <summary>
    /// Submit user input (for ENTER/ASK fields).
    /// </summary>
    public Task SubmitInput(string value)
    {
        if (_sessions.TryGetValue(Context.ConnectionId, out var session))
        {
            session.Bridge.SubmitInput(value);
        }
        return Task.CompletedTask;
    }

    /// <summary>
    /// Send a key press (for trap handling: F1-F10, ESC).
    /// </summary>
    public Task SendKeyPress(string key)
    {
        // TODO: wire to TrapManager when trap key handling is implemented
        return Task.CompletedTask;
    }

    public override Task OnDisconnectedAsync(Exception? exception)
    {
        StopExistingSession(Context.ConnectionId);
        return base.OnDisconnectedAsync(exception);
    }

    private static void StopExistingSession(string connectionId)
    {
        if (_sessions.TryRemove(connectionId, out var session))
        {
            session.Cts.Cancel();
            session.Bridge.Cancel();
        }
    }
}
