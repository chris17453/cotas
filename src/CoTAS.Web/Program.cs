using CoTAS.Web.Hubs;
using Microsoft.Extensions.FileProviders;

var builder = WebApplication.CreateBuilder(args);

// Find the solution root (where CoTAS.sln lives) so relative paths to
// user_code/ and db/ work regardless of which directory dotnet run is invoked from.
var solutionRoot = FindSolutionRoot(builder.Environment.ContentRootPath);
if (solutionRoot != null)
{
    // Override config paths to be relative to solution root
    builder.Configuration["UserCodeDirectory"] =
        Path.Combine(solutionRoot, builder.Configuration["UserCodeDirectory"] ?? "user_code");

    var ddfDir = builder.Configuration["Storage:DdfDirectory"];
    if (ddfDir != null && !Path.IsPathRooted(ddfDir))
        builder.Configuration["Storage:DdfDirectory"] = Path.Combine(solutionRoot, ddfDir);
}

builder.Services.AddSignalR();

var app = builder.Build();

// Serve static files from the project's wwwroot
app.UseDefaultFiles();
app.UseStaticFiles();

app.MapHub<InterpreterHub>("/hub/interpreter");

app.Run();

static string? FindSolutionRoot(string startDir)
{
    var dir = startDir;
    while (dir != null)
    {
        if (File.Exists(Path.Combine(dir, "CoTAS.sln")))
            return dir;
        dir = Path.GetDirectoryName(dir);
    }
    return null;
}
