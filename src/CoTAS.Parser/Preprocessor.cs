using System.Text;

namespace CoTAS.Parser;

/// <summary>
/// TAS source preprocessor. Resolves #lib and #inc directives by textually
/// including referenced library (.LIB) and source (.SRC) files before lexing.
///
/// Directives:
///   #lib name     → include name.LIB (search: same dir, then search paths)
///   #inc name     → include name.SRC (search: same dir, then search paths)
///   #udx, #udc    → compiler mode flags (passed through as-is)
///   #proc, #endp  → procedure markers (passed through as-is)
///   #PSC, #add_flds → other directives (passed through as-is)
/// </summary>
public sealed class Preprocessor
{
    private readonly List<string> _searchPaths;
    private readonly HashSet<string> _includedFiles = new(StringComparer.OrdinalIgnoreCase);
    private const int MaxIncludeDepth = 20;

    /// <summary>
    /// Create a preprocessor with search paths for resolving includes.
    /// </summary>
    /// <param name="sourceDir">Directory of the main source file.</param>
    /// <param name="additionalPaths">Additional search paths for library files.</param>
    public Preprocessor(string sourceDir, IEnumerable<string>? additionalPaths = null)
    {
        _searchPaths = [sourceDir];
        if (additionalPaths != null)
            _searchPaths.AddRange(additionalPaths);
    }

    /// <summary>
    /// Process source text, expanding #lib and #inc directives.
    /// Returns the expanded source with all includes inlined.
    /// </summary>
    public string Process(string source, string sourceFilePath, int depth = 0)
    {
        if (depth > MaxIncludeDepth)
            return source; // prevent infinite recursion

        // Track this file to prevent circular includes
        string fullPath = Path.GetFullPath(sourceFilePath);
        if (!_includedFiles.Add(fullPath) && depth > 0)
            return ""; // already included

        var sb = new StringBuilder();
        using var reader = new StringReader(source);
        string? line;

        while ((line = reader.ReadLine()) != null)
        {
            string trimmed = line.TrimStart();

            if (trimmed.StartsWith("#lib ", StringComparison.OrdinalIgnoreCase) ||
                trimmed.StartsWith("#lib\t", StringComparison.OrdinalIgnoreCase))
            {
                string libRef = ExtractDirectiveArg(trimmed, 4);
                string? libContent = ResolveAndRead(libRef, ".LIB", sourceFilePath);
                if (libContent != null)
                {
                    string libPath = ResolveFilePath(libRef, ".LIB", sourceFilePath) ?? "";
                    sb.AppendLine($"; --- #lib {libRef} ---");
                    sb.AppendLine(Process(libContent, libPath, depth + 1));
                    sb.AppendLine($"; --- end #lib {libRef} ---");
                }
                else
                {
                    // Keep the directive as a comment so it doesn't break parsing
                    sb.AppendLine($"; #lib {libRef} (not found)");
                }
            }
            else if (trimmed.StartsWith("#inc ", StringComparison.OrdinalIgnoreCase) ||
                     trimmed.StartsWith("#inc\t", StringComparison.OrdinalIgnoreCase))
            {
                string incRef = ExtractDirectiveArg(trimmed, 4);
                string? incContent = ResolveAndRead(incRef, ".SRC", sourceFilePath);
                if (incContent != null)
                {
                    string incPath = ResolveFilePath(incRef, ".SRC", sourceFilePath) ?? "";
                    sb.AppendLine($"; --- #inc {incRef} ---");
                    sb.AppendLine(Process(incContent, incPath, depth + 1));
                    sb.AppendLine($"; --- end #inc {incRef} ---");
                }
                else
                {
                    sb.AppendLine($"; #inc {incRef} (not found)");
                }
            }
            else
            {
                sb.AppendLine(line);
            }
        }

        return sb.ToString();
    }

    /// <summary>
    /// Extract the argument from a preprocessor directive line.
    /// Handles comments (;, &&) after the argument.
    /// </summary>
    private static string ExtractDirectiveArg(string line, int directiveLen)
    {
        string rest = line[(directiveLen + 1)..].Trim().TrimEnd('\r');

        // Strip inline comments: && or ;
        int commentIdx = rest.IndexOf("&&");
        if (commentIdx >= 0) rest = rest[..commentIdx].Trim();
        commentIdx = rest.IndexOf(';');
        if (commentIdx >= 0) rest = rest[..commentIdx].Trim();

        // Strip quotes if present
        rest = rest.Trim('\'', '"');

        // Normalize path separators
        rest = rest.Replace('\\', Path.DirectorySeparatorChar);

        return rest;
    }

    /// <summary>
    /// Resolve a library/include reference to a file and read its content.
    /// </summary>
    private string? ResolveAndRead(string reference, string defaultExt, string currentFile)
    {
        string? path = ResolveFilePath(reference, defaultExt, currentFile);
        if (path != null && File.Exists(path))
            return File.ReadAllText(path);
        return null;
    }

    /// <summary>
    /// Resolve a library/include reference to a full file path.
    /// Search order: exact path, current dir, search paths.
    /// </summary>
    private string? ResolveFilePath(string reference, string defaultExt, string currentFile)
    {
        string currentDir = Path.GetDirectoryName(Path.GetFullPath(currentFile)) ?? ".";

        // Build candidate filenames
        var candidates = new List<string>();

        // As-is (may have extension already)
        candidates.Add(reference);

        // With default extension
        if (!Path.HasExtension(reference))
            candidates.Add(reference + defaultExt);

        // Also try without path prefix (just the filename)
        string baseName = Path.GetFileName(reference);
        if (baseName != reference)
        {
            candidates.Add(baseName);
            if (!Path.HasExtension(baseName))
                candidates.Add(baseName + defaultExt);
        }

        // Search in each directory
        foreach (var dir in new[] { currentDir }.Concat(_searchPaths))
        {
            foreach (var candidate in candidates)
            {
                string fullPath = Path.Combine(dir, candidate);
                if (File.Exists(fullPath))
                    return fullPath;

                // Case-insensitive search on Linux
                string? found = FindFileIgnoreCase(dir, candidate);
                if (found != null)
                    return found;
            }
        }

        return null;
    }

    /// <summary>
    /// Find a file in a directory with case-insensitive matching (for Linux).
    /// </summary>
    private static string? FindFileIgnoreCase(string directory, string fileName)
    {
        if (!Directory.Exists(directory))
            return null;

        try
        {
            string targetName = Path.GetFileName(fileName);
            foreach (var file in Directory.GetFiles(directory))
            {
                if (Path.GetFileName(file).Equals(targetName, StringComparison.OrdinalIgnoreCase))
                    return file;
            }
        }
        catch
        {
            // Ignore directory access errors
        }

        return null;
    }
}
