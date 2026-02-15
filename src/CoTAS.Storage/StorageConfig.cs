namespace CoTAS.Storage;

/// <summary>
/// Configuration for the storage engine.
/// TAS code opens "files" which map via DDF .int files to MSSQL tables.
/// The DATABASE_SPACE_NAME in the DDF determines which database to use.
/// </summary>
public sealed class StorageConfig
{
    /// <summary>
    /// Base MSSQL connection string (without database name).
    /// Example: "Server=10.90.0.48;User Id=adv;Password=jookie;TrustServerCertificate=true"
    /// </summary>
    public required string BaseConnectionString { get; init; }

    /// <summary>
    /// Path to the root directory containing DDF subdirectories (e.g., db/).
    /// Each subdirectory (GPACIFIC, JADVDATA, etc.) contains .int files.
    /// </summary>
    public required string DdfDirectory { get; init; }

    /// <summary>
    /// Default SQL schema for all tables.
    /// </summary>
    public string DefaultSchema { get; init; } = "dbo";

    /// <summary>
    /// Maps DDF DATABASE_SPACE_NAME (and aliases) to actual MSSQL database names.
    /// Case-insensitive lookup. Example:
    ///   "GPacific" -> "gpacific"
    ///   "pacific"  -> "gpacific"
    ///   "advdata"  -> "jadvdata"
    /// </summary>
    public Dictionary<string, string> DatabaseMap { get; init; } = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Build a connection string for a specific database name.
    /// </summary>
    public string GetConnectionString(string databaseSpaceName)
    {
        string dbName = ResolveDatabaseName(databaseSpaceName);
        var builder = new Microsoft.Data.SqlClient.SqlConnectionStringBuilder(BaseConnectionString)
        {
            InitialCatalog = dbName
        };
        return builder.ConnectionString;
    }

    /// <summary>
    /// Resolve a DDF DATABASE_SPACE_NAME to an actual MSSQL database name.
    /// </summary>
    public string ResolveDatabaseName(string databaseSpaceName)
    {
        // Case-insensitive lookup (config binding may not preserve OrdinalIgnoreCase comparer)
        foreach (var kvp in DatabaseMap)
        {
            if (string.Equals(kvp.Key, databaseSpaceName, StringComparison.OrdinalIgnoreCase))
                return kvp.Value;
        }

        // Fallback: use the space name as-is (lowercase)
        return databaseSpaceName.ToLowerInvariant();
    }

    /// <summary>
    /// Get all DDF directories to scan (subdirectories of DdfDirectory, plus DdfDirectory itself).
    /// </summary>
    public IEnumerable<string> GetAllDdfDirectories()
    {
        if (string.IsNullOrEmpty(DdfDirectory) || !Directory.Exists(DdfDirectory))
            yield break;

        // Check if the directory itself has .int files
        if (Directory.GetFiles(DdfDirectory, "*.int").Length > 0)
            yield return DdfDirectory;

        // Scan subdirectories (GPACIFIC, JADVDATA, etc.)
        foreach (var subDir in Directory.GetDirectories(DdfDirectory))
        {
            if (Directory.GetFiles(subDir, "*.int").Length > 0)
                yield return subDir;
        }
    }
}
