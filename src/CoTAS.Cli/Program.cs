using Microsoft.Extensions.Configuration;
using CoTAS.Parser;
using CoTAS.Parser.Ast;
using CoTAS.Parser.RunFile;
using CoTAS.Interpreter;
using CoTAS.Interpreter.Commands;
using CoTAS.Bridge;
using CoTAS.Storage;
using CoTAS.Compiler;

// Load configuration from appsettings.json (look next to exe, then in working dir)
var config = new ConfigurationBuilder()
    .SetBasePath(AppContext.BaseDirectory)
    .AddJsonFile("appsettings.json", optional: true)
    .SetBasePath(Directory.GetCurrentDirectory())
    .AddJsonFile("appsettings.json", optional: true)
    .Build();

if (args.Length < 1)
{
    Console.Error.WriteLine("Usage: CoTAS.Cli <source.SRC> [--parse-only] [--batch-parse <dir>] [--read-run <file.RUN>]");
    Console.Error.WriteLine("       CoTAS.Cli --compile <source.SRC> [-o output.RUN]");
    Console.Error.WriteLine("       CoTAS.Cli --roundtrip <file.RUN>");
    Console.Error.WriteLine("       CoTAS.Cli --batch-roundtrip <dir>");
    return 1;
}

// Full round-trip: .RUN → decompile → .SRC → parse → compile → .RUN → compare bytes
if (args[0] == "--full-roundtrip" && args.Length >= 2)
{
    string runFile = args[1];
    if (!File.Exists(runFile))
    {
        Console.Error.WriteLine($"File not found: {runFile}");
        return 1;
    }
    try
    {
        byte[] original = File.ReadAllBytes(runFile);

        bool verbose = args.Contains("-v") || args.Contains("--verbose");

        // Use FullRoundTrip: decompile → parse → compile with metadata
        var (recompiled, report) = TasCompiler.FullRoundTrip(runFile);

        if (verbose)
        {
            Console.Error.Write(report);
            Console.Error.WriteLine($"Recompiled {recompiled.Length} bytes");
        }

        // Optionally save the generated file for debugging
        string? saveGenPath = args.SkipWhile(a => a != "--save-gen").Skip(1).FirstOrDefault();
        if (saveGenPath != null)
            File.WriteAllBytes(saveGenPath, recompiled);

        // Step 5: Compare
        if (original.Length != recompiled.Length)
        {
            Console.Error.WriteLine($"FAIL {Path.GetFileName(runFile)}: size mismatch ({original.Length} vs {recompiled.Length} bytes)");
            // Show segment comparison
            if (original.Length >= 128 && recompiled.Length >= 128)
            {
                int oCode = BitConverter.ToInt32(original, 0);
                int oConst = BitConverter.ToInt32(original, 4);
                int oSpec = BitConverter.ToInt32(original, 8);
                int oLabels = BitConverter.ToInt32(original, 12);
                int oFlds = BitConverter.ToUInt16(original, 16);
                int nCode = BitConverter.ToInt32(recompiled, 0);
                int nConst = BitConverter.ToInt32(recompiled, 4);
                int nSpec = BitConverter.ToInt32(recompiled, 8);
                int nLabels = BitConverter.ToInt32(recompiled, 12);
                int nFlds = BitConverter.ToUInt16(recompiled, 16);
                int oFldBytes = original.Length - (1728 + oCode + oConst + oSpec + oLabels);
                int nFldBytes = recompiled.Length - (1728 + nCode + nConst + nSpec + nLabels);
                Console.Error.WriteLine($"  code: {oCode} vs {nCode} ({nCode - oCode:+#;-#;0})");
                Console.Error.WriteLine($"  const: {oConst} vs {nConst} ({nConst - oConst:+#;-#;0})");
                Console.Error.WriteLine($"  spec: {oSpec} vs {nSpec} ({nSpec - oSpec:+#;-#;0})");
                Console.Error.WriteLine($"  labels: {oLabels} vs {nLabels} ({nLabels - oLabels:+#;-#;0})");
                Console.Error.WriteLine($"  flds: {oFlds} vs {nFlds} ({nFlds - oFlds:+#;-#;0}), bytes: {oFldBytes} vs {nFldBytes} ({nFldBytes - oFldBytes:+#;-#;0})");
            }
            return 1;
        }

        int firstDiff = -1;
        int diffCount = 0;
        for (int i = 0; i < original.Length; i++)
        {
            if (original[i] != recompiled[i])
            {
                if (firstDiff < 0) firstDiff = i;
                diffCount++;
            }
        }

        if (diffCount > 0)
        {
            Console.Error.WriteLine($"FAIL {Path.GetFileName(runFile)}: {diffCount} byte(s) differ, first at offset 0x{firstDiff:X4}");
            int start = Math.Max(0, firstDiff - 4);
            int end = Math.Min(original.Length, firstDiff + 16);
            Console.Error.Write("  orig: ");
            for (int i = start; i < end; i++)
                Console.Error.Write($"{original[i]:X2}{(i == firstDiff ? "*" : " ")}");
            Console.Error.WriteLine();
            Console.Error.Write("  new:  ");
            for (int i = start; i < end; i++)
                Console.Error.Write($"{recompiled[i]:X2}{(i == firstDiff ? "*" : " ")}");
            Console.Error.WriteLine();
            return 1;
        }

        Console.WriteLine($"OK {Path.GetFileName(runFile)} ({original.Length} bytes, byte-identical)");
        return 0;
    }
    catch (Exception ex)
    {
        Console.Error.WriteLine($"ERROR {Path.GetFileName(runFile)}: {ex.Message}");
        if (args.Contains("-v") || args.Contains("--verbose"))
            Console.Error.WriteLine(ex.StackTrace);
        return 1;
    }
}

// Round-trip a .RUN file: read → write → compare bytes
if (args[0] == "--roundtrip" && args.Length >= 2)
{
    string runFile = args[1];
    if (!File.Exists(runFile))
    {
        Console.Error.WriteLine($"File not found: {runFile}");
        return 1;
    }
    try
    {
        byte[] original = File.ReadAllBytes(runFile);
        byte[] rewritten = TasCompiler.RoundTrip(runFile);

        if (original.Length != rewritten.Length)
        {
            Console.Error.WriteLine($"FAIL {Path.GetFileName(runFile)}: size mismatch ({original.Length} vs {rewritten.Length})");
            return 1;
        }

        int firstDiff = -1;
        int diffCount = 0;
        for (int i = 0; i < original.Length; i++)
        {
            if (original[i] != rewritten[i])
            {
                if (firstDiff < 0) firstDiff = i;
                diffCount++;
            }
        }

        if (diffCount > 0)
        {
            Console.Error.WriteLine($"FAIL {Path.GetFileName(runFile)}: {diffCount} byte(s) differ, first at offset 0x{firstDiff:X4}");
            // Show context around first diff
            int start = Math.Max(0, firstDiff - 4);
            int end = Math.Min(original.Length, firstDiff + 16);
            Console.Error.Write("  orig: ");
            for (int i = start; i < end; i++)
                Console.Error.Write($"{original[i]:X2}{(i == firstDiff ? "*" : " ")}");
            Console.Error.WriteLine();
            Console.Error.Write("  new:  ");
            for (int i = start; i < end; i++)
                Console.Error.Write($"{rewritten[i]:X2}{(i == firstDiff ? "*" : " ")}");
            Console.Error.WriteLine();
            return 1;
        }

        Console.WriteLine($"OK {Path.GetFileName(runFile)} ({original.Length} bytes, identical)");
        return 0;
    }
    catch (Exception ex)
    {
        Console.Error.WriteLine($"ERROR {Path.GetFileName(runFile)}: {ex.Message}");
        return 1;
    }
}

// Batch round-trip all .RUN files in a directory
if (args[0] == "--batch-roundtrip" && args.Length >= 2)
{
    string dir = args[1];
    var files = Directory.GetFiles(dir, "*.RUN");
    int pass = 0, fail = 0, error = 0;

    foreach (var file in files.OrderBy(f => f))
    {
        try
        {
            byte[] original = File.ReadAllBytes(file);
            byte[] rewritten = TasCompiler.RoundTrip(file);

            bool identical = original.Length == rewritten.Length;
            if (identical)
            {
                for (int i = 0; i < original.Length; i++)
                {
                    if (original[i] != rewritten[i]) { identical = false; break; }
                }
            }

            if (identical)
            {
                pass++;
            }
            else
            {
                fail++;
                int firstDiff = -1;
                int minLen = Math.Min(original.Length, rewritten.Length);
                for (int i = 0; i < minLen; i++)
                {
                    if (original[i] != rewritten[i]) { firstDiff = i; break; }
                }
                if (firstDiff < 0 && original.Length != rewritten.Length)
                    firstDiff = minLen;
                Console.Error.WriteLine($"FAIL {Path.GetFileName(file)}: diff at 0x{firstDiff:X4} (size {original.Length} vs {rewritten.Length})");
            }
        }
        catch (Exception ex)
        {
            error++;
            Console.Error.WriteLine($"ERROR {Path.GetFileName(file)}: {ex.Message}");
        }
    }

    Console.WriteLine($"Round-trip: {pass}/{files.Length} pass, {fail} fail, {error} error");
    return (fail + error > 0) ? 1 : 0;
}

// Batch full round-trip: decompile → parse → compile → compare bytes
if (args[0] == "--batch-full-roundtrip" && args.Length >= 2)
{
    string dir = args[1];
    var files = Directory.GetFiles(dir, "*.RUN");
    int pass = 0, fail = 0, error = 0;

    foreach (var file in files.OrderBy(f => f))
    {
        try
        {
            byte[] original = File.ReadAllBytes(file);
            var (recompiled, _) = TasCompiler.FullRoundTrip(file);

            bool identical = original.Length == recompiled.Length;
            if (identical)
            {
                for (int i = 0; i < original.Length; i++)
                {
                    if (original[i] != recompiled[i]) { identical = false; break; }
                }
            }

            if (identical)
            {
                pass++;
            }
            else
            {
                fail++;
                Console.Error.WriteLine($"FAIL {Path.GetFileName(file)}: {original.Length} vs {recompiled.Length} ({original.Length - recompiled.Length:+#;-#;0})");
            }
        }
        catch (Exception ex)
        {
            error++;
            Console.Error.WriteLine($"ERROR {Path.GetFileName(file)}: {ex.Message}");
        }
    }

    Console.WriteLine($"Full round-trip: {pass}/{files.Length} pass, {fail} fail, {error} error");
    return (fail + error > 0) ? 1 : 0;
}

// Compile .SRC to .RUN
if (args[0] == "--compile" && args.Length >= 2)
{
    string srcFile = args[1];
    if (!File.Exists(srcFile))
    {
        Console.Error.WriteLine($"File not found: {srcFile}");
        return 1;
    }

    string outFile = Path.ChangeExtension(srcFile, ".RUN");
    int outIdx = Array.IndexOf(args, "-o");
    if (outIdx >= 0 && outIdx + 1 < args.Length)
        outFile = args[outIdx + 1];

    try
    {
        string compSrc = File.ReadAllText(srcFile);
        string srcDir = Path.GetDirectoryName(Path.GetFullPath(srcFile)) ?? ".";
        var pp = new Preprocessor(srcDir);
        compSrc = pp.Process(compSrc, srcFile);

        var lexer = new Lexer(compSrc);
        var tokens = lexer.Tokenize();
        var parser = new TasParser(tokens);
        var program = parser.ParseProgram();

        var compiler = new TasCompiler();
        byte[] runBytes = compiler.Compile(program);

        File.WriteAllBytes(outFile, runBytes);
        Console.WriteLine($"Compiled {Path.GetFileName(srcFile)} -> {Path.GetFileName(outFile)} ({runBytes.Length} bytes)");
        return 0;
    }
    catch (Exception ex)
    {
        Console.Error.WriteLine($"Compile error: {ex}");
        return 1;
    }
}

// Decompile .RUN file back to pseudo-source
if (args[0] == "--decompile" && args.Length >= 2)
{
    string runFile = args[1];
    if (!File.Exists(runFile))
    {
        Console.Error.WriteLine($"File not found: {runFile}");
        return 1;
    }
    try
    {
        var run = RunFileReader.LoadAutoOverlay(runFile);
        var decompiler = new RunFileDecompiler(run);
        Console.Write(decompiler.Decompile());
        return 0;
    }
    catch (Exception ex)
    {
        Console.Error.WriteLine($"Error decompiling: {ex.Message}");
        return 1;
    }
}

// Read .RUN file mode: dump compiled program contents
if (args[0] == "--read-run" && args.Length >= 2)
{
    string runFile = args[1];
    if (!File.Exists(runFile))
    {
        Console.Error.WriteLine($"File not found: {runFile}");
        return 1;
    }

    try
    {
        var run = RunFileReader.Load(runFile);
        var h = run.Header;

        Console.WriteLine($"=== {Path.GetFileName(runFile)} ===");
        Console.WriteLine($"Signature: {h.ProType}");
        Console.WriteLine($"Code: {h.CodeSize} bytes ({run.Instructions.Count} instructions)");
        Console.WriteLine($"Constants: {h.ConstSize} bytes");
        Console.WriteLine($"Specs: {h.SpecSize} bytes");
        Console.WriteLine($"Labels: {h.NumLabels}");
        Console.WriteLine($"Fields: {h.NumFlds} total ({h.NumTempFlds} temp, {h.NumFlds - h.NumTempFlds} defined/file)");
        Console.WriteLine($"Extra field slots: {h.NumExtraFlds}");
        Console.WriteLine($"Defined data segment: {h.DefFldSegSize} bytes");
        Console.WriteLine($"Field spec format: {(h.NewFldSpec ? "new (60 bytes)" : "old (48 bytes)")}");
        Console.WriteLine();

        // File buffers
        if (run.Buffers.Count > 0)
        {
            Console.WriteLine($"--- File Buffers ({run.Buffers.Count}) ---");
            for (int i = 0; i < run.Buffers.Count; i++)
                Console.WriteLine($"  [{i}] {run.Buffers[i]}");
            Console.WriteLine();
        }

        // Show fields by category
        bool verbose = args.Contains("--verbose") || args.Contains("-v");

        var definedFields = run.GetDefinedFields().ToList();
        var fileFields = run.GetFileFields().ToList();
        var resetFields = run.GetResetFields().ToList();

        if (definedFields.Count > 0)
        {
            Console.WriteLine($"--- Defined Fields ({definedFields.Count}) ---");
            foreach (var f in definedFields)
                Console.WriteLine($"  {f}");
            Console.WriteLine();
        }

        if (fileFields.Count > 0)
        {
            Console.WriteLine($"--- File Fields ({fileFields.Count}) ---");
            foreach (var f in fileFields)
                Console.WriteLine($"  {f}");
            Console.WriteLine();
        }

        if (resetFields.Count > 0)
        {
            Console.WriteLine($"--- Reset Fields (inherited from parent) ({resetFields.Count}) ---");
            foreach (var f in resetFields)
                Console.WriteLine($"  {f}");
            Console.WriteLine();
        }

        if (verbose)
        {
            Console.WriteLine($"--- All Fields ({run.Fields.Count}) ---");
            for (int i = 0; i < run.Fields.Count; i++)
                Console.WriteLine($"  [{i}] {run.Fields[i]}");
            Console.WriteLine();

            // Build label-to-instruction mapping
            bool isTas51 = h.ProType == "TAS32";
            int instrSize = isTas51 ? RunFileHeader.Tas51InstructionSize : RunFileHeader.Tas60InstructionSize;
            var labelsByInstr = new Dictionary<int, List<int>>();
            for (int li = 0; li < run.LabelOffsets.Count; li++)
            {
                int instrIdx = run.LabelOffsets[li] / instrSize;
                if (!labelsByInstr.ContainsKey(instrIdx))
                    labelsByInstr[instrIdx] = [];
                labelsByInstr[instrIdx].Add(li);
            }

            Console.WriteLine($"--- Label Offsets ({run.LabelOffsets.Count}) ---");
            for (int i = 0; i < run.LabelOffsets.Count; i++)
            {
                int instrIdx = run.LabelOffsets[i] / instrSize;
                Console.WriteLine($"  Label[{i}] -> instruction {instrIdx} (byte offset {run.LabelOffsets[i]})");
            }
            Console.WriteLine();

            // Full disassembly
            Console.WriteLine($"--- Disassembly ({run.Instructions.Count} instructions) ---");
            for (int i = 0; i < run.Instructions.Count; i++)
            {
                // Show label markers
                if (labelsByInstr.TryGetValue(i, out var labels))
                {
                    foreach (var li in labels)
                        Console.WriteLine($"LABEL_{li}:");
                }
                Console.WriteLine($"  {i,5}: {run.Instructions[i]}");
            }
            Console.WriteLine();

            // Command frequency stats
            var cmdStats = run.Instructions
                .GroupBy(x => x.CommandName)
                .OrderByDescending(g => g.Count())
                .Take(20);
            Console.WriteLine("--- Top 20 Commands ---");
            foreach (var g in cmdStats)
                Console.WriteLine($"  {g.Key,-14} {g.Count(),5}x");
        }

        return 0;
    }
    catch (Exception ex)
    {
        Console.Error.WriteLine($"Error reading .RUN file: {ex.Message}");
        return 1;
    }
}

// Batch parse mode: parse all .SRC files in a directory and report stats
if (args[0] == "--batch-parse" && args.Length >= 2)
{
    string dir = args[1];
    var files = Directory.GetFiles(dir, "*.SRC");
    int success = 0, fail = 0;
    var errors = new List<string>();

    foreach (var file in files)
    {
        try
        {
            Console.Error.Write($"\rParsing {Path.GetFileName(file)}...                    ");
            string src = File.ReadAllText(file);
            string fileDir = Path.GetDirectoryName(Path.GetFullPath(file)) ?? ".";
            var pp = new Preprocessor(fileDir);
            src = pp.Process(src, file);
            var cts = new CancellationTokenSource(TimeSpan.FromSeconds(5));
            var task = Task.Run(() =>
            {
                var lexer = new Lexer(src);
                var tokens = lexer.Tokenize();
                var parser = new TasParser(tokens);
                return parser.ParseProgram();
            }, cts.Token);
            if (task.Wait(TimeSpan.FromSeconds(5)))
            {
                success++;
            }
            else
            {
                fail++;
                errors.Add($"{Path.GetFileName(file)}: TIMEOUT (possible infinite loop)");
            }
        }
        catch (Exception ex)
        {
            fail++;
            errors.Add($"{Path.GetFileName(file)}: {ex.GetBaseException().Message}");
        }
    }
    Console.Error.WriteLine();

    Console.WriteLine($"Parsed: {success}/{files.Length} ({100.0 * success / files.Length:F1}%)");
    Console.WriteLine($"Failed: {fail}");
    if (errors.Count > 0)
    {
        Console.WriteLine("\nErrors:");
        foreach (var e in errors)
            Console.WriteLine($"  {e}");
    }
    return fail > 0 ? 1 : 0;
}

string sourceFile = args[0];
bool parseOnly = args.Contains("--parse-only");

if (!File.Exists(sourceFile))
{
    Console.Error.WriteLine($"File not found: {sourceFile}");
    return 1;
}

string source = File.ReadAllText(sourceFile);

// Preprocess: resolve #lib and #inc includes
string sourceDir = Path.GetDirectoryName(Path.GetFullPath(sourceFile)) ?? ".";
var preprocessor = new Preprocessor(sourceDir);
source = preprocessor.Process(source, sourceFile);

try
{
        var lexer = new Lexer(source);
    var tokens = lexer.Tokenize();

    var parser = new TasParser(tokens);
    var program = parser.ParseProgram();

    Console.Error.WriteLine($"Parsed {program.Statements.Count} statements from {sourceFile}");

    if (parseOnly)
    {
        foreach (var stmt in program.Statements)
            Console.Error.WriteLine($"  [{stmt.Line}] {stmt.GetType().Name}");
        return 0;
    }

    // Build storage config from appsettings.json
    string? baseConn = config["Storage:BaseConnectionString"]
        ?? Environment.GetEnvironmentVariable("COTAS_CONNECTION_STRING");
    string? ddfDir = config["Storage:DdfDirectory"]
        ?? Environment.GetEnvironmentVariable("COTAS_DDF_DIR");

    StorageEngine? storage = null;
    if (!string.IsNullOrEmpty(baseConn))
    {
        try
        {
            var dbMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            var mapSection = config.GetSection("Storage:DatabaseMap");
            foreach (var child in mapSection.GetChildren())
            {
                if (child.Value != null)
                    dbMap[child.Key] = child.Value;
            }

            var storageConfig = new StorageConfig
            {
                BaseConnectionString = baseConn,
                DdfDirectory = ddfDir ?? "db",
                DefaultSchema = config["Storage:DefaultSchema"] ?? "dbo",
                DatabaseMap = dbMap,
            };
            storage = new StorageEngine(storageConfig);
            Console.Error.WriteLine($"Storage engine connected (DDF: {storageConfig.DdfDirectory})");
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Storage init failed (continuing without DB): {ex.Message}");
        }
    }

    try
    {
        var commands = CommandRegistry.CreateDefault(storage);
        var bridge = new ConsoleBridge();
        var interpreter = new TasInterpreter(bridge, commands, storage);
        await interpreter.ExecuteAsync(program);
    }
    finally
    {
        storage?.Dispose();
    }

    return 0;
}
catch (LexerException ex)
{
    Console.Error.WriteLine($"Lexer error: {ex.Message}");
    return 2;
}
catch (ParseException ex)
{
    Console.Error.WriteLine($"Parse error: {ex.Message}");
    return 2;
}
catch (InterpreterException ex)
{
    Console.Error.WriteLine($"Runtime error: {ex.Message}");
    return 3;
}
catch (StorageException ex)
{
    Console.Error.WriteLine($"Storage error: {ex.Message}");
    return 4;
}
