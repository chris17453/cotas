using CoTAS.Storage;

namespace CoTAS.Interpreter.Commands;

/// <summary>
/// Registry of command handlers. Commands register by name (case-insensitive).
/// GenericCommandStmt dispatches through this registry.
/// </summary>
public sealed class CommandRegistry
{
    private readonly Dictionary<string, ICommandHandler> _handlers = new(StringComparer.OrdinalIgnoreCase);

    public void Register(string commandName, ICommandHandler handler)
    {
        _handlers[commandName] = handler;
    }

    public bool TryGetHandler(string commandName, out ICommandHandler handler)
    {
        return _handlers.TryGetValue(commandName, out handler!);
    }

    /// <summary>
    /// Create a registry with all built-in commands registered.
    /// When storage is null, file I/O commands are stubs.
    /// </summary>
    public static CommandRegistry CreateDefault(StorageEngine? storage = null)
    {
        var registry = new CommandRegistry();
        var stub = new StubCommand();

        // Arithmetic
        registry.Register("INC", new IncCommand());
        registry.Register("DEC", new DecCommand());
        registry.Register("ADD", new AddCommand());

        // I/O
        registry.Register("ASK", new AskCommand());
        registry.Register("ENTER", new EnterCommand());
        registry.Register("PMSG", new PmsgCommand());

        // Trap handling
        registry.Register("TRAP", new TrapCommand());
        registry.Register("PUSHT", new PushTrapCommand());
        registry.Register("POPT", new PopTrapCommand());
        registry.Register("XTRAP", new XtrapCommand());

        // Screen commands
        registry.Register("CLRLNE", new ClearLineCommand());
        registry.Register("SAVES", new SaveScreenCommand());
        registry.Register("RSCR", new RestoreScreenCommand());
        registry.Register("REDSP", new RedisplayCommand());
        registry.Register("REDSP3", new RedisplayCommand());
        registry.Register("WINDOW", new WindowCommand());
        registry.Register("WNDW", new WindowCommand());
        registry.Register("PAINT", new PaintCommand());
        registry.Register("CURSOR", new CursorCommand());
        registry.Register("SCROLL", new ScrollCommand());
        registry.Register("REV", new ReverseCommand());
        registry.Register("ROW_COLOR", new RowColorCommand());
        registry.Register("HOTSPOT", new HotSpotCommand());
        registry.Register("BUTTON", new ButtonCommand());
        registry.Register("CAPTION", new CaptionCommand());
        registry.Register("GRAY", new GrayCommand());
        registry.Register("COLOR", new ColorCommand());
        registry.Register("CLRSF", new ClearScreenFieldsCommand());
        registry.Register("SCRN", new ScreenDefCommand());

        // Field/data commands
        registry.Register("FORMAT", new FormatCommand());
        registry.Register("REENT", new ReenterCommand());
        registry.Register("NOVLDMSG", new NoValidMsgCommand());
        registry.Register("FORCE", new ForceCommand());
        registry.Register("FORCE3", new ForceCommand());
        registry.Register("PICTURE", new PictureCommand());
        registry.Register("FILL", new FillCommand());
        registry.Register("TRIM", new TrimFieldCommand());
        registry.Register("UPCASE", new UpcaseFieldCommand());
        registry.Register("JUSTIFY", new JustifyFieldCommand());
        registry.Register("REDEFINE", new RedefineCommand());
        registry.Register("REPLACE", new ReplaceCommand());
        registry.Register("REMOVE", new RemoveCommand());
        registry.Register("PUSHF", new PushFieldCommand());
        registry.Register("POPF", new PopFieldCommand());
        registry.Register("POPS", new PopFieldCommand()); // alias
        registry.Register("INSRT", new InsrtCommand());
        registry.Register("DELC", new DelcCommand());
        registry.Register("MID", new MidCommand());
        registry.Register("KBDUP", new KbdupCommand());
        registry.Register("AUTODEC", new AutoDecCommand());
        registry.Register("AUTOENTER", new AutoEnterCommand());
        registry.Register("AUTOINC", new AutoIncCommand());

        // Navigation
        registry.Register("UPAR", new UpArrowCommand());
        registry.Register("DNAR", new DownArrowCommand());
        registry.Register("FEXIT", new FexitCommand());
        registry.Register("SEXIT", new SexitCommand());
        registry.Register("SLOOP", new SloopCommand());

        // Printing commands
        registry.Register("PON", new PrintOnCommand());
        registry.Register("POFF", new PrintOffCommand());
        registry.Register("TOF", new PrintTopOfFormCommand());
        registry.Register("HP2", new HalfPageCommand());
        registry.Register("RPTFMT", new ReportFormatCommand());
        registry.Register("PRTALL", new PrintAllCommand());
        registry.Register("PRTO", new PrintToCommand());
        registry.Register("CLSPF", new ClosePrintFileCommand());
        registry.Register("PRSET", new PrinterSetCommand());
        registry.Register("PRNUM", new PrinterNumCommand());
        registry.Register("PRTBOX", new PrintBoxCommand());
        registry.Register("P", new PrintBlankLinesCommand());

        // File I/O — real implementations when storage is available, stubs otherwise
        if (storage != null)
        {
            registry.Register("OPENV", new OpenvCommand(storage));
            registry.Register("FINDV", new FindvCommand(storage));
            registry.Register("SAVE", new SaveCommand(storage));
            registry.Register("CLR", new ClrCommand(storage));
            registry.Register("DEL", new DelCommand(storage));
            registry.Register("CLOSE", new CloseCommand(storage));
        }
        else
        {
            registry.Register("OPENV", stub);
            registry.Register("FINDV", stub);
            registry.Register("SAVE", stub);
            registry.Register("CLR", stub);
            registry.Register("DEL", stub);
            registry.Register("CLOSE", stub);
        }
        registry.Register("RLCK", new RecordLockCommand());
        registry.Register("ULKALL", new UnlockAllCommand());
        registry.Register("SRCH", new SrchCommand());
        registry.Register("READ", new ReadRecordCommand());
        registry.Register("WRITE", new WriteRecordCommand());
        registry.Register("SET_ACTIVE", new SetActiveCommand());
        registry.Register("REOPEN", new ReopenFileCommand());
        registry.Register("INIT", new InitFileCommand());
        registry.Register("DELF", new DeleteFileCommand());
        registry.Register("RENF", new RenameFileCommand());
        registry.Register("FILTER", new FilterCommand());
        registry.Register("RELATE", new RelateCommand());
        registry.Register("TRANSACTION", new TransactionCommand());
        registry.Register("MOUNT", new MountCommand());
        registry.Register("FILELOC", new FileLocCommand());

        // Array commands
        registry.Register("RDA", new ArrayReadCommand());
        registry.Register("WRA", new ArrayWriteCommand());
        registry.Register("UDA", new ArrayUpdateCommand());
        registry.Register("SORT", new ArraySortCommand());
        registry.Register("RMVA", new ArrayRemoveCommand());
        registry.Register("DSPA", new ArrayDisplayCommand());
        registry.Register("DLCA", new ArrayDeallocateCommand());

        // Misc commands
        registry.Register("BELL", new BellCommand());
        registry.Register("SOUND", new SoundCommand());
        registry.Register("TRACE", new TraceCommand());
        registry.Register("CLOCK", new ClockCommand());
        registry.Register("ON", new OnCommand());
        registry.Register("CHAIN", new ChainCommand());
        registry.Register("CHAINR", new ChainCommand()); // chain with return
        registry.Register("RUN", new RunCommand());
        registry.Register("RAP", new RunCommand()); // run anytime program
        registry.Register("NORSTRT", new NoRestartCommand());
        registry.Register("NOREDSP", new NoRedisplayCommand());
        registry.Register("CO", new CompanyCodeCommand());
        registry.Register("OWNER", new OwnerCommand());
        registry.Register("PARAMETER", new ParameterCommand());
        registry.Register("INT", new InterruptCommand());
        registry.Register("AUTO_RUN", new AutoRunCommand());
        registry.Register("MENU", new MenuCommand());
        registry.Register("NMENU", new NMenuCommand());
        registry.Register("LIST", new ListCommand());
        registry.Register("RDLIST", new ListCommand()); // redisplay list
        registry.Register("EXPORT", new ExportCommand());
        registry.Register("IMPORT", new ImportCommand());
        registry.Register("DATE", new DateCommand());
        registry.Register("TIME", new TimeCommand());
        registry.Register("GETLBL", new GetlblCommand());
        registry.Register("MOUSE", new MouseCommand());
        registry.Register("POINTER", new PointerCommand());
        registry.Register("ERROR", new ErrorCommand());
        registry.Register("CLRPE", new ClearProgramErrorCommand());
        registry.Register("COMPILE", new CompileCommand());
        registry.Register("SELECT", new FileSelectCommand());
        registry.Register("NOCLR", new NoclrCommand());
        registry.Register("NOFD", new NofdCommand());
        registry.Register("NOCMA", new NocmaCommand());
        registry.Register("NOZERO", new NozeroCommand());
        registry.Register("SET_LINE", new SetLineCommand());
        registry.Register("SORT3", new Sort3Command());

        // UDF/UDC definition markers (handled by interpreter, not commands)
        // FUNC and CMD are parsed as GenericCommandStmt but define subroutines
        // They will be intercepted by the interpreter during the label-collection pass

        return registry;
    }
}
