namespace CoTAS.Parser.RunFile;

/// <summary>
/// TAS bytecode command numbers (CmdNum in TPrgPtr).
/// Mapped from the CmdList array in the TAS 6.0 compiler source (cmpspec3.pas).
/// </summary>
public static class TasOpcode
{
    // Standard commands (0-232)
    public const ushort NOP = 0;            // Remark/comment/no-op
    public const ushort PMSG = 1;           // Print message (also ?)
    public const ushort CLRSCR = 2;         // Clear screen
    public const ushort CURSOR = 3;         // Position cursor
    public const ushort BELL = 4;           // Sound bell
    public const ushort CLR = 6;            // Clear file buffer
    public const ushort CLRLNE = 7;         // Clear line
    public const ushort CLOSE = 8;          // Close file
    public const ushort RDA = 9;            // Read array
    public const ushort CLRSF = 10;         // Clear screen fields
    public const ushort POSTMSG = 11;       // Post message
    public const ushort DEL = 12;           // Delete record
    public const ushort WRTA = 13;          // Write array
    public const ushort MENU = 14;          // Menu command
    public const ushort ASSIGN = 15;        // Assignment (=)
    public const ushort DISPF = 16;         // Display field
    public const ushort FILL = 17;          // Fill field
    public const ushort POINTER = 18;       // Pointer assignment (->)
    public const ushort FIND = 19;          // Find record
    public const ushort SRCH = 20;          // Search file
    public const ushort GOSUB = 21;         // Gosub to label
    public const ushort GOTO = 22;          // Goto label
    public const ushort SAY = 23;           // Say at position
    public const ushort INIFLE = 24;        // Initialize file
    public const ushort UPDTA = 25;         // Update array
    public const ushort FINDV = 26;         // Find variant
    public const ushort NMENU = 27;         // New menu
    public const ushort MOUNT = 28;         // Mount format
    public const ushort XFER = 29;          // Transfer data
    public const ushort ON = 30;            // On goto/gosub
    public const ushort OPEN = 31;          // Open file
    public const ushort RET = 32;           // Return
    public const ushort ENTER = 33;         // Enter data
    public const ushort PBLNK = 34;         // Print blank lines
    public const ushort PBOX = 35;          // Print box
    public const ushort PCHR = 36;          // Print character
    public const ushort PFMT = 37;          // Print format
    public const ushort PON = 38;           // Print on
    public const ushort BRKRET = 39;        // Break return
    public const ushort PVERT = 40;         // Print vertical tab
    public const ushort INC = 41;           // Increment
    public const ushort MID_CMD = 42;       // Mid-string operation
    public const ushort CO = 43;            // Company code
    public const ushort REDEF = 44;         // Redefine
    public const ushort REDSP = 45;         // Redisplay screen
    public const ushort WINDEF = 46;        // Window define
    public const ushort WINACT = 47;        // Window activate
    public const ushort CHAIN = 48;         // Chain to program
    public const ushort SAVE = 49;          // Save record
    public const ushort SAVES = 50;         // Save screen
    public const ushort SCROLL = 51;        // Scroll window
    public const ushort SORTA = 52;         // Sort array
    public const ushort PTOF = 53;          // Print top of form
    public const ushort TRANSX = 54;        // Transaction
    public const ushort TRAP = 55;          // Set trap
    public const ushort ULKALL = 56;        // Unlock all
    public const ushort WINDOW = 57;        // Window command
    public const ushort REENT = 58;         // Re-enter
    public const ushort IF = 59;            // If statement
    public const ushort POPS = 60;          // Pop stack
    public const ushort CLSO = 61;          // Close stream output
    public const ushort PUT_FLD = 62;       // Put field in buffer
    public const ushort SORT3 = 63;         // Sort (Pro 3.0)
    public const ushort OPENV = 64;         // Open variant
    public const ushort REMVA = 65;         // Remove array
    public const ushort ELSE = 66;          // Else / absolute jump
    public const ushort WHILE = 67;         // While loop start
    public const ushort SELECT = 68;        // Select/case start
    public const ushort ENDW = 69;          // End while / absolute jump
    public const ushort LOOP_IF = 70;       // Loop if condition
    public const ushort EXIT_IF = 71;       // Exit if condition
    public const ushort FOR = 72;           // For loop
    public const ushort FUNC = 73;          // User function call
    public const ushort CMD = 74;           // User command call
    public const ushort UDC = 75;           // User-defined command
    public const ushort RAP = 76;           // Run anytime program
    public const ushort CHAINR = 77;        // Chain RAP
    public const ushort CLSPF = 78;         // Close print to file
    public const ushort LISTM = 79;         // List memory
    public const ushort EXPORT = 80;        // Export data
    public const ushort RSCR = 81;          // Reset screen
    public const ushort ADD = 82;           // Add field
    public const ushort LISTF = 83;         // List file
    public const ushort LIST = 84;          // List/display fields
    public const ushort REPL = 85;          // Replace
    public const ushort EXEC = 86;          // Execute program
    public const ushort QUIT = 87;          // Quit program
    public const ushort ERR = 88;           // Error
    public const ushort OWNER = 89;         // Set owner
    public const ushort DALL = 90;          // Delete all records
    public const ushort ASK = 91;           // Ask question
    public const ushort RCN_CMD = 92;       // Record number
    public const ushort REL = 93;           // Relate
    public const ushort FORCE = 94;         // Force
    public const ushort DEC = 95;           // Decrement
    public const ushort PAINT = 96;         // Paint screen
    public const ushort DEALOC = 97;        // Deallocate field
    public const ushort READ = 98;          // Read
    public const ushort WRITE = 99;         // Write
    public const ushort INSERT = 100;       // Insert
    public const ushort SETLINE = 101;      // Set line
    public const ushort REMOUNT = 102;      // Remount
    public const ushort SCRN = 103;         // Screen control
    public const ushort RDLIST = 104;       // Redisplay list
    public const ushort MSG = 106;          // Message
    public const ushort CLRPE = 107;        // Clear program error
    public const ushort UPAR = 108;         // Up arrow
    public const ushort ALLOC = 109;        // Allocate field
    public const ushort PSET = 110;         // Printer set
    public const ushort COLOR = 111;        // Set color
    public const ushort IMPORT = 112;       // Import data
    public const ushort PARAM = 113;        // Parameters
    public const ushort XTRAP = 115;        // Trap operations
    public const ushort SETACT = 116;       // Set active
    public const ushort BKG = 117;          // Background
    public const ushort FRG = 118;          // Foreground
    public const ushort REVERSE = 119;      // Reverse video
    public const ushort FORMAT = 120;       // Format
    public const ushort DISPM = 121;        // Display memory
    public const ushort FILLMEM = 122;      // Fill memory
    public const ushort DELC = 123;         // Delete characters
    public const ushort GOTOL = 124;        // Goto line
    public const ushort DELF = 125;         // Delete file
    public const ushort RENF = 126;         // Rename file
    public const ushort DATE = 127;         // Set date
    public const ushort TIME = 128;         // Set time
    public const ushort PUSHF = 129;        // Push field
    public const ushort POPF = 130;         // Pop field
    public const ushort ROPEN = 131;        // Reopen file
    public const ushort FILTER = 132;       // Filter
    public const ushort WRAP = 133;         // Wrap text
    public const ushort REWRAP = 134;       // Rewrap
    public const ushort UP = 135;           // Uppercase field
    public const ushort CLOCK = 136;        // Set clock
    public const ushort SCAN = 137;         // Scan records
    public const ushort SET_SCAN_FLG = 138; // Internal scan flag
    public const ushort TRIM = 139;         // Trim field
    public const ushort IFDUP = 140;        // If duplicate
    public const ushort IFNA = 141;         // If not active
    public const ushort PRTALL = 142;       // Print all
    public const ushort NOREDSP = 143;      // No redisplay
    public const ushort AUTOINC = 144;      // Auto increment
    public const ushort PUSHT = 145;        // Push trap
    public const ushort POPT = 146;         // Pop trap
    public const ushort CASE = 147;         // Case statement
    public const ushort PRT_NUM = 148;      // Print number
    public const ushort LD_PDRV = 149;      // Load print driver
    public const ushort GOSUBL = 150;       // Gosub line
    public const ushort GETLBL = 151;       // Get label
    public const ushort CDPATH = 152;       // Change dict path
    public const ushort SOUND = 153;        // Sound
    public const ushort TRACE = 154;        // Trace
    public const ushort INT_CMD = 155;      // Interrupt
    public const ushort LIST_EXIT = 156;    // List exit
    public const ushort POKE = 157;         // Poke memory
    public const ushort PEEK = 158;         // Peek memory
    public const ushort KBDUP = 159;        // Keyboard upper
    public const ushort NORSTRT = 160;      // No restart
    public const ushort NOVLDMSG = 161;     // No valid message
    public const ushort AUTOENTER = 162;    // Auto enter
    public const ushort AUTODEC = 163;      // Auto decrement
    public const ushort SHOW_PLINE = 164;   // Show print line
    public const ushort AUTO_RUN = 165;     // Auto run
    public const ushort RUN = 166;          // Run program
    public const ushort MEM_PTR = 167;      // Memory pointer
    public const ushort MEM_SPC = 168;      // Memory space
    public const ushort SAVES3 = 169;       // Save screen (Pro 3.0)
    public const ushort REDSP3 = 170;       // Redisplay (Pro 3.0)
    public const ushort WRAP3 = 171;        // Wrap print (Pro 3.0)
    public const ushort SSPCF = 172;        // Special file number
    public const ushort RDREC = 173;        // Read record
    public const ushort WTREC = 174;        // Write record
    public const ushort OPNO = 175;         // Open non-TAS file
    public const ushort EQU_MID = 176;      // Equals mid
    public const ushort EQU_DAY = 177;      // Equals day
    public const ushort EQU_XMT = 178;      // Equals month
    public const ushort FORCE3 = 179;       // Force (Pro 3.0)
    public const ushort MOUSE = 180;        // Mouse command
    public const ushort PLAYWAV = 181;      // Play WAV file
    public const ushort COMM = 182;         // Communications
    public const ushort JUST = 183;         // Justify
    public const ushort PORT = 184;         // Port operations
    public const ushort START_SCAN = 185;   // Internal scan start
    public const ushort AUTONEW = 186;      // Auto new
    public const ushort WCOLOR = 187;       // Window color set
    public const ushort BUTTON = 188;       // Button
    public const ushort CAPTION = 189;      // Set caption
    public const ushort LOAD_PICTURE = 190; // Load picture
    public const ushort GRAY = 191;         // Gray on/off
    public const ushort BRACE_OPEN = 192;   // Begin brace
    public const ushort BRACE_CLOSE = 193;  // End brace
    public const ushort ROW_COLOR = 194;    // Row color
    public const ushort HOT_SPOT = 196;     // Hot spot
    public const ushort NEXT = 220;         // Next (for loop end)

    // Compiler/control-flow commands (500+)
    public const ushort DEFINE = 501;       // Field definition
    public const ushort ENDIF = 502;        // End if
    public const ushort ELSE_IF = 503;      // Else if
    public const ushort LOOP = 504;         // Loop (while continue)
    public const ushort OTHERWISE = 506;    // Otherwise (default case)
    public const ushort ENDC = 507;         // End case
    public const ushort EXIT_CMD = 508;     // Exit (while break)
    public const ushort FLOOP = 510;        // For loop (continue)
    public const ushort FEXIT = 511;        // For exit (break)
    public const ushort FLOOP_IF = 512;     // For loop if
    public const ushort FEXIT_IF = 513;     // For exit if
    public const ushort SEXIT = 514;        // Scan exit
    public const ushort SEXIT_IF = 515;     // Scan exit if
    public const ushort SLOOP = 516;        // Scan loop
    public const ushort SLOOP_IF = 517;     // Scan loop if
    public const ushort ENDS = 518;         // End scan

    private static readonly Dictionary<ushort, string> _names = new()
    {
        [NOP] = "NOP", [PMSG] = "PMSG", [CLRSCR] = "CLRSCR", [CURSOR] = "CURSOR",
        [BELL] = "BELL", [CLR] = "CLR", [CLRLNE] = "CLRLNE", [CLOSE] = "CLOSE",
        [RDA] = "RDA", [CLRSF] = "CLRSF", [POSTMSG] = "POSTMSG", [DEL] = "DEL",
        [WRTA] = "WRTA", [MENU] = "MENU", [ASSIGN] = "=", [DISPF] = "DISPF",
        [FILL] = "FILL", [POINTER] = "->", [FIND] = "FIND", [SRCH] = "SRCH",
        [GOSUB] = "GOSUB", [GOTO] = "GOTO", [SAY] = "SAY", [INIFLE] = "INIFLE",
        [UPDTA] = "UPDTA", [FINDV] = "FINDV", [NMENU] = "NMENU", [MOUNT] = "MOUNT",
        [XFER] = "XFER", [ON] = "ON", [OPEN] = "OPEN", [RET] = "RET",
        [ENTER] = "ENTER", [PBLNK] = "PBLNK", [PBOX] = "PBOX", [PCHR] = "PCHR",
        [PFMT] = "PFMT", [PON] = "PON", [BRKRET] = "BRKRET", [PVERT] = "PVERT",
        [INC] = "INC", [MID_CMD] = "MID", [CO] = "CO", [REDEF] = "REDEF",
        [REDSP] = "REDSP", [WINDEF] = "WINDEF", [WINACT] = "WINACT", [CHAIN] = "CHAIN",
        [SAVE] = "SAVE", [SAVES] = "SAVES", [SCROLL] = "SCROLL", [SORTA] = "SORTA",
        [PTOF] = "PTOF", [TRANSX] = "TRANSX", [TRAP] = "TRAP", [ULKALL] = "ULKALL",
        [WINDOW] = "WINDOW", [REENT] = "REENT", [IF] = "IF", [POPS] = "POPS",
        [CLSO] = "CLSO", [PUT_FLD] = "PUT_FLD", [SORT3] = "SORT3", [OPENV] = "OPENV",
        [REMVA] = "REMVA", [ELSE] = "ELSE", [WHILE] = "WHILE", [SELECT] = "SELECT",
        [ENDW] = "ENDW", [LOOP_IF] = "LOOP_IF", [EXIT_IF] = "EXIT_IF", [FOR] = "FOR",
        [FUNC] = "FUNC", [CMD] = "CMD", [UDC] = "UDC", [RAP] = "RAP",
        [CHAINR] = "CHAINR", [CLSPF] = "CLSPF", [LISTM] = "LISTM", [EXPORT] = "EXPORT",
        [RSCR] = "RSCR", [ADD] = "ADD", [LISTF] = "LISTF", [LIST] = "LIST",
        [REPL] = "REPL", [EXEC] = "EXEC", [QUIT] = "QUIT", [ERR] = "ERR",
        [OWNER] = "OWNER", [DALL] = "DALL", [ASK] = "ASK", [RCN_CMD] = "RCN",
        [REL] = "REL", [FORCE] = "FORCE", [DEC] = "DEC", [PAINT] = "PAINT",
        [DEALOC] = "DEALOC", [READ] = "READ", [WRITE] = "WRITE", [INSERT] = "INSERT",
        [SETLINE] = "SETLINE", [REMOUNT] = "REMOUNT", [SCRN] = "SCRN", [RDLIST] = "RDLIST",
        [MSG] = "MSG", [CLRPE] = "CLRPE", [UPAR] = "UPAR", [ALLOC] = "ALLOC",
        [PSET] = "PSET", [COLOR] = "COLOR", [IMPORT] = "IMPORT", [PARAM] = "PARAM",
        [XTRAP] = "XTRAP", [SETACT] = "SETACT", [BKG] = "BKG", [FRG] = "FRG",
        [REVERSE] = "REV", [FORMAT] = "FORMAT", [DISPM] = "DISPM", [FILLMEM] = "FILLMEM",
        [DELC] = "DELC", [GOTOL] = "GOTOL", [DELF] = "DELF", [RENF] = "RENF",
        [DATE] = "DATE", [TIME] = "TIME", [PUSHF] = "PUSHF", [POPF] = "POPF",
        [ROPEN] = "ROPEN", [FILTER] = "FILTER", [WRAP] = "WRAP", [REWRAP] = "REWRAP",
        [UP] = "UP", [CLOCK] = "CLOCK", [SCAN] = "SCAN", [SET_SCAN_FLG] = "SET_SCAN_FLG",
        [TRIM] = "TRIM", [IFDUP] = "IFDUP", [IFNA] = "IFNA", [PRTALL] = "PRTALL",
        [NOREDSP] = "NOREDSP", [AUTOINC] = "AUTOINC", [PUSHT] = "PUSHT", [POPT] = "POPT",
        [CASE] = "CASE", [PRT_NUM] = "PRT_NUM", [LD_PDRV] = "LD_PDRV", [GOSUBL] = "GOSUBL",
        [GETLBL] = "GETLBL", [CDPATH] = "CDPATH", [SOUND] = "SOUND", [TRACE] = "TRACE",
        [INT_CMD] = "INT", [LIST_EXIT] = "LIST_EXIT", [POKE] = "POKE", [PEEK] = "PEEK",
        [KBDUP] = "KBDUP", [NORSTRT] = "NORSTRT", [NOVLDMSG] = "NOVLDMSG",
        [AUTOENTER] = "AUTOENTER", [AUTODEC] = "AUTODEC", [SHOW_PLINE] = "SHOW_PLINE",
        [AUTO_RUN] = "AUTO_RUN", [RUN] = "RUN",
        [MEM_PTR] = "MEM_PTR", [MEM_SPC] = "MEM_SPC", [SAVES3] = "SAVES3",
        [REDSP3] = "REDSP3", [WRAP3] = "WRAP3", [SSPCF] = "SSPCF",
        [RDREC] = "RDREC", [WTREC] = "WTREC", [OPNO] = "OPNO",
        [EQU_MID] = "EQU_MID", [EQU_DAY] = "EQU_DAY", [EQU_XMT] = "EQU_XMT",
        [FORCE3] = "FORCE3", [MOUSE] = "MOUSE", [PLAYWAV] = "PLAYWAV",
        [COMM] = "COMM", [JUST] = "JUST", [PORT] = "PORT",
        [START_SCAN] = "START_SCAN", [AUTONEW] = "AUTONEW", [WCOLOR] = "WCOLOR",
        [BUTTON] = "BUTTON", [CAPTION] = "CAPTION", [LOAD_PICTURE] = "LOAD_PICTURE",
        [GRAY] = "GRAY", [BRACE_OPEN] = "{", [BRACE_CLOSE] = "}",
        [ROW_COLOR] = "ROW_COLOR", [HOT_SPOT] = "HOT_SPOT", [NEXT] = "NEXT",
        // Compiler commands
        [DEFINE] = "DEFINE", [ENDIF] = "ENDIF", [ELSE_IF] = "ELSE_IF", [LOOP] = "LOOP",
        [OTHERWISE] = "OTHERWISE", [ENDC] = "ENDC", [EXIT_CMD] = "EXIT",
        [FLOOP] = "FLOOP", [FEXIT] = "FEXIT", [FLOOP_IF] = "FLOOP_IF",
        [FEXIT_IF] = "FEXIT_IF", [SEXIT] = "SEXIT", [SEXIT_IF] = "SEXIT_IF",
        [SLOOP] = "SLOOP", [SLOOP_IF] = "SLOOP_IF", [ENDS] = "ENDS",
    };

    /// <summary>Get the command name for an opcode number.</summary>
    public static string GetName(ushort cmdNum) =>
        _names.TryGetValue(cmdNum, out var name) ? name : $"UNK_{cmdNum}";
}
