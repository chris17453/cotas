namespace CoTAS.Interpreter.Functions;

/// <summary>
/// Screen/display functions backed by FieldManager screen state.
/// </summary>
public static class ScreenFunctions
{
    private static FieldManager? _fields;

    public static void Register(Dictionary<string, Func<List<TasValue>, TasValue>> registry, FieldManager? fields = null)
    {
        _fields = fields;
        registry["ROW"] = Row;
        registry["COL"] = Col;
        registry["LROW"] = LRow;
        registry["MCOL"] = MCol;
        registry["MROW"] = MRow;
        registry["PCOL"] = PCol;
        registry["PROW"] = PRow;
        registry["PWHR"] = PWhr;
        registry["MAX_ROWS"] = MaxRows;
        registry["MAX_COLS"] = MaxCols;
        registry["SCRCHR"] = ScrChr;
        registry["ISCLR"] = IsClr;
        registry["WINDOWS"] = Windows;
        registry["WIN_LASER_PRT"] = WinLaserPrt;
        registry["WINDOW_PTR"] = WindowPtr;
        registry["GET_WIN_COLOR"] = GetWinColor;
        registry["MOUSE_ACT"] = MouseAct;
        registry["MOUSE_ON"] = MouseOn;
    }

    public static void SetFieldManager(FieldManager fields) => _fields = fields;

    private static TasValue Row(List<TasValue> args) =>
        new(TasType.Integer, _fields?.CursorRow ?? 1);

    private static TasValue Col(List<TasValue> args) =>
        new(TasType.Integer, _fields?.CursorCol ?? 1);

    private static TasValue LRow(List<TasValue> args) =>
        new(TasType.Integer, 25); // standard TAS screen

    private static TasValue MCol(List<TasValue> args) =>
        new(TasType.Integer, 80);

    private static TasValue MRow(List<TasValue> args) =>
        new(TasType.Integer, 25);

    private static TasValue PCol(List<TasValue> args) =>
        new(TasType.Integer, _fields?.PrinterCol ?? 1);

    private static TasValue PRow(List<TasValue> args) =>
        new(TasType.Integer, _fields?.PrinterRow ?? 1);

    private static TasValue PWhr(List<TasValue> args)
    {
        int r = _fields?.PrinterRow ?? 1;
        int c = _fields?.PrinterCol ?? 1;
        return new TasValue(TasType.Alpha, $"{r:D2},{c:D2}", 5);
    }

    private static TasValue MaxRows(List<TasValue> args) =>
        new(TasType.Integer, 25);

    private static TasValue MaxCols(List<TasValue> args) =>
        new(TasType.Integer, 80);

    private static TasValue ScrChr(List<TasValue> args)
    {
        // SCRCHR(row, col) - get character at screen position
        if (args.Count < 2) return new TasValue(TasType.Alpha, " ", 1);
        int row = args[0].AsInteger();
        int col = args[1].AsInteger();
        char ch = _fields?.GetScreenChar(row, col) ?? ' ';
        return new TasValue(TasType.Alpha, ch.ToString(), 1);
    }

    private static TasValue IsClr(List<TasValue> args) =>
        new(TasType.Logical, _fields?.IsScreenClear() ?? true);

    private static TasValue Windows(List<TasValue> args) =>
        new(TasType.Logical, true); // always in windows mode

    private static TasValue WinLaserPrt(List<TasValue> args) =>
        new(TasType.Logical, true); // laser printer available

    private static TasValue WindowPtr(List<TasValue> args) =>
        new(TasType.Integer, 0); // no active window

    private static TasValue GetWinColor(List<TasValue> args) =>
        new(TasType.Integer, 7); // default white on black

    private static TasValue MouseAct(List<TasValue> args) =>
        new(TasType.Logical, true); // mouse is always active in web mode

    private static TasValue MouseOn(List<TasValue> args) =>
        new(TasType.Logical, true); // mouse is always on in web mode
}
