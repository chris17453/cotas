namespace CoTAS.Interpreter.Functions;

/// <summary>
/// Date and time functions: DATE, TIME, MNTH, YEAR, DOM, DOW, CDOW, DTOC, CTOD, DTOS, DIFF, MDY
/// </summary>
public static class DateTimeFunctions
{
    public static void Register(Dictionary<string, Func<List<TasValue>, TasValue>> registry)
    {
        registry["DATE"] = Date;
        registry["TIME"] = Time;
        registry["MNTH"] = Mnth;
        registry["YEAR"] = Year;
        registry["DOM"] = Dom;
        registry["DOW"] = Dow;
        registry["CDOW"] = Cdow;
        registry["CMNTH"] = Cmnth;
        registry["DTOC"] = Dtoc;
        registry["CTOD"] = Ctod;
        registry["DTOS"] = Dtos;
        registry["DIFF"] = Diff;
        registry["MDY"] = Mdy;
        registry["FLDATE"] = FlDate;
        registry["FLTIME"] = FlTime;
    }

    private static TasValue Date(List<TasValue> args)
    {
        // DATE() returns current date as string MM/DD/YY
        string d = DateTime.Now.ToString("MM/dd/yy");
        return new TasValue(TasType.Alpha, d, 8);
    }

    private static TasValue Time(List<TasValue> args)
    {
        // TIME() returns current time as HH:MM:SS
        string t = DateTime.Now.ToString("HH:mm:ss");
        return new TasValue(TasType.Alpha, t, 8);
    }

    private static TasValue Mnth(List<TasValue> args)
    {
        // MNTH(date) returns month number
        var dt = ParseDate(args.Count > 0 ? args[0].AsString() : "");
        return new TasValue(TasType.Integer, dt?.Month ?? 0);
    }

    private static TasValue Year(List<TasValue> args)
    {
        // YEAR(date) returns 4-digit year
        var dt = ParseDate(args.Count > 0 ? args[0].AsString() : "");
        return new TasValue(TasType.Integer, dt?.Year ?? 0);
    }

    private static TasValue Dom(List<TasValue> args)
    {
        // DOM(date) returns day of month
        var dt = ParseDate(args.Count > 0 ? args[0].AsString() : "");
        return new TasValue(TasType.Integer, dt?.Day ?? 0);
    }

    private static TasValue Dow(List<TasValue> args)
    {
        // DOW(date) returns day of week (1=Sunday, 7=Saturday)
        var dt = ParseDate(args.Count > 0 ? args[0].AsString() : "");
        if (dt == null) return new TasValue(TasType.Integer, 0);
        return new TasValue(TasType.Integer, (int)dt.Value.DayOfWeek + 1);
    }

    private static TasValue Cdow(List<TasValue> args)
    {
        // CDOW(date) returns day name (e.g., "Monday")
        var dt = ParseDate(args.Count > 0 ? args[0].AsString() : "");
        if (dt == null) return new TasValue(TasType.Alpha, "", 9);
        string name = dt.Value.DayOfWeek.ToString();
        return new TasValue(TasType.Alpha, name, 9);
    }

    private static TasValue Cmnth(List<TasValue> args)
    {
        // CMNTH(date) returns month name
        var dt = ParseDate(args.Count > 0 ? args[0].AsString() : "");
        if (dt == null) return new TasValue(TasType.Alpha, "", 9);
        string name = dt.Value.ToString("MMMM");
        return new TasValue(TasType.Alpha, name, 9);
    }

    private static TasValue Dtoc(List<TasValue> args)
    {
        // DTOC(date) converts date to character MM/DD/YY
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 8);
        var dt = ParseDate(args[0].AsString());
        if (dt == null) return new TasValue(TasType.Alpha, "", 8);
        string s = dt.Value.ToString("MM/dd/yy");
        return new TasValue(TasType.Alpha, s, 8);
    }

    private static TasValue Ctod(List<TasValue> args)
    {
        // CTOD(string) converts character to date
        if (args.Count < 1) return new TasValue(TasType.Date, "", 8);
        string s = args[0].AsString().Trim();
        var dt = ParseDate(s);
        if (dt == null) return new TasValue(TasType.Date, "", 8);
        return new TasValue(TasType.Date, dt.Value.ToString("yyyyMMdd"), 8);
    }

    private static TasValue Dtos(List<TasValue> args)
    {
        // DTOS(date) returns YYYYMMDD string (sortable)
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 8);
        var dt = ParseDate(args[0].AsString());
        if (dt == null) return new TasValue(TasType.Alpha, "", 8);
        return new TasValue(TasType.Alpha, dt.Value.ToString("yyyyMMdd"), 8);
    }

    private static TasValue Diff(List<TasValue> args)
    {
        // DIFF(date1, date2) returns number of days between dates
        if (args.Count < 2) return new TasValue(TasType.Integer, 0);
        var dt1 = ParseDate(args[0].AsString());
        var dt2 = ParseDate(args[1].AsString());
        if (dt1 == null || dt2 == null) return new TasValue(TasType.Integer, 0);
        int days = (int)(dt2.Value - dt1.Value).TotalDays;
        return new TasValue(TasType.Integer, days);
    }

    private static TasValue Mdy(List<TasValue> args)
    {
        // MDY(date) returns "Month DD, YYYY" format
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 18);
        var dt = ParseDate(args[0].AsString());
        if (dt == null) return new TasValue(TasType.Alpha, "", 18);
        string s = dt.Value.ToString("MMMM dd, yyyy");
        return new TasValue(TasType.Alpha, s, 18);
    }

    private static TasValue FlDate(List<TasValue> args)
    {
        // FLDATE() returns date of last file access (stub: returns today)
        string d = DateTime.Now.ToString("MM/dd/yy");
        return new TasValue(TasType.Alpha, d, 8);
    }

    private static TasValue FlTime(List<TasValue> args)
    {
        // FLTIME() returns time of last file access (stub: returns now)
        string t = DateTime.Now.ToString("HH:mm:ss");
        return new TasValue(TasType.Alpha, t, 8);
    }

    /// <summary>
    /// Parse a date string in various TAS formats: MM/DD/YY, MM/DD/YYYY, YYYYMMDD
    /// </summary>
    internal static DateTime? ParseDate(string s)
    {
        s = s.Trim();
        if (string.IsNullOrEmpty(s)) return null;

        // YYYYMMDD format
        if (s.Length == 8 && !s.Contains('/') && !s.Contains('-'))
        {
            if (DateTime.TryParseExact(s, "yyyyMMdd", null, System.Globalization.DateTimeStyles.None, out var dt1))
                return dt1;
        }

        // Try common formats
        string[] formats = ["MM/dd/yy", "MM/dd/yyyy", "M/d/yyyy", "M/d/yy", "yyyy-MM-dd"];
        if (DateTime.TryParseExact(s, formats, null, System.Globalization.DateTimeStyles.None, out var dt))
            return dt;

        // Fallback: generic parse
        if (DateTime.TryParse(s, out dt))
            return dt;

        return null;
    }
}
