# Advanced Accounting 5.1 for DOS

**Publisher:** Addsum Business Software, Inc. (https://www.addsuminc.com/)
**Original Release:** Fall 1996
**Written In:** TAS Professional 5.1
**Database Engine:** Btrieve (optionally Pervasive/Actian Zen)

## Why CoTAS Exists

Advanced Accounting 5.1 for DOS is the primary target application for CoTAS. The sole purpose of the CoTAS runtime is to run Advanced Accounting source code and compiled programs **without** the original TAS 5.1 runtime or compiler — replacing both the aging 16-bit execution environment and the expensive Btrieve/Pervasive database engine with modern, open-source alternatives.

## Modules (13 integrated)

1. **General Ledger** — Fully transactionalized, post back to prior years (up to 5), 6 years of summary data, departmental support, consolidated financials
2. **Accounts Receivable** — Customer management, aging, collections tracking with notes
3. **Accounts Payable** — Vendor management, payment processing
4. **Sales Order** — Full sales order entry and processing, bar code support
5. **Purchase Order** — Multi-location receiving on same PO
6. **Inventory Control** — Multi-location, last cost and average cost tracked at location level, 25-character product codes, bar code field
7. **Payroll** — User-maintained tax tables (no forced annual update purchases)
8. **Point of Sale** — True integrated POS with cash drawers, pole displays, register/clerk support, audit trails, sales-by-hour reports, fully GUI
9. **Job Cost** — Project-based cost tracking
10. **Bill of Materials** — Multi-level finished goods and sub-assemblies, up to 250 parts per level, work orders from sales orders
11. **Consignment** — Both consignee and consignor handling
12. **Report Output** — Built-in output to PDF, RTF, HTML, Excel, Quattro, Lotus, text, graphic, report emulation (no additional purchase required)

## Key Features

- **Custom Modifiable:** Developers can modify the actual individual program source code (not just UI options) — highest level of integration, integrity, and efficiency
- **Unlimited Users:** Multi-user license has no per-seat limits
- **Minimal Hardware:** If the PC runs Windows 98+, it runs Advanced Accounting
- **Printer Flexibility:** Full support for dot matrix and multi-part forms, plus Windows-style printing
- **Bar Code Support:** Code 39, Codabar, Code 128, UPC, and more; bar code labels, bar coded sales/purchase orders
- **Word-Wrapped Notes:** Unlimited notes on customers, vendors, GL accounts, employees, SO/PO line items
- **Multi-Company:** Single installation supports 1300+ companies
- **Security:** User permissions, module hiding, branch office restrictions, date range controls
- **Non-Proprietary Data:** Data readable by other programs, all data exportable
- **CPA Integration:** GL export to Excel, Lotus, tab delimited, CSV, or QuickBooks IIF

## Platform Compatibility

- DOS natively (fastest mode)
- Windows 98, 2000, XP, Vista, 7, 8, 10, 11 (32-bit)
- 64-bit via Windows Virtual PC (Win 7 Professional+)
- Windows Server: NT/2000/2003/2008/2011 SBS/2012/2016/2019/2022
- Novell 4.2+
- DOS and Windows modes run simultaneously on same data

## Why Modernization Is Needed

1. **16-bit runtime** — The TAS 5.1 runtime is 16-bit DOS code; 64-bit Windows cannot run it natively
2. **Btrieve dependency** — Requires Pervasive/Actian Zen licenses (paid, per-seat)
3. **No web access** — Character-mode DOS interface, no remote/browser access
4. **Original vendor defunct** — Business Tools, Inc. website (business-tools.com) has been defunct for over 10 years
5. **Thousands of users** — Active installations still depend on this software daily
6. **Decades of customization** — Many installations have extensive custom modifications that cannot simply be ported to a different platform

## What CoTAS Provides

- Run existing TAS 5.1 source code (.SRC) directly — no compilation needed
- Run existing compiled programs (.RUN) via decompilation
- Replace Btrieve with MSSQL/MySQL/PostgreSQL — no paid database drivers
- Web-based terminal UI — access from any browser, anywhere
- Cross-platform — Windows, Linux, macOS
- .NET 10 runtime — modern, supported, actively maintained
- Multi-user via SignalR — each connection gets isolated session
- Preserves the full TAS 5.1 command set (127 commands, 170+ functions)
