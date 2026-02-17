#!/usr/bin/env python3
"""
Generate C# compiler table files from Pascal source.
Reads cmpspec.pas, cmpspec2.pas, cmpspec3.pas, specline.pas, General.pas
and produces:
  - CommandSpec.cs     (types + CompileAction enum)
  - SpecOffsets.cs     (spec offset constants)
  - OptionTable.cs     (320 option keywords)
  - CommandTable.cs    (CmdList + rc_/cc_ arrays)
"""

import re
import sys
from pathlib import Path

PASCAL_DIR = Path("doodles/delphi_code/editor_tools")
OUTPUT_DIR = Path("src/CoTAS.Compiler")

def read_file(name):
    return (PASCAL_DIR / name).read_text(encoding="utf-8", errors="replace")


def strip_pascal_comments(text):
    """Remove Pascal (* ... *) and { ... } comments."""
    # Remove (* ... *) style comments (multiline)
    text = re.sub(r'\(\*.*?\*\)', '', text, flags=re.DOTALL)
    return text


# ─── 1. Extract CompileAction constants from General.pas ───────────────────

def extract_compile_actions(text):
    """Extract p_e=1, c_f=3, ... and special comp_if=102, etc."""
    actions = {}
    for m in re.finditer(r'^\s*(\w+)\s*=\s*(\d+)\s*;', text, re.MULTILINE):
        name, val = m.group(1), int(m.group(2))
        # Only take the compiler op types (p_e through chk_chr_ft + special 101-136)
        if name in ('p_e','p_f','c_f','l_l','c_file','c_file2','chk_yn','chk_ny',
                     'chk_chr','c_f_all','c_slv_rel','c_f_all_rfo','chk_mem',
                     'c_trace','chk_trap','if_l_l','chk_f_fld','chk_rel_fld',
                     'chk_elbl','c_key','c_f_mount','c_schema','chk_chr1',
                     'c_at','set_y','set_n','chk_chr2','c_scope','c_ufc',
                     'c_tops','c_upda_val','c_ufc_cf','l_udf',
                     'c_f_all_no_exp','c_ca_val','cc1_p_e','chk_chr_ft'):
            actions[name] = val
        elif name.startswith('define_fld') or name.startswith('comp_') or \
             name.startswith('start_brace') or name.startswith('stop_brace') or \
             name.startswith('setup_') or name.startswith('parse_on') or \
             name.startswith('get_scrn'):
            actions[name] = val
    return actions

# ─── 2. Extract char lists from General.pas ───────────────────────────────

def extract_char_lists(text):
    """Extract spdc, arnf, br, lta, etc."""
    lists = {}
    for m in re.finditer(r"^\s*(\w+):\s*string\s*=\s*'([^']*)'", text, re.MULTILINE):
        lists[m.group(1)] = m.group(2)
    return lists

# ─── 3. Extract OptionList from cmpspec3.pas ──────────────────────────────

def extract_option_list(text):
    """Extract the 320-entry OptionList array."""
    m = re.search(r"OptionList\s*:\s*array\[1\.\.320\]\s*of\s*string\s*=\s*\((.*?)\);", text, re.DOTALL)
    if not m:
        raise ValueError("OptionList not found")
    body = m.group(1)
    # Strip Pascal { } comments
    body = re.sub(r'\{[^}]*\}', '', body)
    # Match quoted strings
    options = [o.strip() for o in re.findall(r"'([^']+)'", body)]
    return options

# ─── 4. Extract CmdList from cmpspec3.pas ─────────────────────────────────

def extract_cmd_list(text):
    """Extract CmdList entries from the real (non-commented) CmdList array."""
    # Find the REAL CmdList (after the commented example)
    m = re.search(r'CmdList\s*:\s*array\[0\.\.LastTrueCmdNum\+14\]\s*of\s*TCmdStruct\s*=\s*\(', text)
    if not m:
        raise ValueError("CmdList not found")
    start = m.end()
    # Find closing ); 
    end_m = re.search(r'\)\s*\)', text[start:])
    if not end_m:
        raise ValueError("CmdList end not found")
    body = text[start:start+end_m.end()]
    
    cmds = []
    for em in re.finditer(
        r"\(Name:\s*'([^']*)';\s*"
        r"(?:[Nn]um|NUM):\s*(\d+);\s*"
        r"Size:\s*(\d+|rap_size);\s*"
        r"Options:\s*(@rc_\w+|nil);\s*"
        r"Dflts:\s*(@cc_\w+|nil)"
        r"(?:;\s*SpclCmp:\s*(True|False))?",
        body, re.IGNORECASE
    ):
        cmds.append({
            'name': em.group(1),
            'num': int(em.group(2)),
            'size': em.group(3),
            'options': em.group(4) if em.group(4) != 'nil' else None,
            'defaults': em.group(5) if em.group(5) != 'nil' else None,
            'special': em.group(6) == 'True' if em.group(6) else False,
        })
    return cmds

# ─── 5. Extract standalone command structs (AbsJmp, etc.) ─────────────────

def extract_standalone_cmds(text):
    """Extract AbsJmp, DoIf, etc."""
    standalones = {}
    for m in re.finditer(
        r"(\w+)\s*:\s*TCmdStruct\s*=\s*\(Name:\s*'([^']*)';\s*"
        r"Num:\s*(\d+);\s*Size:\s*(\d+);\s*"
        r"Options:\s*(@rc_\w+|nil);\s*Dflts:\s*(@cc_\w+|nil)"
        r"(?:;\s*SpclCmp:\s*(True|False))?\s*\)",
        text, re.IGNORECASE
    ):
        standalones[m.group(1)] = {
            'name': m.group(2),
            'num': int(m.group(3)),
            'size': int(m.group(4)),
            'options': m.group(5) if m.group(5) != 'nil' else None,
            'defaults': m.group(6) if m.group(6) != 'nil' else None,
            'special': m.group(7) == 'True' if m.group(7) else False,
        }
    return standalones

# ─── 6. Extract rc_ and cc_ arrays ──────────────────────────────────────

def extract_option_arrays(text):
    """Extract all rc_ arrays (TCStruct)."""
    arrays = {}
    for m in re.finditer(
        r'rc_(\w+)\s*:\s*array\[0\.\.(\d+)\]\s*of\s*TCStruct\s*=\s*\((.*?)\);',
        text, re.DOTALL
    ):
        name = m.group(1)
        body = m.group(3)
        entries = []
        for em in re.finditer(
            r"\(O:\s*(\d+);\s*D:\s*(\w+);\s*S:\s*(\w+)(?:;\s*[Cc]:\s*'([^']*)')?\)",
            body
        ):
            o, d, s, c = em.group(1), em.group(2), em.group(3), em.group(4)
            entries.append({'O': int(o), 'D': d, 'S': s, 'C': c})
        arrays[name] = entries
    return arrays

def extract_default_arrays(text):
    """Extract all cc_ arrays (TCDflt)."""
    arrays = {}
    for m in re.finditer(
        r'cc_(\w+)\s*:\s*array\[0\.\.(\d+)\]\s*of\s*TCDflt\s*=\s*\((.*?)\);',
        text, re.DOTALL
    ):
        name = m.group(1)
        body = m.group(3)
        entries = []
        for em in re.finditer(
            r"\(D:\s*(\w+);\s*S:\s*(\w+)(?:;\s*[Cc]:\s*'([^']*)')?\)",
            body
        ):
            d, s, c = em.group(1), em.group(2), em.group(3)
            entries.append({'D': d, 'S': s, 'C': c})
        arrays[name] = entries
    return arrays

# ─── 7. Extract spec offset constants from specline.pas ───────────────────

def extract_spec_offsets(text):
    """Extract all constant = value pairs."""
    offsets = {}
    for m in re.finditer(r'^\s*(\w+)\s*=\s*(\d+)\s*;', text, re.MULTILINE):
        offsets[m.group(1)] = int(m.group(2))
    return offsets

# Also look for rap_size
def extract_rap_size(text):
    # rap_size = rap_no_save+1+1; where rap_no_save = 19
    rap_no_save = 19
    for m in re.finditer(r'^\s*rap_no_save\s*=\s*(\d+)\s*;', text, re.MULTILINE):
        rap_no_save = int(m.group(1))
    for m in re.finditer(r'^\s*rap_size\s*=\s*rap_no_save\s*\+\s*1\s*\+\s*1\s*;', text, re.MULTILINE):
        return rap_no_save + 1 + 1
    for m in re.finditer(r'^\s*rap_size\s*=\s*(\d+)\s*;', text, re.MULTILINE):
        return int(m.group(1))
    return rap_no_save + 1 + 1  # 21


# ═══════════════════════════════════════════════════════════════════════════
#  C# CODE GENERATION
# ═══════════════════════════════════════════════════════════════════════════

def to_pascal_case(s):
    """Convert snake_case to PascalCase."""
    return ''.join(w.capitalize() for w in s.split('_'))

def generate_command_spec_cs(actions):
    """Generate CommandSpec.cs with types and CompileAction enum."""
    lines = []
    lines.append("// AUTO-GENERATED from Pascal source by generate_compiler_tables.py")
    lines.append("// DO NOT EDIT MANUALLY")
    lines.append("")
    lines.append("namespace CoTAS.Compiler;")
    lines.append("")
    
    # CompileAction enum
    lines.append("/// <summary>Compilation primitive actions from the Pascal compiler.</summary>")
    lines.append("public enum CompileAction")
    lines.append("{")
    lines.append("    None = 0,")
    
    # Standard actions (1-37)
    standard = [(n, v) for n, v in actions.items() if v <= 37]
    standard.sort(key=lambda x: x[1])
    for name, val in standard:
        cs_name = to_pascal_case(name)
        lines.append(f"    {cs_name} = {val},")
    
    lines.append("")
    lines.append("    // Special compilation routines (control flow)")
    
    # Special actions (101+)
    special = [(n, v) for n, v in actions.items() if v > 37]
    special.sort(key=lambda x: x[1])
    for name, val in special:
        cs_name = to_pascal_case(name)
        lines.append(f"    {cs_name} = {val},")
    
    lines.append("}")
    lines.append("")
    
    # OptionDef record
    lines.append("/// <summary>One option spec for a command (maps keyword to compile action + spec offset).</summary>")
    lines.append("public readonly record struct OptionDef(")
    lines.append("    int OptionIndex,       // Index into OptionTable.Keywords (0 = positional/first arg)")
    lines.append("    CompileAction Action,  // What compilation primitive to use")
    lines.append("    int SpecOffset,        // Byte offset in spec line to write result")
    lines.append("    string? CharList = null // Valid characters for chk_chr-type actions")
    lines.append(");")
    lines.append("")
    
    # DefaultDef record
    lines.append("/// <summary>Default value spec for unset options.</summary>")
    lines.append("public readonly record struct DefaultDef(")
    lines.append("    CompileAction Action,  // Default action (chk_yn, chk_ny, chk_chr, 0=none)")
    lines.append("    int SpecOffset,        // Byte offset in spec line")
    lines.append("    string? CharList = null // Default char for chk_chr")
    lines.append(");")
    lines.append("")
    
    # CommandDef record
    lines.append("/// <summary>Complete command definition (mirrors Pascal TCmdStruct).</summary>")
    lines.append("public sealed class CommandDef")
    lines.append("{")
    lines.append("    public required string Name { get; init; }")
    lines.append("    public required int Opcode { get; init; }")
    lines.append("    public required int SpecSize { get; init; }")
    lines.append("    public OptionDef[] Options { get; init; } = [];")
    lines.append("    public DefaultDef[] Defaults { get; init; } = [];")
    lines.append("    public bool IsSpecial { get; init; }")
    lines.append("}")
    
    return '\n'.join(lines)


def generate_spec_offsets_cs(offsets):
    """Generate SpecOffsets.cs with all constants."""
    lines = []
    lines.append("// AUTO-GENERATED from specline.pas by generate_compiler_tables.py")
    lines.append("// DO NOT EDIT MANUALLY")
    lines.append("")
    lines.append("namespace CoTAS.Compiler;")
    lines.append("")
    lines.append("/// <summary>Spec line byte offsets for each command's parameters.</summary>")
    lines.append("public static class SpecOffsets")
    lines.append("{")
    
    # Group by prefix (command name)
    groups = {}
    for name, val in sorted(offsets.items(), key=lambda x: x[0]):
        prefix = name.split('_')[0] if '_' in name else name
        if prefix not in groups:
            groups[prefix] = []
        groups[prefix].append((name, val))
    
    for prefix in sorted(groups.keys()):
        for name, val in groups[prefix]:
            cs_name = to_pascal_case(name)
            lines.append(f"    public const int {cs_name} = {val};")
    
    lines.append("}")
    
    return '\n'.join(lines)


def generate_option_table_cs(options):
    """Generate OptionTable.cs with the 320 keywords."""
    lines = []
    lines.append("// AUTO-GENERATED from cmpspec3.pas by generate_compiler_tables.py")
    lines.append("// DO NOT EDIT MANUALLY")
    lines.append("")
    lines.append("namespace CoTAS.Compiler;")
    lines.append("")
    lines.append("/// <summary>320 option keywords used in command syntax (1-indexed in Pascal, 0-indexed here).</summary>")
    lines.append("public static class OptionTable")
    lines.append("{")
    lines.append("    /// <summary>Option keywords indexed from 0. Pascal index N = C# index N-1.</summary>")
    lines.append("    public static readonly string[] Keywords =")
    lines.append("    [")
    for i, opt in enumerate(options):
        lines.append(f'        "{opt}", // {i+1}')
    lines.append("    ];")
    lines.append("}")
    
    return '\n'.join(lines)


def resolve_spec_offset(name, offsets):
    """Resolve a spec offset name to its numeric value (case-insensitive)."""
    if name == '0':
        return 0
    if name in offsets:
        return offsets[name]
    # Case-insensitive lookup
    name_lower = name.lower()
    for k, v in offsets.items():
        if k.lower() == name_lower:
            return v
    return None


def resolve_action(name, actions):
    """Resolve a compile action name to its enum name."""
    if name == '0':
        return 'CompileAction.None'
    # If it's a pure number, look up by value
    if name.isdigit():
        val = int(name)
        for aname, aval in actions.items():
            if aval == val:
                return f'CompileAction.{to_pascal_case(aname)}'
        return f'(CompileAction){name}'
    cs_name = to_pascal_case(name)
    return f'CompileAction.{cs_name}'


def generate_command_table_cs(cmds, rc_arrays, cc_arrays, offsets, actions, rap_size):
    """Generate CommandTable.cs with all command definitions."""
    lines = []
    lines.append("// AUTO-GENERATED from cmpspec*.pas by generate_compiler_tables.py")
    lines.append("// DO NOT EDIT MANUALLY")
    lines.append("")
    lines.append("using System.Collections.Frozen;")
    lines.append("")
    lines.append("namespace CoTAS.Compiler;")
    lines.append("")
    lines.append("/// <summary>Complete command table - all ~140 TAS commands with options and defaults.</summary>")
    lines.append("public static class CommandTable")
    lines.append("{")
    
    # Build dictionary
    lines.append("    /// <summary>Lookup command by name (case-insensitive).</summary>")
    lines.append("    public static readonly FrozenDictionary<string, CommandDef> Commands;")
    lines.append("")
    lines.append("    /// <summary>All command definitions in declaration order.</summary>")
    lines.append("    public static readonly CommandDef[] AllCommands;")
    lines.append("")
    lines.append("    static CommandTable()")
    lines.append("    {")
    lines.append("        AllCommands = BuildAllCommands();")
    lines.append("        var dict = new Dictionary<string, CommandDef>(StringComparer.OrdinalIgnoreCase);")
    lines.append("        foreach (var cmd in AllCommands)")
    lines.append("        {")
    lines.append("            dict.TryAdd(cmd.Name, cmd); // first wins for duplicates like ? = PMSG")
    lines.append("        }")
    lines.append("        Commands = dict.ToFrozenDictionary(StringComparer.OrdinalIgnoreCase);")
    lines.append("    }")
    lines.append("")
    lines.append("    private static CommandDef[] BuildAllCommands() =>")
    lines.append("    [")
    
    unresolved_offsets = set()
    
    for cmd in cmds:
        size_str = str(rap_size) if cmd['size'] == 'rap_size' else cmd['size']
        spcl_str = "true" if cmd['special'] else "false"
        
        # Build options
        opt_entries = []
        if cmd['options']:
            rc_name = cmd['options'].replace('@rc_', '')
            if rc_name in rc_arrays:
                for entry in rc_arrays[rc_name]:
                    if entry['O'] == 0 and entry['D'] == '0' and entry['S'] == '0':
                        continue  # terminator
                    action = resolve_action(entry['D'], actions)
                    offset = resolve_spec_offset(entry['S'], offsets)
                    if offset is None:
                        unresolved_offsets.add(entry['S'])
                        offset_str = f"0 /* TODO: {entry['S']} */"
                    else:
                        offset_str = str(offset)
                    c_str = f', "{entry["C"]}"' if entry.get('C') else ''
                    opt_entries.append(f'new({entry["O"]}, {action}, {offset_str}{c_str})')
        
        # Build defaults
        dflt_entries = []
        if cmd['defaults']:
            cc_name = cmd['defaults'].replace('@cc_', '')
            if cc_name in cc_arrays:
                for entry in cc_arrays[cc_name]:
                    if entry['D'] == '0' and entry['S'] == '0':
                        continue  # terminator
                    action = resolve_action(entry['D'], actions)
                    offset = resolve_spec_offset(entry['S'], offsets)
                    if offset is None:
                        unresolved_offsets.add(entry['S'])
                        offset_str = f"0 /* TODO: {entry['S']} */"
                    else:
                        offset_str = str(offset)
                    c_str = f', "{entry["C"]}"' if entry.get('C') else ''
                    dflt_entries.append(f'new({action}, {offset_str}{c_str})')
        
        # Emit command
        lines.append(f'        new() {{ Name = "{cmd["name"]}", Opcode = {cmd["num"]}, SpecSize = {size_str}, IsSpecial = {spcl_str},')
        
        if opt_entries:
            lines.append(f'            Options = [{", ".join(opt_entries)}],')
        if dflt_entries:
            lines.append(f'            Defaults = [{", ".join(dflt_entries)}],')
        
        lines.append(f'        }},')
    
    lines.append("    ];")
    lines.append("}")
    
    if unresolved_offsets:
        print(f"WARNING: {len(unresolved_offsets)} unresolved spec offsets:", file=sys.stderr)
        for s in sorted(unresolved_offsets):
            print(f"  {s}", file=sys.stderr)
    
    return '\n'.join(lines)


def main():
    general = read_file("General.pas")
    specline = read_file("specline.pas")
    cmpspec1 = strip_pascal_comments(read_file("cmpspec.pas"))
    cmpspec2 = strip_pascal_comments(read_file("cmpspec2.pas"))
    cmpspec3 = strip_pascal_comments(read_file("cmpspec3.pas"))
    
    all_specs = cmpspec1 + "\n" + cmpspec2 + "\n" + cmpspec3
    
    # Extract data
    actions = extract_compile_actions(general)
    print(f"Extracted {len(actions)} compile actions")
    
    char_lists = extract_char_lists(general)
    print(f"Extracted {len(char_lists)} char lists")
    
    offsets = extract_spec_offsets(specline)
    print(f"Extracted {len(offsets)} spec offsets")
    
    rap_size = extract_rap_size(specline)
    print(f"rap_size = {rap_size}")
    
    options = extract_option_list(cmpspec3)
    print(f"Extracted {len(options)} option keywords")
    
    cmds = extract_cmd_list(cmpspec3)
    print(f"Extracted {len(cmds)} CmdList entries")
    
    rc_arrays = extract_option_arrays(all_specs)
    print(f"Extracted {len(rc_arrays)} rc_ option arrays")
    
    cc_arrays = extract_default_arrays(all_specs)
    print(f"Extracted {len(cc_arrays)} cc_ default arrays")
    
    # Generate C# files
    cs = generate_command_spec_cs(actions)
    (OUTPUT_DIR / "CommandSpec.cs").write_text(cs)
    print(f"Generated CommandSpec.cs ({len(cs)} bytes)")
    
    cs = generate_spec_offsets_cs(offsets)
    (OUTPUT_DIR / "SpecOffsets.cs").write_text(cs)
    print(f"Generated SpecOffsets.cs ({len(cs)} bytes)")
    
    cs = generate_option_table_cs(options)
    (OUTPUT_DIR / "OptionTable.cs").write_text(cs)
    print(f"Generated OptionTable.cs ({len(cs)} bytes)")
    
    cs = generate_command_table_cs(cmds, rc_arrays, cc_arrays, offsets, actions, rap_size)
    (OUTPUT_DIR / "CommandTable.cs").write_text(cs)
    print(f"Generated CommandTable.cs ({len(cs)} bytes)")
    
    print("\nDone!")

if __name__ == "__main__":
    main()
