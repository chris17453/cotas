#!/usr/bin/env python3
"""Binary diff tool for TAS .RUN files — section-by-section comparison."""
import struct, sys, os

def read_header(data):
    """Parse .RUN 128-byte header."""
    if len(data) < 128:
        return None
    fields = struct.unpack_from('<7i', data, 0)
    h = {
        'code_size':    fields[0],
        'const_size':   fields[1],
        'spec_size':    fields[2],
        'label_size':   fields[3],
        'scrn_fld_num': fields[4],
        'num_flds':     fields[5],
        'temp_flds':    fields[6],
    }
    more = struct.unpack_from('<5i', data, 28)
    h['num_temp_flds']  = more[0]
    h['fld_name_size']  = more[1]
    h['temp_fld_size']  = more[2]
    h['def_fld_seg_size'] = more[3]
    h['num_extra_flds'] = more[4]
    h['prg_names']      = struct.unpack_from('<i', data, 48)[0]
    h['debug_flg']      = data[52]
    h['pro_type']        = data[53:58].decode('ascii', errors='replace')
    h['num_labels']      = struct.unpack_from('<i', data, 58)[0]
    h['new_fld_spec']    = data[62]
    h['chk_up_vld']      = data[63]
    h['inc_labels']      = data[64]
    h['obj_used']        = data[78]
    return h

def get_sections(data, h):
    """Return dict of named sections with their byte ranges."""
    off = 128
    buf_end = off + 1600
    code_end = buf_end + h['code_size']
    const_end = code_end + h['const_size']
    spec_end = const_end + h['spec_size']
    label_end = spec_end + h['label_size']
    field_end = label_end + h['fld_name_size']
    return {
        'header':    (0, 128),
        'buffers':   (128, buf_end),
        'code':      (buf_end, code_end),
        'constants': (code_end, const_end),
        'spec':      (const_end, spec_end),
        'labels':    (spec_end, label_end),
        'fields':    (label_end, field_end),
    }

def hex_dump_diff(a, b, offset=0, context=2, max_diffs=30):
    """Show differing bytes between two byte sequences."""
    diffs = []
    max_len = max(len(a), len(b))
    for i in range(max_len):
        av = a[i] if i < len(a) else None
        bv = b[i] if i < len(b) else None
        if av != bv:
            diffs.append(i)

    if not diffs:
        return []

    lines = []
    shown = set()
    count = 0
    for d in diffs:
        if count >= max_diffs:
            lines.append(f"  ... and {len(diffs) - count} more differences")
            break
        start = max(0, d - context)
        end = min(max_len, d + context + 1)
        for i in range(start, end):
            if i in shown:
                continue
            shown.add(i)
            av = f"{a[i]:02X}" if i < len(a) else "--"
            bv = f"{b[i]:02X}" if i < len(b) else "--"
            ac = chr(a[i]) if i < len(a) and 32 <= a[i] < 127 else '.'
            bc = chr(b[i]) if i < len(b) and 32 <= b[i] < 127 else '.'
            marker = " <<" if av != bv else ""
            lines.append(f"  {offset+i:06X}  orig={av} ({ac})  comp={bv} ({bc}){marker}")
        count += 1
        if count < len(diffs) and count < max_diffs:
            lines.append("")
    return lines

def dump_instructions(data, h, label=""):
    """Dump instruction opcodes from code segment."""
    buf_off = 128 + 1600
    code = data[buf_off:buf_off + h['code_size']]
    instr_size = 7 if h['pro_type'].startswith('TAS32') else 8
    instrs = []
    for i in range(0, len(code), instr_size):
        if i + instr_size > len(code):
            break
        opcode = struct.unpack_from('<H', code, i)[0]
        if instr_size == 7:
            spec_size = code[i+2]
            spec_ptr = struct.unpack_from('<i', code, i+3)[0]
        else:
            spec_size = code[i+3]
            spec_ptr = struct.unpack_from('<i', code, i+4)[0]
        instrs.append((i, opcode, spec_size, spec_ptr))
    return instrs

def main():
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <original.RUN> <compiled.RUN>")
        sys.exit(1)

    orig_path, comp_path = sys.argv[1], sys.argv[2]
    orig = open(orig_path, 'rb').read()
    comp = open(comp_path, 'rb').read()

    ho = read_header(orig)
    hc = read_header(comp)

    print(f"=== RUN FILE DIFF: {os.path.basename(orig_path)} ===")
    print(f"Original: {len(orig)} bytes    Compiled: {len(comp)} bytes    Delta: {len(comp)-len(orig):+d}")
    print()

    # Header comparison
    print("--- HEADER ---")
    for key in ho:
        vo, vc = ho[key], hc[key]
        marker = " <<< DIFF" if vo != vc else ""
        print(f"  {key:20s}  orig={str(vo):>10s}  comp={str(vc):>10s}{marker}")
    print()

    # Section size comparison
    so = get_sections(orig, ho)
    sc = get_sections(comp, hc)
    print("--- SECTION SIZES ---")
    for name in so:
        os_start, os_end = so[name]
        cs_start, cs_end = sc[name]
        o_sz = os_end - os_start
        c_sz = cs_end - cs_start
        marker = " <<< DIFF" if o_sz != c_sz else ""
        print(f"  {name:12s}  orig={o_sz:>8d}  comp={c_sz:>8d}  delta={c_sz-o_sz:+d}{marker}")
    print()

    # Section-by-section byte diff
    for name in ['header', 'buffers', 'code', 'constants', 'spec', 'labels', 'fields']:
        os_start, os_end = so[name]
        cs_start, cs_end = sc[name]
        o_data = orig[os_start:os_end]
        c_data = comp[cs_start:cs_end]
        if o_data == c_data:
            print(f"--- {name.upper()} --- ✓ identical")
        else:
            ndiffs = sum(1 for i in range(max(len(o_data), len(c_data)))
                        if (o_data[i] if i < len(o_data) else None) != (c_data[i] if i < len(c_data) else None))
            print(f"--- {name.upper()} --- ✗ {ndiffs} byte differences")
            lines = hex_dump_diff(o_data, c_data, offset=0, max_diffs=15)
            for l in lines:
                print(l)
        print()

    # Instruction comparison
    oi = dump_instructions(orig, ho, "orig")
    ci = dump_instructions(comp, hc, "comp")
    print(f"--- INSTRUCTIONS --- orig={len(oi)} comp={len(ci)}")
    max_instr = max(len(oi), len(ci))
    mismatches = 0
    for idx in range(max_instr):
        if idx < len(oi) and idx < len(ci):
            o = oi[idx]
            c = ci[idx]
            if o[1:] != c[1:]:
                if mismatches < 20:
                    print(f"  [{idx:3d}] orig=op:{o[1]:04X} sz:{o[2]:2d} ptr:{o[3]:5d}  "
                          f"comp=op:{c[1]:04X} sz:{c[2]:2d} ptr:{c[3]:5d}  <<<")
                mismatches += 1
        elif idx < len(oi):
            if mismatches < 20:
                print(f"  [{idx:3d}] orig=op:{oi[idx][1]:04X}  comp=MISSING  <<<")
            mismatches += 1
        else:
            if mismatches < 20:
                print(f"  [{idx:3d}] orig=MISSING  comp=op:{ci[idx][1]:04X}  <<<")
            mismatches += 1
    if mismatches == 0:
        print("  All instructions match ✓")
    elif mismatches > 20:
        print(f"  ... {mismatches} total mismatches")
    print()

if __name__ == '__main__':
    main()
