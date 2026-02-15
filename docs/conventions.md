# Conventions

## Parameter Notation

| Marker | Meaning |
|--------|---------|
| *Required* | Parameter must be provided |
| *Optional* | Parameter may be omitted |
| `f/c/e` | Accepts field, constant, or expression |
| `sac` | Special alpha constant (e.g., `#ADD_FLDS`) |

## Program Editor Paths

Menu paths like `User interface -> Messages -> Ask` show where to find each command in the TAS Program Editor.

## Platform Markers

| Marker | Meaning |
|--------|---------|
| **Windows Only** | Ignored (no error) in DOS |
| **DOS Only** | Ignored (no error) in Windows |
| *(none)* | Works in both |

## Field Name Rules

- Max 10 characters
- Must start with a letter (A–Z)
- Letters, digits, underscores only
- Case insensitive
- Cannot be a reserved word

## File Numbering

- Files numbered 1–99
- File 0 reserved for system
- `@file_number` references a file by runtime number
