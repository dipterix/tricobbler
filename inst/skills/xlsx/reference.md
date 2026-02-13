# XLSX Skill Reference (openxlsx / R)

> Quick-reference for agents working with `.xlsx` files via the `openxlsx` R
> package. All operations are R-only; no Python or Java required. LibreOffice
> is optional for full formula recalculation.

---

## Table of Contents

1. [Agent Checklist](#agent-checklist)
2. [Best Practices](#best-practices)
3. [Common Operations](#common-operations)
4. [Available Scripts](#available-scripts)
5. [Script Details](#script-details)
6. [openxlsx API Reference](#openxlsx-api-reference)
7. [Formula Verification Checklist](#formula-verification-checklist)
8. [Appendix: Output Requirements (Financial Models)](#appendix-output-requirements-financial-models)

---

## Agent Checklist

Before and during every xlsx task, confirm:

- [ ] Use **Excel formulas** (`writeFormula`) instead of hardcoding R-computed values
- [ ] Use **`openxlsx`** (no Java dependency) -- never import Python libraries
- [ ] Formulas omit the leading `=` (e.g. `"SUM(B2:B5)"`)
- [ ] All colours include the `#` prefix (e.g. `"#00FE00"`)
- [ ] All cell/row indices are **1-based**
- [ ] Style JSON is **flat** (keys map to `createStyle()` params; no nesting)
- [ ] JSON strings use **double quotes** only; `data_json` is a *stringified* JSON string
- [ ] After writing formulas, run **`xlsx_eval.R`** (or `xlsx_recalc.R`) before delivering
- [ ] **Never stop** to ask about colours, fonts, or minor styling -- use defaults
- [ ] Check file exists before reading; verify sheet names before targeting a sheet
- [ ] Use `detectDates = TRUE` when reading date columns
- [ ] Use `stack = TRUE` in `addStyle()` to layer styles without overwriting

---

## Best Practices

### Use Formulas, Not Hardcoded Values

Always let Excel do the calculation so the spreadsheet stays dynamic.

**Wrong** -- hardcoding in R:
```r
total <- sum(df$Sales)
writeData(wb, "Sheet1", total, startCol = 2, startRow = 10)
```

**Right** -- Excel formula:
```r
writeFormula(wb, "Sheet1", "SUM(B2:B9)", startCol = 2, startRow = 10)
```

This applies to **all** calculations: totals, percentages, ratios, averages, etc.

### Use Sensible Defaults

Never interrupt a workflow for trivial questions. If the user says "green", use
`#00FE00`. Refer to the [hex colour table](#common-hex-colours) for defaults.

### JSON Argument Formatting

Script arguments (`data_json`, `formulas_json`, `updates_json`) must be
**stringified JSON** with double quotes.

**Wrong:**
```json
{"data_json": "[{'Item': 'Apples'}]"}
```

**Right:**
```json
{"data_json": "[{\"Item\":\"Apples\",\"Price\":3}]"}
```

`data_json` must be an array of row-objects (column names as keys), never
arrays-of-arrays.

### Prefer RANDBETWEEN for Random Integers

Use `RANDBETWEEN(1,5)` instead of `RAND()*4+1`. Cleaner, produces actual
integers, and is fully supported by `xlsx_eval`.

---

## Common Operations

### Reading Data

```r
library(openxlsx)

df <- read.xlsx("file.xlsx", sheet = 1, detectDates = TRUE)
head(df)
summary(df)

# Load workbook object (preserves formulas/styles)
wb <- loadWorkbook("file.xlsx")
readWorkbook(wb, sheet = 1, rows = 1:5, cols = 1:3)
```

### Creating a New Workbook

```r
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", data.frame(Item = c("A", "B"), Value = c(10, 20)))

# Formula (no leading '=')
writeFormula(wb, "Sheet1", x = "SUM(B2:B3)", startCol = 2, startRow = 4)

# Style
bold <- createStyle(textDecoration = "bold")
addStyle(wb, "Sheet1", style = bold, rows = 1, cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Sheet1", cols = 1:2, widths = c(20, 15))

saveWorkbook(wb, "output.xlsx", overwrite = TRUE)
```

### Editing an Existing Workbook

```r
wb <- loadWorkbook("existing.xlsx")
writeData(wb, "Sheet1", "New Value", startCol = 1, startRow = 1)
writeFormula(wb, "Sheet1", x = "SUM(B2:B10)", startCol = 2, startRow = 11)
addWorksheet(wb, "NewSheet")
writeData(wb, "NewSheet", data.frame(x = 1:5))
saveWorkbook(wb, "modified.xlsx", overwrite = TRUE)
```

### Styles and Formatting

```r
# Combined header style
header_style <- createStyle(
  textDecoration = "bold", fgFill = "#4472C4",
  fontColour = "#FFFFFF", halign = "center",
  fontSize = 12, fontName = "Arial"
)
addStyle(wb, "Sheet1", style = header_style, rows = 1, cols = 1:5,
         gridExpand = TRUE, stack = TRUE)

# Number format
currency <- createStyle(numFmt = "$#,##0.00")
addStyle(wb, "Sheet1", style = currency, rows = 2:10, cols = 2, stack = TRUE)

# Column widths, freeze panes, merge
setColWidths(wb, "Sheet1", cols = 1:3, widths = c(20, 15, 15))
freezePane(wb, "Sheet1", firstRow = TRUE, firstCol = TRUE)
mergeCells(wb, "Sheet1", cols = 1:3, rows = 1)
```

### Cell Reference Utilities

```r
# Column letter <-> number
match("C", LETTERS)   # 3
LETTERS[3]            # "C"

# Columns beyond Z
col_to_letter <- function(col) {
  result <- ""
  while (col > 0) {
    col <- col - 1
    result <- paste0(LETTERS[col %% 26 + 1], result)
    col <- col %/% 26
  }
  result
}
# col_to_letter(27) => "AA"
```

---

## Available Scripts

| Script | Purpose |
|--------|---------|
| `xlsx_create.R` | Create a new workbook with data and optional formulas |
| `xlsx_read.R`   | Read and display cell contents from a workbook |
| `xlsx_update.R` | Update specific cells (values, formulas, or styles) in an existing workbook |
| `xlsx_eval.R`   | Evaluate formulas in R (SUM, AVERAGE, IF, ROUND, RANDBETWEEN, arithmetic, etc.) |
| `xlsx_recalc.R` | Recalculate all formulas via LibreOffice (if installed) and scan for errors |

### Recommended Workflow

1. **Create or load** -- `xlsx_create.R` for new files, `xlsx_update.R` for edits
2. **Add formulas and formatting** -- `formulas_json` in create; `formula`/`style` in update
3. **Evaluate formulas** -- `xlsx_eval.R` for R-native, or `xlsx_recalc.R` for full recalculation
4. **Verify** -- `xlsx_read.R` with `include_styles=true` to inspect values and styles

---

## Script Details

### xlsx_create.R

Creates a new workbook with tabular data and optional formulas.

| Parameter | Required | Description |
|-----------|----------|-------------|
| `file` | yes | Output file path |
| `data_json` | yes | Stringified JSON array of row-objects |
| `sheet` | no | Sheet name (default `"Sheet1"`) |
| `formulas_json` | no | Dict `{"B5":"SUM(B2:B4)"}` or array `[{"cell":"B5","formula":"SUM(B2:B4)"}]` |
| `header` | no | `"true"` / `"false"` (default `"true"`) |

Values starting with `=` in `data_json` are auto-written as formulas. Leading `=` in `formulas_json` is stripped.

**Example:**
```json
{
  "file": "/tmp/demo.xlsx",
  "data_json": "[{\"Item\":\"Apples\",\"Price\":3},{\"Item\":\"Bananas\",\"Price\":2}]",
  "formulas_json": "{\"B4\":\"SUM(B2:B3)\"}",
  "header": "true"
}
```

### xlsx_read.R

Reads and displays cell contents from an existing workbook.

| Parameter | Required | Description |
|-----------|----------|-------------|
| `file` | yes | Path to `.xlsx` file |
| `sheet` | no | Sheet name or index (default `"Sheet1"`) |
| `range` | no | A1-style range, single cell, or `"all"` (default `"all"`) |
| `include_styles` | no | `"true"` to show styling metadata (default `"false"`) |

**Note:** Formula cells appear as `=<formula>` when read back (e.g. `=SUM(B2:B4)`). Use `xlsx_eval.R` to get computed values.

### xlsx_update.R

Updates specific cells in an existing workbook -- values, formulas, and/or styles.

| Parameter | Required | Description |
|-----------|----------|-------------|
| `file` | yes | Path to `.xlsx` file |
| `updates_json` | yes | JSON array of updates or dict `{"B2":10}` |
| `sheet` | no | Sheet name (default `"Sheet1"`) |

Each update element can contain:

- `cell` (required): A1-style reference
- `value`: number, string, or logical
- `formula`: formula string (leading `=` stripped)
- `style`: **flat** object with `createStyle()` keys

**Style JSON must be flat:**

```json
{"cell":"B5","style":{"fgFill":"#00FE00","fontColour":"#FF0000"}}
```

Never nest as `{"fill":{"fgColor":...}}`.

### xlsx_eval.R

Evaluates formulas stored in a workbook using R.

| Parameter | Required | Description |
|-----------|----------|-------------|
| `file` | yes | Path to `.xlsx` file |
| `cells` | no | Comma-separated cells or `"all"` (default `"all"`) |
| `sheet` | no | Sheet name (default `"Sheet1"`) |

**Supported:** `SUM`, `AVERAGE`, `MIN`, `MAX`, `COUNT`, `PRODUCT`, `MEDIAN`,
`RANDBETWEEN`, `RAND`, `ROUND`, `ABS`, `SQRT`, `LOG`, `LN`, `LOG10`, `EXP`,
`INT`, `MOD`, `POWER`, `CEILING`, `FLOOR`, `SIGN`, `PI`, `IF`, and arithmetic
(`+`, `-`, `*`, `/`, `^`).

**Not supported:** `VLOOKUP`, `INDEX/MATCH`, `CONCATENATE`, `TEXT`, `SUMIF`,
`COUNTIF`, `IFERROR`. Use `xlsx_recalc.R` for these.

Evaluation is recursive -- if a formula references another formula cell, it is
resolved first.

**Returns:**
```json
{
  "status": "success",
  "total_formulas": 4,
  "evaluated": 1,
  "results": [{"cell":"B5","formula":"SUM(B2:B4)","value":15,"type":"formula"}]
}
```

### xlsx_recalc.R

Recalculates all formulas via LibreOffice and scans for Excel errors.

| Parameter | Required | Description |
|-----------|----------|-------------|
| `file` | yes | Path to `.xlsx` file |
| `timeout` | no | Seconds (default `"30"`) |

**Returns:**
```json
{
  "status": "success",
  "total_errors": 0,
  "total_formulas": 42,
  "error_summary": {}
}
```

If errors exist, `status` is `"errors_found"` with details:
```json
{"#REF!": {"count": 2, "locations": ["Sheet1!B5", "Sheet1!C10"]}}
```

---

## openxlsx API Reference

### Key Functions

| Function | Purpose |
|----------|---------|
| `createWorkbook()` | New empty workbook |
| `loadWorkbook(file)` | Load existing `.xlsx` |
| `addWorksheet(wb, name)` | Add a sheet |
| `writeData(wb, sheet, x, ...)` | Write values |
| `writeFormula(wb, sheet, x, ...)` | Write formula strings |
| `read.xlsx(file, sheet)` | Quick read to data.frame |
| `readWorkbook(wb, sheet, ...)` | Read with more control |
| `saveWorkbook(wb, file, overwrite)` | Save to disk |
| `addStyle(wb, sheet, style, ...)` | Apply formatting |
| `createStyle(...)` | Define a style |
| `setColWidths(wb, sheet, cols, widths)` | Set column widths |
| `freezePane(wb, sheet, ...)` | Freeze panes |
| `mergeCells(wb, sheet, cols, rows)` | Merge cells |
| `sheets(wb)` | List sheet names |

### `createStyle()` Parameters

| Parameter | Description | Example |
|-----------|-------------|---------|
| `fgFill` | Background fill (hex) | `"#FFFF00"` |
| `fontColour` | Font colour (hex) | `"#FF0000"` |
| `textDecoration` | bold / italic / underline / strikeout | `"bold"` |
| `fontSize` | Font size (pt) | `14` |
| `fontName` | Font family | `"Arial"` |
| `numFmt` | Number format string | `"$#,##0.00"` |
| `halign` | Horizontal: left, center, right | `"center"` |
| `valign` | Vertical: top, center, bottom | `"top"` |
| `wrapText` | Wrap text | `TRUE` |
| `border` | Border sides | `"TopBottomLeftRight"` |
| `borderColour` | Border colour (hex) | `"#000000"` |
| `borderStyle` | thin / medium / thick / double | `"thin"` |

### Common Hex Colours

| Colour | Hex |
|--------|-----|
| Black | `#000000` |
| White | `#FFFFFF` |
| Red | `#FF0000` |
| Green | `#00FE00` |
| Blue | `#0000FF` |
| Yellow | `#FFFF00` |
| Dark green | `#008000` |
| Orange | `#FFA500` |
| Purple | `#800080` |
| Grey | `#808080` |
| Light grey | `#D3D3D3` |

### Important Notes

- `writeFormula()` omits leading `=`: use `"SUM(B2:B5)"` not `"=SUM(B2:B5)"`
- Formula cells have no cached value until opened in Excel; use `xlsx_eval.R` in R
- `addStyle()` with `stack = TRUE` layers styles; without it, new styles replace old
- Colours **must** include the `#` prefix
- `writeData()` with `colNames = FALSE` writes values without a header row
- Cell indices are always **1-based** (A1 = row 1, col 1)

---

## Formula Verification Checklist

### Essential Verification

- [ ] **Test 2-3 sample references** before building full model
- [ ] **Column mapping**: column 2 = B, column 27 = AA
- [ ] **Row offset**: with headers, data row 1 = Excel row 2

### Common Pitfalls

- [ ] **NA handling**: check for `NA` values before writing formulas
- [ ] **Formula cells read as NA**: `openxlsx` doesn't cache -- use `xlsx_eval`
- [ ] **Division by zero**: check denominators (`#DIV/0!`)
- [ ] **Wrong references**: verify cell refs (`#REF!`)
- [ ] **Cross-sheet refs**: use `Sheet1!A1` format
- [ ] **`=` in data_json**: auto-converted to formulas by `xlsx_create.R`; `xlsx_update.R` needs explicit `"formula"` key
- [ ] **RAND() vs RANDBETWEEN()**: prefer `RANDBETWEEN(lo,hi)` for integers

### Formula Testing Strategy

- [ ] Start small: test on 2-3 cells first
- [ ] Verify all referenced cells exist
- [ ] Test edge cases: zero, negative, very large values
- [ ] Always run `xlsx_eval.R` before delivering

---
