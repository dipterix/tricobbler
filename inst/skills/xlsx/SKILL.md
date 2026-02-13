---
name: xlsx
description: "Use this skill for any task involving Microsoft Excel files (.xlsx). Capabilities include reading data from spreadsheets, creating new workbooks, editing existing files, formatting cells, and writing formulas. This skill uses the R package `openxlsx`, which requires no Java dependency. Trigger especially when the user mentions file name that contains extension '.xlsx'. Do NOT trigger then the primary object is a document, plain text file (.txt, .csv, .tsv), HTML, script, database, workflow."
---

# Excel File Manipulation (R/openxlsx)

## Overview
This skill provides capabilities to interact with `.xlsx` files using the `openxlsx` R package. It allows for reading data into R dataframes, writing dataframes to Excel sheets, applying styling (fonts, colors, borders), and inserting Excel formulas.

## CRITICAL: Never Stop to Ask Questions

**When given a multi-step workflow, execute ALL steps without pausing to ask clarifying questions.** Use reasonable defaults for anything unspecified (colors, fonts, formatting details) and proceed to the next step. If a user asks you to do 10 steps, you must complete all 10 — do NOT stop at step 8 to ask a question. You may include these questions as "further steps" and append at the end of the tasks as a summary text.

## Core Guidelines

1.  **No Java Required**: This skill uses `openxlsx`, which is stable and does not require a Java runtime environment.
2.  **1-Based Indexing**: R and `openxlsx` use 1-based indexing for rows and columns (Row 1, Column 1 is A1).
3.  **Formulas are Strings**: When writing formulas from R, they are stored as text strings. They will not display a calculated value until the file is opened in Excel.
    *   *Do not* attempt to read back a cell immediately after writing a formula to verify the result; it will be empty or contain the formula string.
    *   Use `xlsx_eval.R` to compute formula values in R after writing them.
4.  **Dates**: Excel stores dates as serial numbers. Always use `detectDates = TRUE` when reading to convert them to R Date objects.
5.  **Professional Formatting**: Unless specified otherwise, apply clean, professional formatting (e.g., bold headers, standard fonts like Arial or Calibri, appropriate column widths).

## Requirements & Format

- **Zero Formula Errors**: When using formula, zero errors are tolerated
- **Years**: Format as text strings (e.g., "2026" not "2,026")
- **Currency**: Use `$##,##0` format and specify units in headers ("GDP ($mm)")
- **Zeros**: Use number formatting to make all zeros, including percentages (e.g., "$##,##0;($##,##0);-")
- **Percentages**: Use one decimal 0.0% format by default
- **Multiples**: Format as 0.0x for valuation multiples (ratio, multiplier)
- **Negative numbers**: Use parentheses (123) not minus -123
- Use cell references in formulas: do NOT hardcode values , for example: Use `=A1*(1+$A$10)` instead of `=A5*3.15`
- Always check for off-by-one errors in ranges when using formula
- Comment the sources/references with format: "Source: [System/Document/Title], [Date], [Specific Reference], [URL if applicable]"
- When creating table, the units should reflect in the table headers

## Common Operations

### 1. Reading Excel Files

Use `read.xlsx` to load data.

```r
library(openxlsx)

# standard read
df <- read.xlsx("data.xlsx", sheet = 1)

# robust read with date detection and specific start row
df <- read.xlsx(
  xlsxFile = "input.xlsx",
  sheet = "Sheet1",
  startRow = 1,
  detectDates = TRUE,  # Critical for date columns
  na.strings = c("NA", "")
)
```

### 2. Creating and Writing Files

For simple tasks, `write.xlsx` is sufficient. For complex reports with multiple sheets or styling, use the Workbook object workflow.

**Simple:**
```r
write.xlsx(iris, "output.xlsx", asTable = TRUE)
```

**Advanced (Workbook Object):**
```r
wb <- createWorkbook()
addWorksheet(wb, "Summary")

# Write data
writeData(wb, "Summary", x = df, startCol = 1, startRow = 1)

# Save
saveWorkbook(wb, "report.xlsx", overwrite = TRUE)
```

### 3. Styling and Formatting

Styling requires creating a style object and then applying it to a grid of cells.

```r
# Create a style
header_style <- createStyle(
    fontSize = 12,
    fontColour = "#FFFFFF",
    fgFill = "#4F81BD",
    textDecoration = "bold",
    halign = "center"
)

# Apply style to specific cells (e.g., Row 1, Cols 1-5)
addStyle(wb, "Summary", style = header_style, rows = 1, cols = 1:5, gridExpand = TRUE)

# Set column widths
setColWidths(wb, "Summary", cols = 1:5, widths = "auto")
```

### 4. Writing Formulas

You can write Excel formulas into cells. Remember these are evaluated by Excel, not R.

```r
# Write a sum formula to cell B10
writeFormula(wb, "Summary", x = "SUM(B2:B9)", startCol = 2, startRow = 10)
```

## Tool-Calling Guide

The tool is called `skill-xlsx`. It accepts a single JSON `object` parameter with the following structure:

### Actions

1.  **`"readme"`** -- returns this SKILL.md body. **Call this first.**
    ```json
    {"action": "readme"}
    ```

2.  **`"reference"`** -- read a reference file (e.g. `reference.md`).  
    ```json
    {"action": "reference", "reference_kwargs": {"file_name": "reference.md"}}
    ```

3.  **`"script"`** -- execute one of the available R scripts.  
    ```json
    {
      "action": "script",
      "cli_kwargs": {
        "file_name": "xlsx_create.R",
        "args": ["--file=/path/to/file.xlsx", "--data_json=[{\"Item\":\"Apples\",\"Price\":3}]", "--formulas_json={\"B5\":\"SUM(B2:B4)\"}"]
      }
    }
    ```
    Use `args=["--help"]` to see usage for any script.

### Script Examples

**Update a cell value:**
```json
{
  "action": "script",
  "cli_kwargs": {
    "file_name": "xlsx_update.R",
    "args": ["--file=/path/to/file.xlsx", "--sheet=Sheet1", "--updates_json=[{\"cell\":\"B2\",\"value\":10}]"]
  }
}
```

**Apply a background color (style) to a cell:**
```json
{
  "action": "script",
  "cli_kwargs": {
    "file_name": "xlsx_update.R",
    "args": ["--file=/path/to/file.xlsx", "--sheet=Sheet1", "--updates_json=[{\"cell\":\"B5\",\"fgFill\":\"#00FF00\"}]"]
  }
}
```

**Update value AND style in one call:**
```json
{
  "action": "script",
  "cli_kwargs": {
    "file_name": "xlsx_update.R",
    "args": ["--file=/path/to/file.xlsx", "--updates_json=[{\"cell\":\"B2\",\"value\":10},{\"cell\":\"B5\",\"fgFill\":\"#00FF00\"}]"]
  }
}
```

**Read with styles:**
```json
{
  "action": "script",
  "cli_kwargs": {
    "file_name": "xlsx_read.R",
    "args": ["--file=/path/to/file.xlsx", "--include_styles=true"]
  }
}
```

### Available Scripts

| Script file | Purpose |
|-------------|---------|  
| `xlsx_create.R` | Create a new workbook with data and optional formulas |
| `xlsx_read.R`   | Read and display cell contents from a workbook |
| `xlsx_update.R` | Update specific cells (values, formulas, or styles) |
| `xlsx_eval.R`   | Evaluate formulas in R (SUM, AVERAGE, IF, ROUND, etc.) |
| `xlsx_recalc.R` | Recalculate via LibreOffice and scan for errors |

### Key Points

*   Script arguments are passed as `cli_kwargs.args`, an **array of strings** in `--key=value` format.
*   `data_json`, `formulas_json`, `updates_json` must be **stringified JSON** inside args (escaped double quotes).
*   **Styling is done via `--updates_json`** -- there is NO `--style_json` parameter. Use **flat style keys** directly in the update object: `[{"cell":"B5","fgFill":"#00FF00"}]`. Supported keys: `fgFill`, `fontColour`, `bold`, `fontSize`, `numFmt`, `fontName`, `halign`, `valign`, `wrapText`, `border`, `borderColour`, `borderStyle`.
*   The `cells` parameter for `xlsx_eval.R` is a **comma-separated string** (e.g. `"--cells=B5,B6"`), NOT a JSON array.
*   `writeFormula` omits the leading `=`. Use `"SUM(B2:B5)"` not `"=SUM(B2:B5)"`.
*   Formula cells appear as `NA` when read back. Use `xlsx_eval.R` to get computed values.

## Best Practices for Agents

*   **Never ask clarifying questions**: Use sensible defaults for colors (`#00FE00` for green, `#FF0000` for red), fonts (Calibri or Arial), and other styling.
*   **Defaults**: If the user does not specify colors or fonts, choose standard professional defaults (e.g., white text on blue background for headers).
*   **Complete all steps**: When given a multi-step task, finish every step before responding.
*   **Validation**: Check if the file exists before reading.
*   **Sheet Names**: Verify sheet names exist when reading specific sheets.
*   **Output cleanup**: If your script generates intermediate files, clean them up or clarify which is the final deliverable.

## Troubleshooting

*   **"File is open" error**: Ensure the destination .xlsx file is not currently open in Microsoft Excel, as this locks the file and prevents R from saving.
*   **Date integers**: If you see numbers like `44500` instead of dates, you forgot `detectDates = TRUE` in `read.xlsx`.


## How to use References

The `reference.md` file provides more details. Due to the length, here I only paste its table-of-contents so you can quickly find the sections you need.

*Table of contents from `reference.md`*

1. [Agent Checklist](#agent-checklist)
2. [Best Practices](#best-practices)
3. [Common Operations](#common-operations)
4. [Available Scripts](#available-scripts)
5. [Script Details](#script-details)
6. [openxlsx API Reference](#openxlsx-api-reference)
7. [Formula Verification Checklist](#formula-verification-checklist)
8. [Appendix: Output Requirements (Financial Models)](#appendix-output-requirements-financial-models)

*Agent Checklist* 

Before and during every xlsx task, confirm:

- [ ] Use **Excel formulas** (`writeFormula`) instead of hardcoding R-computed values
- [ ] Use **`openxlsx`** (no Java dependency) -- never import Python libraries
- [ ] Formulas omit the leading `=` (e.g. `"SUM(B2:B5)"`)
- [ ] All colours include the `#` prefix (e.g. `"#00FE00"`)
- [ ] All cell/row indices are **1-based**
- [ ] Style JSON is **flat** (place `fgFill`, `bold`, etc. directly alongside `cell`; no nesting)
- [ ] JSON strings use **double quotes** only; `data_json` is a *stringified* JSON string
- [ ] After writing formulas, run **`xlsx_eval.R`** (or `xlsx_recalc.R`) before delivering
- [ ] **Never stop** to ask about colours, fonts, or minor styling -- use defaults
- [ ] Check file exists before reading; verify sheet names before targeting a sheet
- [ ] Use `detectDates = TRUE` when reading date columns
- [ ] Use `stack = TRUE` in `addStyle()` to layer styles without overwriting

