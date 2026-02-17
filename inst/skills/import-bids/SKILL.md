---
name: import-bids
description: >
  Import BIDS neuroimaging datasets into RAVE analysis platform. Use when
  working with BIDS-formatted intracranial EEG (iEEG) data to convert
  electrode coordinates, FreeSurfer surfaces, and signal data into RAVE's
  project structure. Supports BrainVision and Matlab signal formats. Handles
  automated FreeSurfer migration, electrode coordinate transformation, and
  epoch table construction. Use this skill when you need to (1) Convert
  OpenNeuro or local BIDS datasets to RAVE format, (2) Import iEEG recordings
  with anatomical surfaces, (3) Register electrode coordinates to subject MRI
  space, (4) Construct analysis-ready RAVE projects from raw BIDS data.
---

# Import BIDS Data to RAVE

## Overview

This skill imports neuroimaging data from BIDS-formatted datasets into the RAVE analysis platform. It automates the complete workflow of:

1. Loading BIDS project metadata
2. Migrating FreeSurfer surfaces (or GIFTI-format surfaces)
3. Importing electrode coordinate tables
4. Ingesting iEEG signal data
5. Constructing epoch and channel metadata
6. Running RAVE's import pipeline

## Prerequisites

- BIDS dataset with proper directory structure containing `datasets_description.json`
- Subject's electrode coordinates in scanRAS space (typically `*_electrodes.tsv` in `ieeg/` folder)
- FreeSurfer output under `derivatives/freesurfer/` or custom surface derivatives folder
- Signal data in supported format (BrainVision .eeg files or individual Matlab .mat files per channel)
- RAVE project name and subject code determined before running import

## Quick Start

```bash
Rscript scripts/import-bids.R \
  --bids-project-path ~/rave_data/bids_dir/ds005953 \
  --bids-subject-code 01
```

This creates a new RAVE project named after the BIDS folder (`ds005953`) with subject code `ds005953_01`.

## Parameters

See [parameters.md](references/parameters.md) for detailed documentation of all parameters, defaults, and examples.

### Required Arguments

- **`bids_project_path`** - Absolute path to BIDS dataset root (must contain `datasets_description.json`)
- **`bids_subject_code`** - Subject identifier (with or without leading `sub-`)

### Optional Arguments

- **`--rave-project-name`** - RAVE project name (default: folder name of `bids_project_path`)
- **`--rave-subject-prefix`** - Subject code prefix in RAVE (default: `{project_name}_`)
- **`--freesurfer-name`** - Derivatives folder name for surfaces (default: `freesurfer`)
- **`--override-freesurfer`** - Force update FreeSurfer import if subject already imported

## Workflow

### 1. BIDS Project Validation
The script loads the BIDS project using the `bidsr` R package and validates that the specified subject exists.

### 2. RAVE Subject Creation
Creates a new RAVE subject with properly formatted code (no dashes, starts with letter) and initializes directory structure.

### 3. FreeSurfer/Surface Migration
Migrates either:
- **Standard FreeSurfer layout**: Copies `label/`, `mri/`, `surf/`, and other directories
- **GIFTI surfaces** (Hermes format): Converts `pial.l.surf.gii` and `pial.r.surf.gii` to FreeSurfer format

If subject already exists in RAVE and surfaces are valid, skips migration unless `--override-freesurfer` is set.

### 4. Electrode Coordinate Import
Reads electrode positions from BIDS tabular files (`*_electrodes.tsv`) in scanRAS space and imports them into RAVE.

### 5. Signal Data Ingestion
Imports iEEG signal data in supported format:
- **BrainVision**: Single `.eeg` file with header/marker files
- **Matlab**: Individual `.mat` files (one per channel) containing raw signal

### 6. Epoch and Channel Metadata
Constructs epoch definitions from BIDS events and creates channel metadata (labels, types, sampling rates).

### 7. RAVE Import Pipeline
Runs RAVE's native import pipeline to finalize subject in RAVE database.

## Common Workflows

### Import from OpenNeuro

Combine with the `download-openneuro` skill:

```bash
# Download dataset first
openneuro <- raveio::load_snippet("download-openneuro")
path <- openneuro("ds005953")  # Downloads to ~/rave_data/bids_dir/ds005953

# Then import
import_bids <- raveio::load_snippet("import-bids")
import_bids(
  bids_project_path = path,
  bids_subject_code = "01"
)
```

### Batch Import Multiple Subjects

```bash
# Create a shell loop
for subject in 01 02 03; do
  Rscript scripts/import-bids.R \
    --bids-project-path ~/my_bids_dataset \
    --bids-subject-code $subject
done
```

### Handle Non-Standard Surface Folder Names

If FreeSurfer derivatives use versioned folder names:

```bash
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids_dataset \
  --bids-subject-code 01 \
  --freesurfer-name freesurfer-7.4.1
```

### Override Existing Import

To reimport after data corrections:

```bash
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids_dataset \
  --bids-subject-code 01 \
  --override-freesurfer
```

## Expected Directory Structure

### Input (BIDS Dataset)

```
NSDiEEG_dataset/
├─ datasets_description.json
├─ sub-01/
│  └─ ses-ieeg01/
│     └─ ieeg/
│        ├─ sub-01_ses-ieeg01.eeg          (BrainVision format)
│        ├─ sub-01_ses-ieeg01.vhdr
│        ├─ sub-01_ses-ieeg01.vmrk
│        ├─ sub-01_ses-ieeg01_electrodes.tsv
│        └─ sub-01_ses-ieeg01_channels.tsv
├─ derivatives/
│  └─ freesurfer/
│     └─ sub-01/
│        ├─ mri/
│        ├─ surf/
│        ├─ label/
│        └─ ...
```

### Output (RAVE Project)

```
~/rave_data/projects/ds005953/
└─ sub-ds005953_01/
   ├─ rave/
   │  ├─ preprocess/
   │  ├─ cache/
   │  └─ ...
   ├─ imaging/
   │  └─ fs/                    (FreeSurfer copy)
   │     ├─ mri/
   │     ├─ surf/
   │     └─ ...
   ├─ meta/
   │  ├─ electrodes.csv
   │  ├─ time_frequency.yaml
   │  └─ ...
```

## Troubleshooting

**Problem**: Script cannot find FreeSurfer directory
- Check that `derivatives/{freesurfer_name}/sub-{subject}/` exists
- Verify folder name matches `--freesurfer-name` parameter
- Use `--freesurfer-name` to specify custom folder names

**Problem**: Electrode coordinates not imported
- Verify `*_electrodes.tsv` exists in `{bids_project}/sub-{subject}/ses-*/ieeg/`
- Check that file is valid TSV with required columns (x, y, z)
- Ensure coordinates are in scanRAS space (T1w MRI coordinates)

**Problem**: Signal data not found
- Verify signal files are in `ieeg/` folder with proper naming
- Check format: BrainVision (`.eeg`/`.vhdr`/`.vmrk`) or Matlab (`.mat`)
- Ensure all required files present (BrainVision requires header and marker files)

**Problem**: "Subject already exists" error
- Use `--override-freesurfer` to force reimport
- Or manually remove subject from RAVE: `remove.subjects(project = "projname", subjects = "subcode")`

## Parameters Quick Reference

| Parameter | Type | Default | Notes |
|-----------|------|---------|-------|
| `bids_project_path` | string | required | Absolute path to BIDS root |
| `bids_subject_code` | string | required | Subject code (with/without `sub-`) |
| `--rave-project-name` | string | folder basename | RAVE project name |
| `--rave-subject-prefix` | string | `{project}_` | Subject code prefix |
| `--freesurfer-name` | string | `freesurfer` | Derivatives folder name |
| `--override-freesurfer` | flag | false | Force FreeSurfer reimport |

See [parameters.md](references/parameters.md) for full details and examples.

