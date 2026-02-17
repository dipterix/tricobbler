# Import-BIDS Parameters Reference

## Required Parameters

### `bids_project_path`

**Type**: String (file path)  
**Description**: Absolute path to the root directory of the BIDS dataset  
**Requirements**:
- Must be an absolute path (not relative)
- Must contain `datasets_description.json` file
- Must have proper BIDS structure with subjects

**Examples**:
```bash
Rscript scripts/import-bids.R \
  --bids-project-path /home/user/rave_data/bids_dir/ds005953 \
  --bids-subject-code 01
```

**Error troubleshooting**:
- If error "datasets_description.json not found": Verify the path points to BIDS root, not a subject folder
- If error "Path does not exist": Check absolute path spelling and disk availability

---

### `bids_subject_code`

**Type**: String  
**Description**: The subject identifier code in the BIDS dataset  
**Format**: Can be specified with or without the `sub-` prefix
  - With prefix: `01` or `sub-01` (both work)
  - The script automatically handles the prefix

**Examples**:
```bash
# Both of these are equivalent:
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids_data \
  --bids-subject-code 01

Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids_data \
  --bids-subject-code sub-01
```

**Error troubleshooting**:
- If error "Subject not found in BIDS project": Verify folder `sub-{code}` exists in BIDS root
- Check spelling matches BIDS naming exactly

---

## Optional Parameters

### `--rave-project-name`

**Type**: String  
**Default**: The basename of `bids_project_path` directory  
**Description**: The project name in RAVE that will contain the imported subject  
**Notes**:
- Must follow RAVE naming conventions (alphanumeric, underscores, no spaces or dashes)
- RAVE stores projects in `$RAVE_DATA/projects/{project_name}/`
- If project doesn't exist, it will be created automatically

**Examples**:
```bash
# Default behavior (project name = "ds005953")
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/ds005953 \
  --bids-subject-code 01

# Custom project name
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/ds005953 \
  --bids-subject-code 01 \
  --rave-project-name NSD_iEEG

# Same dataset imported into multiple projects
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/ds005953 \
  --bids-subject-code 01 \
  --rave-project-name pilot_analysis

Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/ds005953 \
  --bids-subject-code 01 \
  --rave-project-name final_analysis
```

---

### `--rave-subject-prefix`

**Type**: String  
**Default**: `{rave_project_name}_` (project name followed by underscore)  
**Description**: Prefix added to the subject code in RAVE to create the full subject identifier  
**Notes**:
- RAVE subject codes cannot contain dashes (BIDS uses `sub-01`, RAVE uses `ds005953_01`)
- Prefix is prepended to the numeric/alphanumeric subject code
- Useful for batch imports to maintain subject code conventions
- If specified, completely replaces the default prefix

**Examples**:
```bash
# Default: subject "01" becomes "ds005953_01"
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/ds005953 \
  --bids-subject-code 01 \
  --rave-project-name ds005953

# Custom prefix: subject "01" becomes "NSD_01"
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/ds005953 \
  --bids-subject-code 01 \
  --rave-project-name ds005953 \
  --rave-subject-prefix NSD_

# No prefix (empty string): subject "01" becomes "01"
# (not recommended unless you manage uniqueness manually)
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/ds005953 \
  --bids-subject-code 01 \
  --rave-subject-prefix ""

# Batch import with consistent naming
for subj in 01 02 03 04; do
  Rscript scripts/import-bids.R \
    --bids-project-path /data/bids/multi_subject \
    --bids-subject-code $subj \
    --rave-project-name myproject \
    --rave-subject-prefix patient_
done
# Results: patient_01, patient_02, patient_03, patient_04
```

---

### `--freesurfer-name`

**Type**: String  
**Default**: `freesurfer`  
**Description**: Name of the FreeSurfer (or surface derivatives) folder under the BIDS `derivatives/` directory  
**Usage**: Allows flexibility for non-standard surface folder naming or custom surface derivatives  
**Notes**:
- Looks for folder at: `{bids_project_path}/derivatives/{freesurfer_name}/sub-{subject}/`
- Supports both standard FreeSurfer directory structure and GIFTI surface format
- When GIFTI surfaces found, script automatically converts `pial.l.surf.gii` and `pial.r.surf.gii` to FreeSurfer format
- If folder doesn't exist, warning is issued but import continues (surfaces can be added later)

**Examples**:
```bash
# Standard FreeSurfer (default)
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids \
  --bids-subject-code 01
# Looks for: ~/my_bids/derivatives/freesurfer/sub-01/

# Hermes-format GIFTI surfaces
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids \
  --bids-subject-code 01 \
  --freesurfer-name surface
# Looks for: ~/my_bids/derivatives/surface/sub-01/

# FreeSurfer with version in folder name
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids \
  --bids-subject-code 01 \
  --freesurfer-name freesurfer-7.4.1
# Looks for: ~/my_bids/derivatives/freesurfer-7.4.1/sub-01/

# Custom anatomy folder name
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids \
  --bids-subject-code 01 \
  --freesurfer-name anatomy
# Looks for: ~/my_bids/derivatives/anatomy/sub-01/
```

**Directory structure examples**:

Standard FreeSurfer layout:
```
derivatives/freesurfer/sub-01/
├─ mri/
│  ├─ T1.mgz
│  ├─ wm.mgz
│  ├─ aparc+aseg.mgz
│  └─ wmparc.mgz
├─ surf/
│  ├─ lh.pial
│  ├─ lh.white
│  ├─ lh.inflated
│  ├─ rh.pial
│  ├─ rh.white
│  └─ rh.inflated
└─ label/
   ├─ lh.aparc.annot
   └─ rh.aparc.annot
```

Hermes GIFTI format:
```
derivatives/surface/sub-01/
├─ pial.l.surf.gii         <- Converted to lh.pial
├─ pial.r.surf.gii         <- Converted to rh.pial
└─ (other files)
```

---

### `--override-freesurfer`

**Type**: Boolean flag (no value needed)  
**Default**: `FALSE` (off)  
**Description**: Force reimport of FreeSurfer data even if subject already exists in RAVE with valid surfaces  
**When to use**:
- After correcting FreeSurfer output and want to reimport
- When updating to a newer FreeSurfer version
- When surfaces were incomplete and have been replaced
- When you want to refresh coordinates or labels

**Examples**:
```bash
# Initial import
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids \
  --bids-subject-code 01
# Imports FreeSurfer if not already present in RAVE

# Later: after correcting FreeSurfer outputs
# Re-run with override flag
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids \
  --bids-subject-code 01 \
  --override-freesurfer
# Backs up old surfaces and reimports new ones

# Combined with other parameters
Rscript scripts/import-bids.R \
  --bids-project-path ~/my_bids \
  --bids-subject-code 01 \
  --rave-project-name myproject \
  --freesurfer-name freesurfer-7.4.1 \
  --override-freesurfer
```

**Behavior details**:
- When enabled: Backs up existing RAVE surfaces (renamed to `.bak`) and copies new ones
- When disabled (default): Checks if subject already has valid surfaces; if yes, skips import
- Script validates existing surfaces using `threeBrain` before deciding to skip
- If surfaces are invalid/incomplete, reimport proceeds automatically regardless of flag

---

## Complete Usage Examples

### Example 1: Simple OpenNeuro Import

```bash
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/ds005953 \
  --bids-subject-code 01
```

**Result**: Project `ds005953` created with subject `ds005953_01`

---

### Example 2: Custom Project with Multiple Subjects

```bash
for subj in 01 02 03 04 05; do
  Rscript scripts/import-bids.R \
    --bids-project-path /data/bids/my_study \
    --bids-subject-code $subj \
    --rave-project-name my_analysis \
    --rave-subject-prefix patient_
done
```

**Result**: Project `my_analysis` with subjects `patient_01` through `patient_05`

---

### Example 3: Non-Standard Surfaces with Reimport

```bash
# Initial import with Hermes GIFTI surfaces
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/hermes_data \
  --bids-subject-code 07 \
  --rave-project-name hermes_ieeg \
  --freesurfer-name surface

# Later, after correcting surface coordinates
Rscript scripts/import-bids.R \
  --bids-project-path /data/bids/hermes_data \
  --bids-subject-code 07 \
  --rave-project-name hermes_ieeg \
  --freesurfer-name surface \
  --override-freesurfer
```

---

## Default Behavior Table

| Scenario | Behavior |
|----------|----------|
| Missing `--rave-project-name` | Uses BIDS folder basename (e.g., `ds005953`) |
| Missing `--rave-subject-prefix` | Uses `{project_name}_` (e.g., `ds005953_`) |
| Missing `--freesurfer-name` | Looks for `derivatives/freesurfer/` folder |
| `--override-freesurfer` not set | Checks existing surfaces; only imports if missing/invalid |
| Surfaces folder doesn't exist | Warns but continues import (warning logged) |
| Subject already in RAVE | Continues; skips FreeSurfer if valid unless `--override-freesurfer` set |


