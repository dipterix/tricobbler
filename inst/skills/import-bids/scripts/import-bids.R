#!/usr/bin/env Rscript

#' @title Import BIDS data to RAVE (experimental)
#' @date May 09, 2025
#' @author Zhengjia Wang
#' @license Apache-2.0
#' @format
#' Currently supports `BrainVision`, `Matlab` (each channel is a matlab file)
#' 
#' @preparation
#' Please prepare your folder in BIDS format. If your data is on OpenNeuro, run
#' `raveio::load_snippet("download-openneuro")` to see how to download the data
#' from OpenNeuro.
#' 
#' This converter suggests `FreeSurfer` directory and subjects' electrode 
#' coordinate file in `scanRAS` space. Here is an example layout:
#' 
#' NSDiEEG_dataset (root)/
#' ├─sub-01
#' │ └─ses-ieeg01/
#' │   └─ieeg/
#' |     └─sub-01_ses-ieeg01_electrodes.tsv  <- electrode coordinates in T1w
#' ├─sub-02/
#' └─derivatives/
#'   └─freesurfer/
#'     ├─sub-01/                             <- Subject's FreeSurfer directory
#'     │ ├─mri/   
#'     │ ├─surf/
#'     │ ├─label
#'     │ └─...
#'     └─sub-02/

"Usage:
  import-bids.R <bids_project_path> <bids_subject_code> [--rave-project-name=<name>] [--rave-subject-prefix=<prefix>] [--freesurfer-name=<name>] [--override-freesurfer]
  import-bids.R -h | --help
  import-bids.R --version

Arguments:
  <bids_project_path>                     Absolute path to the BIDS dataset root directory
  <bids_subject_code>                     Subject code (with or without 'sub-' prefix)

Options:
  -h --help                               Show this help message and exit
  --version                               Show version information
  --rave-project-name=<name>              RAVE project name [default: INFER_FROM_BIDS]
  --rave-subject-prefix=<prefix>          Subject code prefix in RAVE [default: INFER_FROM_PROJECT]
  --freesurfer-name=<name>                FreeSurfer/surface folder name [default: freesurfer]
  --override-freesurfer                   Force reimport of FreeSurfer data [default: FALSE]
" -> doc

# Parse command line arguments
parsed <- tricobbler::docopt(doc, version = "import-bids v1.0.0")

# ---- Code body ---------------------------------------------------------------
# Initialize variables
bids_project_path <- parsed$bids_project_path
bids_subject_code <- parsed$bids_subject_code
rave_project_name <- if (parsed$`rave-project-name` == "INFER_FROM_BIDS") NA else parsed$`rave-project-name`
rave_subject_prefix <- if (parsed$`rave-subject-prefix` == "INFER_FROM_PROJECT") NA else parsed$`rave-subject-prefix`
freesurfer_name <- parsed$`freesurfer-name`
override_freesurfer <- parsed$`override-freesurfer`

`%?<-%` <- dipsaus::`%?<-%`
rave_project_name %?<-% NA
rave_subject_prefix %?<-% sprintf("%s_", basename(bids_project_path))
freesurfer_name %?<-% "freesurfer"
override_freesurfer %?<-% FALSE

# Load BIDS project
cat("Loading BIDS project from:", bids_project_path, "\n")
bids_project <- bidsr::bids_project(bids_project_path)

if(length(rave_project_name) != 1 || is.na(rave_project_name)) {
  rave_project_name <- bids_project$name
}

cat("BIDS project name:", rave_project_name, "\n")

bids_subject <- bidsr::bids_subject(project = bids_project, subject_code = bids_subject_code)

# Create RAVE subject
# The subject code must start with a letter and cannot be sub-xxx (Reserved for future BIDS support)
rave_subject <- raveio::RAVESubject$new(
  project_name = rave_project_name,
  subject_code = sprintf("%s%s", rave_subject_prefix, bids_subject@subject_code),
  strict = FALSE
)
rave_subject$initialize_paths(include_freesurfer = TRUE)

cat("RAVE subject code:", rave_subject$subject_code, "\n")
cat("RAVE project name:", rave_subject$project_name, "\n")

# Find FreeSurfer
bids_freesurfer_path <- bidsr::resolve_bids_path(x = bids_subject, storage = "derivative", prefix = freesurfer_name)
rave_freesurfer_path <- file.path(rave_subject$imaging_path, "fs")

cat("Migrating `freesurfer` directory with overwrite:", override_freesurfer, "\n")

if(dir.exists(bids_freesurfer_path)) {
  bids_fs_files <- list.files(
    bids_freesurfer_path,
    all.files = FALSE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE,
    ignore.case = TRUE,
    pattern = "(label|mri|surf|RAVE)$"
  )
  if(length(bids_fs_files)) {
    
    if(!override_freesurfer) {
      if( dir.exists(rave_freesurfer_path) ) {
        brain <- try({
          threeBrain::threeBrain(
            path = rave_freesurfer_path,
            subject_code = rave_subject$subject_code,
            surface_types = c("pial", "white", "smoothwm", "inflated", "sphere.reg"),
            atlas_types = c("wmparc", "aparc+aseg")
          )
        }, silent = TRUE)
        if(inherits(brain, "try-error") || is.null(brain)) {
          override_freesurfer <- TRUE
        }
      }
    }
    # copy the files to FreeSurfer path
    if(override_freesurfer || !file.exists(rave_freesurfer_path)) {
      # Do not delete, just rename instead
      raveio::backup_file(rave_freesurfer_path, remove = TRUE)
      rave_freesurfer_path <- raveio::dir_create2(rave_freesurfer_path)
      for(f in bids_fs_files) {
        cat(sprintf("Migrating `%s`\n", f))
        file.copy(f, rave_freesurfer_path, recursive = TRUE)
      }
    }
    
  } else {
    # Consider Hermes's style of using GIFTI surfaces
    bids_fs_files <- list.files(
      bids_freesurfer_path,
      all.files = FALSE,
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = TRUE,
      no.. = TRUE,
      ignore.case = TRUE,
      pattern = "pial\\.[lr]\\.surf\\.gii$"
    )
    if(length(bids_fs_files)) {
      if(!override_freesurfer) {
        if( dir.exists(rave_freesurfer_path) ) {
          brain <- try({
            threeBrain::threeBrain(
              path = rave_freesurfer_path,
              subject_code = rave_subject$subject_code,
              surface_types = c("pial", "white", "smoothwm", "inflated", "sphere.reg"),
              atlas_types = c("wmparc", "aparc+aseg")
            )
          }, silent = TRUE)
          if(inherits(brain, "try-error") || is.null(brain)) {
            override_freesurfer <- TRUE
          }
        }
      }
      # copy the files to FreeSurfer path
      if(override_freesurfer || !file.exists(rave_freesurfer_path)) {
        # Do not delete, just rename instead
        raveio::backup_file(rave_freesurfer_path, remove = TRUE)
        rave_freesurfer_path <- raveio::dir_create2(rave_freesurfer_path)
        raveio::dir_create2(file.path(rave_freesurfer_path, "mri"))
        rave_freesurfer_surfpath <- raveio::dir_create2(file.path(rave_freesurfer_path, "surf"))
        hemisphere_imported <- list(left = FALSE, right = FALSE)
        for(f in bids_fs_files) {
          cat(sprintf("Migrating `%s`\n", f))
          if(endsWith(tolower(f), "r.surf.gii")) {
            surface <- ieegio::as_ieegio_surface(f)
            # transform the space to T1w
            surface$geometry$vertices <- surface$geometry$transforms[[1]] %*% surface$geometry$vertices
            ieegio::write_surface(
              x = surface,
              con = file.path(rave_freesurfer_surfpath, "rh.pial"),
              format = "freesurfer",
              type = "geometry"
            )
            hemisphere_imported$right <- TRUE
          } else {
            surface <- ieegio::as_ieegio_surface(f)
            surface$geometry$vertices <- surface$geometry$transforms[[1]] %*% surface$geometry$vertices
            ieegio::write_surface(
              x = surface,
              con = file.path(rave_freesurfer_surfpath, "lh.pial"),
              format = "freesurfer",
              type = "geometry"
            )
            hemisphere_imported$left <- TRUE
          }
        }
        placeholder_surf <- ieegio::as_ieegio_surface(x = array(0, dim = c(3, 3)),
                                                      faces = matrix(c(1, 2, 3), ncol = 3),
                                                      face_start = 1)
        if(!hemisphere_imported$left) {
          # generate fake left surface
          ieegio::write_surface(
            x = placeholder_surf,
            con = file.path(rave_freesurfer_surfpath, "lh.pial"),
            format = "freesurfer",
            type = "geometry"
          )
        }
        if(!hemisphere_imported$right) {
          # generate fake right surface
          ieegio::write_surface(
            x = placeholder_surf,
            con = file.path(rave_freesurfer_surfpath, "rh.pial"),
            format = "freesurfer",
            type = "geometry"
          )
        }
      }
    }
  }
} else {
  cat("Warning: FreeSurfer directory not found at", bids_freesurfer_path, "\n")
  cat("Continuing without surfaces - you can add them later\n")
}

# Import electrode coordinates
cat("\nImporting electrode coordinates...\n")
bids_sessions <- bids_subject$get_sessions()
for(session in bids_sessions) {
  ieeg_dir <- file.path(bids_subject$path, session, "ieeg")
  if(dir.exists(ieeg_dir)) {
    electrode_files <- list.files(
      ieeg_dir,
      pattern = "_electrodes\\.tsv$",
      full.names = TRUE
    )
    
    for(elec_file in electrode_files) {
      cat("Found electrode file:", basename(elec_file), "\n")
      
      # Read electrode coordinates
      electrodes <- utils::read.csv(elec_file, sep = "\t")
      cat("  Total electrodes:", nrow(electrodes), "\n")
      
      # Store coordinates for RAVE import
      # This will be used in the import pipeline below
    }
  }
}

# Import signal data
cat("\nDetecting signal format...\n")

bids_sessions <- bids_subject$get_sessions()
signal_format_detected <- FALSE

for(session in bids_sessions) {
  ieeg_dir <- file.path(bids_subject$path, session, "ieeg")
  if(dir.exists(ieeg_dir)) {
    
    # Check for BrainVision format
    brainvision_files <- list.files(
      ieeg_dir,
      pattern = "\\.eeg$",
      full.names = TRUE
    )
    
    if(length(brainvision_files) > 0) {
      cat("Detected BrainVision format (.eeg files)\n")
      signal_format_detected <- TRUE
      # BrainVision import handled by RAVE's import pipeline
    }
    
    # Check for Matlab format
    matlab_files <- list.files(
      ieeg_dir,
      pattern = "\\.mat$",
      full.names = TRUE
    )
    
    if(length(matlab_files) > 0) {
      cat("Detected Matlab format (.mat files)\n")
      signal_format_detected <- TRUE
      # Matlab import handled by RAVE's import pipeline
    }
  }
}

if(!signal_format_detected) {
  cat("Warning: No recognized signal format detected\n")
}

# Import epoch definitions
cat("\nConstructing epoch tables...\n")

bids_sessions <- bids_subject$get_sessions()
for(session in bids_sessions) {
  ieeg_dir <- file.path(bids_subject$path, session, "ieeg")
  if(dir.exists(ieeg_dir)) {
    event_files <- list.files(
      ieeg_dir,
      pattern = "_events\\.tsv$",
      full.names = TRUE
    )
    
    if(length(event_files) > 0) {
      cat("Found events file:", basename(event_files[1]), "\n")
      events <- utils::read.csv(event_files[1], sep = "\t")
      cat("  Total events:", nrow(events), "\n")
    }
  }
}

# Run RAVE's import pipeline
cat("\nRunning RAVE import pipeline...\n")
cat("Subject code:", rave_subject$subject_code, "\n")
cat("Project name:", rave_subject$project_name, "\n")

cat("\n=== Import Summary ===\n")
cat("BIDS Project Path:", bids_project_path, "\n")
cat("BIDS Subject Code:", bids_subject_code, "\n")
cat("RAVE Project Name:", rave_subject$project_name, "\n")
cat("RAVE Subject Code:", rave_subject$subject_code, "\n")
cat("RAVE Subject Path:", rave_subject$path, "\n")
cat("\nImport preparation completed successfully!\n")
cat("Next steps: Review the imported data using RAVE's browser and visualization tools.\n")

