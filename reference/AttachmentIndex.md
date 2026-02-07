# Attachment Index for Workflow Result Tracking

R6 class that manages the attachment index for workflow execution.
Backed by RSQLite, this class provides atomic, query-friendly tracking
of attachment lifecycle status. All database access uses
open/close-per-operation pattern for safety.

## Details

The index tracks each attachment through its lifecycle:

- init:

  Runtime created, execution not yet started

- running:

  Agent execution in progress

- finished:

  Execution completed successfully

- errored:

  Execution completed with error

- skipped:

  Execution was skipped (reserved for future use)

### Backend Abstraction

All `SQLite` calls are isolated behind the private `$.with_db()` method.
To swap backends (e.g., duckdb), only `$.with_db()` and `$.init_db()`
need modification.

## Methods

### Public methods

- [`AttachmentIndex$new()`](#method-TricobblerAttachmentIndex-new)

- [`AttachmentIndex$register()`](#method-TricobblerAttachmentIndex-register)

- [`AttachmentIndex$update_status()`](#method-TricobblerAttachmentIndex-update_status)

- [`AttachmentIndex$mark_finished()`](#method-TricobblerAttachmentIndex-mark_finished)

- [`AttachmentIndex$get()`](#method-TricobblerAttachmentIndex-get)

- [`AttachmentIndex$list()`](#method-TricobblerAttachmentIndex-list)

- [`AttachmentIndex$query()`](#method-TricobblerAttachmentIndex-query)

- [`AttachmentIndex$list_incomplete()`](#method-TricobblerAttachmentIndex-list_incomplete)

- [`AttachmentIndex$exists()`](#method-TricobblerAttachmentIndex-exists)

- [`AttachmentIndex$get_db_path()`](#method-TricobblerAttachmentIndex-get_db_path)

------------------------------------------------------------------------

### Method `new()`

Initialize the attachment index

#### Usage

    AttachmentIndex$new(db_path)

#### Arguments

- `db_path`:

  character, path to the SQLite database file

------------------------------------------------------------------------

### Method `register()`

Register a new attachment entry with status 'init'

#### Usage

    AttachmentIndex$register(attachment_id, stage, state, agent_id, attempt = 0L)

#### Arguments

- `attachment_id`:

  character, unique attachment identifier

- `stage`:

  character, stage name

- `state`:

  character, state name

- `agent_id`:

  character, agent identifier

- `attempt`:

  integer, attempt number

------------------------------------------------------------------------

### Method `update_status()`

Update the status of an existing attachment

#### Usage

    AttachmentIndex$update_status(attachment_id, status)

#### Arguments

- `attachment_id`:

  character, attachment identifier

- `status`:

  character, new status

------------------------------------------------------------------------

### Method `mark_finished()`

Mark an attachment as finished or errored

#### Usage

    AttachmentIndex$mark_finished(attachment_id, succeed)

#### Arguments

- `attachment_id`:

  character, attachment identifier

- `succeed`:

  logical, whether execution succeeded

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Retrieve a single index entry

#### Usage

    AttachmentIndex$get(attachment_id)

#### Arguments

- `attachment_id`:

  character, attachment identifier

#### Returns

A single-row data.frame or `NULL` if not found

------------------------------------------------------------------------

### Method [`list()`](https://rdrr.io/r/base/list.html)

List all index entries

#### Usage

    AttachmentIndex$list(status = NULL)

#### Arguments

- `status`:

  character or `NULL`, optional status filter

#### Returns

A data.frame of all matching entries (most recent first)

------------------------------------------------------------------------

### Method `query()`

Query index entries by state and/or stage

#### Usage

    AttachmentIndex$query(state, stage = NULL, status = NULL)

#### Arguments

- `state`:

  character, state name to filter by

- `stage`:

  character or `NULL`, optional stage filter

- `status`:

  character or `NULL`, optional status filter

#### Returns

A data.frame of matching entries (most recent first)

------------------------------------------------------------------------

### Method `list_incomplete()`

Find incomplete entries (init or running past timeout)

#### Usage

    AttachmentIndex$list_incomplete(timeout_secs = NULL)

#### Arguments

- `timeout_secs`:

  numeric, seconds after which init/running entries are considered
  incomplete. If `NULL` (default), returns all entries with status
  'init' or 'running' regardless of age.

#### Returns

A data.frame of incomplete entries

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if an attachment exists in the index

#### Usage

    AttachmentIndex$exists(attachment_id)

#### Arguments

- `attachment_id`:

  character, attachment identifier

#### Returns

logical

------------------------------------------------------------------------

### Method `get_db_path()`

Get the database file path

#### Usage

    AttachmentIndex$get_db_path()

#### Returns

character, path to the SQLite database
