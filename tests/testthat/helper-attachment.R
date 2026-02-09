# Helper for testing attachments since record_result API changed on AgentContext

record_dummy_result <- function(
    ctx, result, stage, state, agent_id,
    current_attempt, description = "") {
  
  # Create attachment ID manually (mimicking AgentRuntime)
  # [stage][state][agentid]_YYMMDDTHHMMSS_{attempt}
  timestamp <- format(Sys.time(), "%y%m%dT%H%M%S")
  # Ensure uniqueness if called multiple times quickly
  Sys.sleep(0.01) 
  
  attachment_id <- sprintf(
    "[%s][%s][%s]_%s_%d",
    stage, state, agent_id, timestamp, current_attempt
  )
  
  # Mock runtime object
  # Create S7 objects for type safety in AgentContext@record_attachment
  
  agent <- Agent(
    function(runtime, ...) "dummy",
    id = agent_id,
    description = "Dummy agent"
  )
  
  policy <- StatePolicy(
    name = state,
    stage = stage,
    agent_id = agent_id
  )
  
  # Create actual AgentRuntime object instead of mock list
  # This requires proper initialization
  
  # To make this helper control the ID, we might need to be careful.
  # But AgentContent$record_attachment uses runtime$attachment_id.
  
  # Let's reconstruct the one from AgentRuntime to match our file writing
  # The AgentRuntime generates ID based on Sys.time(). 
  # If we want to control it, we might be fighting the class.
  
  # Alternative: Let AgentRuntime generate the ID,
  # and use THAT to write the file.
  
  # Let's try to instantiate AgentRuntime first, get its ID, and use it.
  runtime <- AgentRuntime$new(
    agent = agent,
    context = ctx,
    policy = policy,
    attempt = current_attempt
  )
  attachment_id <- runtime$attachment_id
  
  # Prepare attachment data
  attachment <- list(
    result = result,
    succeed = TRUE,
    description = description,
    agent_id = agent_id,
    stage = stage,
    state = state,
    current_attempt = current_attempt,
    id = attachment_id,
    ._timestamp = Sys.time()
  )
  
  # Write file
  attachment_folder <- ctx$attachment_path
  if (!dir.exists(attachment_folder)) {
    dir.create(attachment_folder, recursive = TRUE, showWarnings = FALSE)
  }
  attachment_path <- file.path(
    attachment_folder,
    sprintf("%s.rds", attachment_id)
  )
  saveRDS(attachment, attachment_path)
  
  # Update index
  # record_attachment expects a runtime object
  ctx$record_attachment(runtime, succeed = TRUE)
  
  invisible(attachment_id)
}
