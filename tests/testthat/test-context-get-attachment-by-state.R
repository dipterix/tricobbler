
test_that("get_attachment_by_state retrieves correct attachment", {
  # Setup context
  ctx <- AgentContext$new()
  ctx$init_resources()
  
  # Record some results
  # State A, attempt 1
  ctx$record_result(
    result = "result_A1",
    stage = "stage1",
    state = "stateA",
    agent_id = "agent1",
    current_attempt = 1,
    description = "A1"
  )
  
  # State A, attempt 2 (retry)
  ctx$record_result(
    result = "result_A2",
    stage = "stage1",
    state = "stateA",
    agent_id = "agent1",
    current_attempt = 2,
    description = "A2"
  )
  
  # State B
  ctx$record_result(
    result = "result_B",
    stage = "stage1",
    state = "stateB",
    agent_id = "agent2",
    current_attempt = 1,
    description = "B"
  )
  
  # State A in stage 2 (same state name, different stage)
  ctx$record_result(
    result = "result_A_stage2",
    stage = "stage2",
    state = "stateA",
    agent_id = "agent1",
    current_attempt = 1,
    description = "A_s2"
  )
  
  # Test retrieval
  
  # 1. Retrieve latest stateA (should be stage2)
  att <- ctx$get_attachment_by_state("stateA", stage = "stage2")
  expect_equal(att$result, "result_A_stage2")
  expect_equal(att$stage, "stage2")
  
  # 2. Retrieve stateA from stage1 (should be attempt 2)
  att <- ctx$get_attachment_by_state("stateA", stage = "stage1")
  expect_equal(att$result, "result_A2")
  expect_equal(att$current_attempt, 2)
  
  # 3. Retrieve stateB
  att <- ctx$get_attachment_by_state("stateB", stage = "stage1")
  expect_equal(att$result, "result_B")
  
  # 4. Retrieve non-existent state
  expect_null(ctx$get_attachment_by_state("missingstate", stage = "stage1"))
  
  # 5. Validation
  expect_error(ctx$get_attachment_by_state(), "required")
})
