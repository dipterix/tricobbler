
test_that("Manifest validates depends_on correctness", {
  mp <- MasterPolicy(
    name = "test", version = "1.0.0",
    stages = c("one", "two"), parameters = list()
  )
  s_two_dummy <- StatePolicy(name = "dummy_s2", stage = "two", agent_id = "a0")
  
  # Valid Case: s1 -> s2 (same stage, priority order OK)
  # s1 (prio 90) depends on s2 (prio 100)
  s_dep <- StatePolicy(
    name = "upstream", stage = "one", priority = 100,
    agent_id = "a1"
  )
  s_main <- StatePolicy(
    name = "downstream", stage = "one", priority = 90,
    agent_id = "a2",
    depends_on = StateDeps(
      input = list(state = "upstream")
    )
  )
  
  expect_true(S7::S7_inherits(
    Manifest(master = mp, states = list(s_dep, s_main, s_two_dummy)),
    Manifest
  ))
  
  # Invalid: Missing state
  s_bad <- StatePolicy(
    name = "broken", stage = "one", priority = 80, agent_id = "a3",
    depends_on = StateDeps(x = list(state = "missing"))
  )
  expect_error(
    Manifest(master = mp, states = list(s_dep, s_main, s_bad, s_two_dummy)),
    "non-existent state 'missing'"
  )
  
  # Invalid: Priority violation (same state)
  # s_main (prio 90) depends on s_low (prio 80)
  s_low <- StatePolicy(
    name = "latetodinner", stage = "one", priority = 80, agent_id = "a4"
  )
  s_oops <- StatePolicy(
    name = "oops", stage = "one", priority = 90, agent_id = "a5",
    depends_on = StateDeps(x = list(state = "latetodinner"))
  )
  expect_error(
    Manifest(master = mp, states = list(s_low, s_oops, s_two_dummy)),
    "priority \\(80\\) is not higher than dependent priority \\(90\\)"
  )
  
  # Invalid: Future stage
  s_future <- StatePolicy(
    name = "future", stage = "two", agent_id = "a6"
  )
  s_past <- StatePolicy(
    name = "past", stage = "one", agent_id = "a7",
    depends_on = StateDeps(x = list(state = "future"))
  )
  expect_error(
    Manifest(master = mp, states = list(s_future, s_past)),
    "invalid future dependency"
  )
  
  # Invalid: Duplicate state names
  expect_error(
    Manifest(master = mp, states = list(s_dep, s_dep, s_two_dummy)),
    "unique names"
  )
  
  # Valid: Cross-stage
  s_s2 <- StatePolicy(
    name = "s2_state", stage = "two", agent_id = "a8",
    depends_on = StateDeps(prev = list(state = "upstream")) # upstream is stage "one"
  )
  expect_true(S7::S7_inherits(
    Manifest(master = mp, states = list(s_dep, s_s2)),
    Manifest
  ))
})
