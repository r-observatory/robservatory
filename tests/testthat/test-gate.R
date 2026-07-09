test_that("check_size accepts a healthy count and rejects shrinkage", {
  # First run: no baseline, only the floor applies.
  expect_true(check_size(20000, baseline = NULL, floor = 1000))
  expect_false(check_size(500, baseline = NULL, floor = 1000))

  # With a baseline: reject a drop of more than (1 - tolerance).
  expect_true(check_size(19900, baseline = 20000, floor = 1000))   # 0.5% drop, allowed
  expect_false(check_size(19000, baseline = 20000, floor = 1000))  # 5% drop, rejected
  expect_true(check_size(21000, baseline = 20000, floor = 1000))   # growth is fine

  # Degenerate inputs are never trustworthy.
  expect_false(check_size(NA_integer_, baseline = 20000, floor = 1000))
  expect_false(check_size(0, baseline = NULL, floor = 1000))
})
