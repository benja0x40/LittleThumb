# > Dependencies ===============================================================
context("Dependencies")

# + Basics ---------------------------------------------------------------------
test_that("Basics", {

  expect_false(IsKnowObject("x"))

  RegisterObject("x")
  expect_true(IsKnowObject("x"))
  ForgetObject("x")
  expect_false(IsKnowObject("x"))

  RegisterObject("a")
  RegisterObject("b")

  RegisterObject("x")
  RegisterObject("y")
  RegisterObject("z")

  SetParent("x", "b")
  SetParent("y", "b")
  SetParent("z", "b")

  SetParent("b", "a")

  expect_null(GetParents("a"))
  expect_identical(GetParents("b"), "a")

  expect_identical(GetParents("x"), c("a", "b"))
  expect_identical(GetParents("y"), c("a", "b"))
  expect_identical(GetParents("z"), c("a", "b"))

  expect_identical(GetChilds("a"), "b")
  expect_identical(GetChilds("b"), c("x", "y", "z"))

  # Cleanup
  ResetRegistry()
})
