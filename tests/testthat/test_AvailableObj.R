# > AvailableObj ===============================================================
context("AvailableObj")

# + Basics ---------------------------------------------------------------------
test_that("Basics", {


  expect_error(AvailableObj(), regexp = "insufficient")

  x <- 1:10
  expect_false(AvailableObj(x))

  f <- SaveObj(x)
  expect_true(AvailableObj(x))

  expect_true(AvailableObj(name = "x"))
  expect_false(AvailableObj(name = "y"))

  expect_error(AvailableObj(x, parent.name = "a"))

  RegisterObject("a")
  expect_false(AvailableObj(x, parent.name = "a"))

  # Cleanup
  expect_true(file.remove(f))
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})
