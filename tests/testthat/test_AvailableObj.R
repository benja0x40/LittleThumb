# > AvailableObj ===============================================================
context("AvailableObj")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  x <- 1:10
  f <- SaveObj(x)

  expect_true(AvailableObj(x))
  expect_false(AvailableObj(y))

  expect_true(AvailableObj(name = "x"))
  expect_false(AvailableObj(name = "y"))

  # Cleanup
  expect_true(file.remove(f))
})
