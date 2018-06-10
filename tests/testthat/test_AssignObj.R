# > AssignObj ==================================================================
context("AssignObj")

# + Basics ---------------------------------------------------------------------
test_that("Basics", {

  env <- new.env()
  lst <- list()

  x <- z <- LETTERS[1:5]
  i <- k <- 1:10

  expect_error(AssignObj(x, from = NonExistentObject))
  expect_error(AssignObj(x, to = NonExistentObject))
  expect_error(AssignObj(x, from = "NonExistentObject"), regexp = "'from'")
  expect_error(AssignObj(x, to = "NonExistentObject"), regexp = "'to'")

  AssignObj(x, to = env)
  AssignObj(i, to = lst)

  expect_true(length(env) == 1)
  expect_true(length(lst) == 1)
  expect_identical(env$x, z)
  expect_identical(lst$i, k)

  expect_false(exists("x"))
  expect_false(exists("i"))

  AssignObj(x, from = env, to = lst)
  AssignObj(i, from = lst, to = env)
  expect_true(length(env) == 1)
  expect_true(length(lst) == 1)
  expect_identical(env$i, k)
  expect_identical(lst$x, z)

  expect_silent(AssignObj(i, from = env, to = env))
  expect_silent(AssignObj(x, from = lst, to = lst))
  expect_identical(env$i, k)
  expect_identical(lst$x, z)

  AssignObj(i, from = env)
  AssignObj(x, from = lst)
  expect_true(length(env) == 0)
  expect_true(length(lst) == 0)

  expect_true(exists("x"))
  expect_true(exists("i"))
})
