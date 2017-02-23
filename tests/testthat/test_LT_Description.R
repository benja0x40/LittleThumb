# =============================================================================.
context("LT_Description.R | functions")
# -----------------------------------------------------------------------------.
test_that("obj2txt, txt2obj", {
  a <- new("LT_Description", df = DataFrame(mtcars))
  b <- txt2obj(obj2txt(a))
  expect_identical(a@properties, b@properties)
  expect_identical(as.matrix(a@df), as.matrix(b@df))
})
# -----------------------------------------------------------------------------.
