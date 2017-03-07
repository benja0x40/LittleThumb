# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# LT Objects ===================================================================
context(.ttmsg.("> TextRepresentation", "functions"))

# + valid_id -------------------------------------------------------------------
test_that("valid_id", {

  a <- c(".", letters, LETTERS)
  d <- 0:9
  s <- c(".", "_")
  f <- c(" ", "+", "-", "*", "/", "=", ">", "<", "$", "#") # etc.

  for(i in 1:10) {
    x <- paste(c(sample(a, 1), sample(c(s, a, d), 7)), collapse = "")
    expect_true(valid_id(x))
  }
  for(i in 1:10) {
    x <- paste(c(sample(a, 1), sample(f, 1)), collapse = "")
    expect_false(valid_id(x))
  }

  x <- paste(c(sample(d, 1), sample(c(s, a, d), 7)), collapse = "")
  expect_false(valid_id(x))
  expect_error(valid_id(x, error = T))

  x <- paste(c("_", sample(c(s, a, d), 7)), collapse = "")
  expect_false(valid_id(x))
  expect_error(valid_id(x, error = T))

})

# + make_id --------------------------------------------------------------------
test_that("make_id", {

  expect_true(grepl("^[^ ]+$", make_id(), perl = T))
  for(i in 1:10) expect_true(valid_id(make_id(), type = "LT"))
})
