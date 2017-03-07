# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# Messages =====================================================================
context(.ttmsg.("> Basics", "messages"))

# + checklist ------------------------------------------------------------------
test_that("checklist", {

  msg <- "missing"
  lst <- LETTERS[1:3]

  chk <- rep(T, 3)
  expect_silent(checklist(chk, lst, msg))

  chk <- lst != "B"
  expect_error(checklist(chk, lst, msg), "missing\nB")

  chk <- rep(F, 3)
  expect_error(checklist(chk, lst, msg), "missing\nA\nB\nC")
})

# + txt_pad --------------------------------------------------------------------
test_that("txt_pad", {

  expect_identical(txt_pad(), "")

  a <- txt_pad("A", "B")
  b <- "A B"
  expect_identical(a, b)

  a <- txt_pad("A", "B", sep = "-")
  b <- "A-B"
  expect_identical(a, b)

  a <- txt_pad("A", "B", sep = "\n", split_lines = T)
  b <- c("A", "B")
  expect_identical(a, b)

  a <- txt_pad(x = "-", n = 5)
  b <- "-----"
  expect_identical(a, b)

  a <- txt_pad("+", x = "-", n = 5)
  b <- "+ ---"
  expect_identical(a, b)

  a <- txt_pad(x = "-", "+", n = 5)
  b <- "--- +"
  expect_identical(a, b)

  a <- txt_pad(x = "-", "+", x = "-", n = 5)
  b <- "- + -"
  expect_identical(a, b)

  a <- txt_pad("A", x = "/", "B", x = "/", "C", n = 9)
  b <- "A / B / C"
  expect_identical(a, b)

  a <- txt_pad("A", x = "/", "B", x = "/", "C", n = 11)
  b <- "A // B // C"
  expect_identical(a, b)
})
# + txt_out --------------------------------------------------------------------
test_that("txt_out", {

  r <- "abc --- xyz"
  expect_output(txt_out("abc", x = "-", "xyz", n = 11), r)

  r <- "abc\n---\nxyz"
  expect_output(txt_out("abc", x = "-", "xyz", n = 3, sep = "\n"), r)

  r <- "  abc\n  ---\n  xyz"
  expect_output(txt_out("abc", x = "-", "xyz", indent = 2, sep = "\n"), r)

  r <- "---\n1\n---\n123\n---"
  expect_output(txt_out(x = "-", "1", x = "-", "123", x = "-", sep = "\n"), r)
})
