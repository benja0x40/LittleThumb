# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# S3 DEFINITION ================================================================
context(.ttmsg.("LT_TabularData", "definition"))

# + new ------------------------------------------------------------------------
test_that("new", {

  r <- LT_TabularData()

  expect_s3_class(r, c("LT_TabularData", "data.frame"))
  expect_true(inherits(r, "data.frame"))
})

# > TextRepresentation =========================================================
context(.ttmsg.("LT_TabularData", "> TextRepresentation"))

# + obj2txt --------------------------------------------------------------------
test_that("obj2txt", {

  ltn <- "something"
  r <- LT_TabularData(val = LETTERS[24:25], nbr = 1:2)

  s <- obj2txt(r, name = ltn)
  expect_s3_class(s, c("LT_TabularData", "character"))
  expect_true(inherits(s, "LT_TabularData"))
  expect_true(inherits(s, "character"))

  # cat(paste0(s, "\n"), sep = "")
  expect_equal(length(s), 5)

  s <- obj2txt(r, rn = T)
  expect_match(s[2], "^# =+", perl = T)
  expect_match(s[1], "# Tabular data \\(with row names.*\\)", perl = T)
  expect_identical(s[3], "val\tnbr")
  expect_identical(s[4], "1\tX\t1")
  expect_identical(s[5], "2\tY\t2")

  s <- obj2txt(r, name = ltn)
  expect_identical(s[1], paste0("# Tabular data : ", ltn))

  attr(r, "lt_name") <- ltn
  s <- obj2txt(r)
  expect_identical(s[1], paste0("# Tabular data : ", ltn))
})

# + txt2obj --------------------------------------------------------------------
test_that("txt2obj", {

  ltn <- "something"
  x <- LT_TabularData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)

  y <- txt2obj(obj2txt(x))
  expect_identical(x, y)

  y <- txt2obj(obj2txt(x, name = ltn))
  expect_identical(attr(y, "lt_name"), ltn)

  attr(x, "lt_name") <- ltn
  y <- txt2obj(obj2txt(x))
  expect_identical(x, y)

  rownames(x) <- rev(rownames(x))
  y <- txt2obj(obj2txt(x, rn = T))
  expect_identical(x, y)
})

# > Registration ===============================================================
context(.ttmsg.("LT_TabularData", "> Registration"))

# + is_key ---------------------------------------------------------------------
test_that("is_key", {

  r <- LT_TabularData(key = LETTERS[1:3], val = LETTERS[24:26], nbr = 1:3)

  expect_true(is_key(r, 1:3))
  expect_true(is_key(r, colnames(r)))

  expect_false(is_key(r, 0:1))
  expect_false(is_key(r, c("notpresent", "nbr")))

  expect_error(is_key(r, 0:1, error = T))
  expect_error(is_key(r, c("notpresent", "nbr"), error = T))
})

# + td_selector ----------------------------------------------------------------
test_that("td_selector", {

  r <- LT_TabularData(
    key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6,
    group = as.numeric(gl(3, 2))
  )

  s <- td_selector(r, nbr %% 2 == 1)
  expect_identical(s, rep(c(T, F), 3))

  s <- td_selector(r, nbr %% 2 == 1, v = c("key", "val"))
  expect_identical(s, r[c(1, 3, 5), 1:2])

  k <- 3
  s <- td_selector(r, nbr == k | group == k)
  expect_identical(s, c(F, F, T, F, T, T))

  s <- td_selector(r, nbr == k | group == k, v = c("key", "val"))
  expect_identical(s, r[c(3, 5:6), 1:2])
})

# + is_registered --------------------------------------------------------------
test_that("is_registered", {

  r <- LT_TabularData(key = LETTERS[1:3], val = LETTERS[24:26], nbr = 1:3)

  expect_true(is_registered(r, "A", k = "key"))
  expect_true(is_registered(r, "Z", k = "val"))

  expect_false(is_registered(r, "0", k = "key"))
  expect_false(is_registered(r, "0", k = "val"))

  expect_error(
    is_registered(r, "0", k = "key", lbl = "id", error = T), "undefined id 0"
  )
  expect_error(
    is_registered(r, "0", k = "val", lbl = "id", error = T), "undefined id 0"
  )
})

# + register_value -------------------------------------------------------------
test_that("register_value", {

  r <- LT_TabularData(key = LETTERS[1:3], val = LETTERS[24:26], nbr = 1:3)

  expect_identical(register_value(r, k = "key", "A", v = "val"), "X")
  expect_identical(register_value(r, k = "val", "Z", v = "key"), "C")

  register_value(r, k = "key", "A", v = "val") <- "1"
  register_value(r, k = "val", "Z", v = "key") <- "2"

  expect_identical(register_value(r, k = "key", "A", v = "val"), "1")
  expect_identical(register_value(r, k = "val", "Z", v = "key"), "2")

  expect_error(register_value(r, k = "key", "0", v = "val"))
  expect_error(register_value(r, k = "val", "0", v = "key"))

  x <- r
  x[2, 3] <- 0
  y <- r
  register_value(y, k = "key", "B", v = "nbr") <- 0
  expect_identical(x, y)
})

# + register_filter ------------------------------------------------------------
test_that("register_filter", {

  r <- LT_TabularData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)

  expect_error(register_filter(r, "notpresent == 0"))
  expect_error(register_filter(r, quote(x == 0)))

  f <- register_filter(r, nbr > 3, rn = T)
  expect_equal(f, r[4:6, ])

  f <- register_filter(r, "nbr > 3", rn = T)
  expect_identical(f, r[4:6, ])
})

# + register_split -------------------------------------------------------------
test_that("register_split", {

  r <- LT_TabularData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)

  expect_error(register_split(r, "notpresent"))
  expect_error(register_split(r, 1:2))

  x <- register_split(r, "val", ids = F, rn = T)
  y <- register_split(r, rep(1:3, 2), ids = F, rn = T)
  expect_identical(x, y)

})

# + register_merge -------------------------------------------------------------
test_that("register_merge", {

  r <- LT_TabularData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)
  g <- as.character(gl(3, 2))
  g <- cbind(r, group = g, stringsAsFactors = F)
  s <- register_split(r, g$group, ids = F)

  m <- register_merge(s[[2]], s[[3]], rn = T)
  expect_identical(m, r[3:6, ])

  m <- register_merge(s[[2]], s[[3]], ids = "group", rn = T)
  m$group <- as.character(as.numeric(m$group) + 1)
  expect_identical(m, g[3:6, ])

  r <- LT_TabularData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)
  g <- paste0("G", gl(3, 2))
  g <- cbind(r, group = g, stringsAsFactors = F)
  s <- register_split(r, g$group, ids = T)

  m <- register_merge(G2 = s[[2]], G3 = s[[3]], ids = "group", rn = T)
  expect_identical(m, g[3:6, ])

  m <- register_merge(s[2:3], ids = "group", rn = T)
  expect_identical(m, g[3:6, ])

  s <- register_split(r, g$group, ids = T)
  s[[1]]$nbr <- NULL
  s[[2]]$val <- NULL
  s[[3]]$key <- NULL

  m <- register_merge(s, rn = T)
  expect_identical(m$key, c(r$key[1:4], NA, NA))
  expect_identical(m$nbr, c(NA, NA, r$nbr[3:6]))

  m <- register_merge(s[1:2], full = F, rn = T)
  expect_identical(m, r[1:4, 1, drop = F])

  m <- register_merge(s[2:3], full = F, rn = T)
  expect_identical(m, r[3:6, 3, drop = F])

  m <- register_merge(s, full = F, rn = T)
  expect_equal(ncol(m), 0)
  expect_equal(nrow(m), 6)
})
