# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# DEFINITION ===================================================================
context(.ttmsg.("LT_MetaData", "definition"))

# + new ------------------------------------------------------------------------
test_that("new", {

  r <- LT_MetaData(key = LETTERS[1:3], val = LETTERS[24:26], nbr = 1:3)
  expect_is(r, "list")
})

# > TextRepresentation =========================================================
context(.ttmsg.("LT_MetaData", "> TextRepresentation"))

# + obj2txt --------------------------------------------------------------------
test_that("obj2txt", {

  md1 <- as.list(LETTERS[1:3]); names(md1) <- letters[24:26]
  md2 <- as.list(1:3); names(md2) <- letters[24:26]
  r <- LT_MetaData(md1, md2, .names. = c("md1", "md2"))

  ltn <- "something"
  rxn <- paste0("^# /+ jsonlite : ", ltn, " /+$")
  r <- LT_MetaData(lbl = letters[1:2], nbr = 1:2)

  s <- obj2txt(r)
  expect_s3_class(s, c("LT_MetaData", "character"))
  expect_true(inherits(s, "LT_MetaData"))
  expect_true(inherits(s, "character"))

  # cat(paste0(s, "\n"), sep = "")
  expect_match(s[1], "^# /+ jsonlite /+$", perl = T)

  s <- obj2txt(r, name = ltn)
  expect_match(s[1], rxn, perl = T)

  attr(r, "lt_name") <- ltn
  s <- obj2txt(r)
  expect_match(s[1], rxn, perl = T)
})

# + txt2obj --------------------------------------------------------------------
test_that("txt2obj", {

  ltn <- "something"
  x <- LT_MetaData(lbl = letters[1:2], nbr = 1:2)

  y <- txt2obj(obj2txt(x))
  expect_identical(x, y)

  y <- txt2obj(obj2txt(x, name = ltn))
  expect_identical(attr(y, "lt_name"), ltn)

  attr(x, "lt_name") <- ltn
  y <- txt2obj(obj2txt(x))
  expect_identical(x, y)
})

# > Registration =========================================================
context(.ttmsg.("LT_MetaData", "> Registration"))

# + is_key ---------------------------------------------------------------------
test_that("is_key", {

  md1 <- as.list(LETTERS[24:26]); names(md1) <- LETTERS[1:3]
  md2 <- as.list(1:3); names(md2) <- letters[24:26]
  r <- LT_MetaData(md1, md2, .names. = c("md1", "md2"))

  expect_true(is_key(r, "md1"))
  expect_true(is_key(r, names(r)))

  expect_false(is_key(r, 0:1))
  expect_false(is_key(r, c("notpresent", "nbr")))

  expect_error(is_key(r, 0:1, error = T))
  expect_error(is_key(r, c("notpresent", "nbr"), error = T))
})


# + is_registered --------------------------------------------------------------
test_that("is_registered", {

  md1 <- as.list(LETTERS[1:3]); names(md1) <- letters[24:26]
  md2 <- as.list(1:3); names(md2) <- letters[24:26]
  r <- LT_MetaData(md1, md2, .names. = c("md1", "md2"))

  expect_true(is_registered(r, k = "md1", "x"))
  expect_true(is_registered(r, k = "md2", "z"))

  expect_false(is_registered(r, k = "md1", "0"))
  expect_false(is_registered(r, k = "md2", "0"))

  expect_error(
    is_registered(r, k = "md1", "0", lbl = "id", error = T), "undefined id 0"
  )
  expect_error(
    is_registered(r, k = "md2", "0", lbl = "id", error = T), "undefined id 0"
  )
})

# # + register_value -------------------------------------------------------------
# test_that("register_value", {
#
#   r <- LT_MetaData(key = LETTERS[1:3], val = LETTERS[24:26], nbr = 1:3)
#
#   expect_identical(register_value(r, "A", k = "key", v = "val"), "X")
#   expect_identical(register_value(r, "Z", k = "val", v = "key"), "C")
#
#   register_value(r, "A", k = "key", v = "val") <- "1"
#   register_value(r, "Z", k = "val", v = "key") <- "2"
#
#   expect_identical(register_value(r, "A", k = "key", v = "val"), "1")
#   expect_identical(register_value(r, "Z", k = "val", v = "key"), "2")
#
#   expect_error(register_value(r, "0", k = "key", v = "val"))
#   expect_error(register_value(r, "0", k = "val", v = "key"))
#
#   x <- r
#   x[2, 3] <- 0
#   y <- r
#   register_value(y, "B", k = "key", v = "nbr") <- 0
#   expect_identical(x, y)
# })

# # + register_filter ------------------------------------------------------------
# test_that("register_filter", {
#
#   r <- LT_MetaData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)
#
#   expect_error(register_filter(r, "notpresent == 0"))
#   expect_error(register_filter(r, quote(x == 0)))
#
#   f <- register_filter(r, quote(nbr > 3), rn = T)
#   expect_equal(f, r[4:6, ])
#
#   f <- register_filter(r, "nbr > 3", rn = T)
#   expect_identical(f, r[4:6, ])
# })
#
# # + register_split -------------------------------------------------------------
# test_that("register_split", {
#
#   r <- LT_MetaData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)
#
#   expect_error(register_split(r, "notpresent"))
#   expect_error(register_split(r, 1:2))
#
#   x <- register_split(r, "val", ids = F, rn = T)
#   y <- register_split(r, rep(1:3, 2), ids = F, rn = T)
#   expect_identical(x, y)
#
# })
#
# # + register_merge -------------------------------------------------------------
# test_that("register_merge", {
#
#   r <- LT_MetaData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)
#   g <- as.character(gl(3, 2))
#   g <- cbind(r, group = g, stringsAsFactors = F)
#   s <- register_split(r, g$group, ids = F)
#
#   m <- register_merge(s[[2]], s[[3]], rn = T)
#   expect_identical(m, r[3:6, ])
#
#   m <- register_merge(s[[2]], s[[3]], ids = "group", rn = T)
#   m$group <- as.character(as.numeric(m$group) + 1)
#   expect_identical(m, g[3:6, ])
#
#   r <- LT_MetaData(key = LETTERS[1:6], val = LETTERS[24:26], nbr = 1:6)
#   g <- paste0("G", gl(3, 2))
#   g <- cbind(r, group = g, stringsAsFactors = F)
#   s <- register_split(r, g$group, ids = T)
#
#   m <- register_merge(G2 = s[[2]], G3 = s[[3]], ids = "group", rn = T)
#   expect_identical(m, g[3:6, ])
#
#   m <- register_merge(s[2:3], ids = "group", rn = T)
#   expect_identical(m, g[3:6, ])
#
#   s <- register_split(r, g$group, ids = T)
#   s[[1]]$nbr <- NULL
#   s[[2]]$val <- NULL
#   s[[3]]$key <- NULL
#
#   m <- register_merge(s, rn = T)
#   expect_identical(m$key, c(r$key[1:4], NA, NA))
#   expect_identical(m$nbr, c(NA, NA, r$nbr[3:6]))
#
#   m <- register_merge(s[1:2], full = F, rn = T)
#   expect_identical(m, r[1:4, 1, drop = F])
#
#   m <- register_merge(s[2:3], full = F, rn = T)
#   expect_identical(m, r[3:6, 3, drop = F])
#
#   m <- register_merge(s, full = F, rn = T)
#   expect_equal(ncol(m), 0)
#   expect_equal(nrow(m), 6)
# })
