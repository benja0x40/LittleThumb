# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# LT Objects ===================================================================
context(.ttmsg.("> FileSystem", "functions"))

# + make_path ------------------------------------------------------------------
test_that("make_path", {

  expect_identical("a/b/c",     make_path( "a", "b", "c"))
  expect_identical("a/b/c/",    make_path( "a", "b", "c/"))
  expect_identical("/a/b/c",    make_path("/a", "b", "c"))

  expect_identical("a/b/c",     make_path("a///b//", "/c"))
  expect_identical("a/b/c",     make_path("a/", "b", "/c"))
  expect_identical("a/c",       make_path("a/", "/", "/c"))

  expect_identical("a/b/c.txt", make_path("a", "b", "c",  ext = ".txt"))
  expect_identical("a/b/c/.hidden", make_path("a", "b", "c/", ext = ".hidden"))

  expect_identical( "~/a/b.txt", make_path( "~", "a", "b",  ext = ".txt"))
  expect_identical("../a/b.txt", make_path("..", "a", "b",  ext = ".txt"))
})
