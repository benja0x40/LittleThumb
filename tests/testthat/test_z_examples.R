# > Examples ===================================================================
context("Examples")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  # Create a simple R object named 'a'
  a <- 1:10

  # Save object a in the current working directory
  SaveObj(a)

  # Simulate restarting the R environment
  rm(a)

  # Check if object a is available in the R environment (FALSE)
  expect_true(exists("a"))

  # Check if an RDS file is available for object a (TRUE)
  expect_true(AvailableObj(a))

  # Load object a from the current working directory
  LoadObj(a)

  # Delete the RDS file associated to object a and remove the object itself
  DeleteObj(a)

  # Now both lines below should return FALSE
  expect_false(AvailableObj(a))
  expect_false(exists("a"))

  # A verbose way to automatically make/save/load the object 'a' (don't use this)
  if(AvailableObj(a)) {
    LoadObj(a)
  } else {
    a <- 1:10
    SaveObj(a)
  }

  # Automatically make/save/load the object 'a'
  MakeObj(a, { a <- 1:10 })

  # MkObj is a short alias of function MakeObj
  MkObj(a, { a <- 1:10 })

  # Cleanup
  DeleteObj(a)

})
