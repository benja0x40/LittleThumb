# Create a simple R object named 'a'
a <- 1:10

# Save object a in the current working directory
SaveObj(a)

# Simulate restarting the R environment
rm(a)

# Check if object a is available in the R environment (FALSE)
exists("a")

# Check if an RDS file is available for object a (TRUE)
AvailableObj(a)

# Load object a from the current working directory
LoadObj(a)

print(a)

# Delete the RDS file associated to object a and remove the object itself
DeleteObj(a)

# Now both lines below should return FALSE
AvailableObj(a)
exists("a")

# A verbose way to automatically make/save/load the object 'a' (don't use this)
if(AvailableObj(a)) {
  LoadObj(a)
} else {
  a <- 1:10
  SaveObj(a)
}

# Automatically make/save/load the object 'a'
MakeObj(a, { a <- 1:10 })

# Cleanup ======================================================================
DeleteObj(a)
unlink("AutoSaved", recursive = TRUE)
