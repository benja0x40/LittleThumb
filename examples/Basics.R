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
DeleteObj(a, remove = T)

# Now both lines below should return FALSE
AvailableObj(a)
exists("a")

# Concise way to automatically make/save/load object a
MakeObj(a, { a <- 1:10 })

print(a)

# Cleanup
DeleteObj(a, remove = T)
