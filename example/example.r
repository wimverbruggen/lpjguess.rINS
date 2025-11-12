# Read the ins file
myins <- read_ins("example/example.ins")

# Resolve groups
myins_res <- resolve_groups(myins)

# Change one parameter value
myins_res$pft$grass$param2 <- 45

# Write final ins file
write_ins(myins_res,"example/example_resolved.ins")

