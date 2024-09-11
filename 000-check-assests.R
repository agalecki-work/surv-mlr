require(mlr3)
require(mlr3pipelines)

#--- List of available tasks


# ========= List all available PipeOps
available_pipeops <- as.data.table(mlr_pipeops)[, c("key", "input.num", "output.num", "packages")]
print(available_pipeops)

# Print names of all available PipeOps
pipeop_names <- available_pipeops[["key"]]
print(pipeop_names)

# Filter and print details for specific types of PipeOps (e.g., mutation)
mutation_pipeops <- available_pipeops[grepl("mutate", key)]
print(mutation_pipeops)

#--- Inspect Detailed Information for a Specific PipeOp

# Create an instance of a specific PipeOp, e.g., "mutate"
pipeop_instance <- po("colapply")  # mutate,colapply
print(pipeop_instance)
names(pipeop_instance)


# To inspect all parameters of 'PipeOpMutate'
params <- pipeop_instance$param_set
print(params)
names(params)
