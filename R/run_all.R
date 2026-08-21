# Run the reproducible first-stage workflow
source(file.path("R", "01_world_interest_rate.R"))
source(file.path("R", "02_update_parameter_registry.R"))

cat("\nAll currently implemented research scripts completed.\n")
