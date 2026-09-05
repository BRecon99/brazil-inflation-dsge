# =============================================================================
# RUN ALL EMPIRICAL RESEARCH SCRIPTS
# =============================================================================

cat("\n============================================================\n")
cat("STARTING EMPIRICAL PIPELINE\n")
cat("============================================================\n")


cat("\n[1/3] World interest-rate estimation\n")

source(
  file.path(
    "R",
    "01_world_interest_rate.R"
  ),
  local = new.env()
)


cat("\n[2/3] Updating world-rate parameter registry\n")

source(
  file.path(
    "R",
    "02_update_parameter_registry.R"
  ),
  local = new.env()
)


cat("\n[3/3] Brazilian steady-state inflation calibration\n")

source(
  file.path(
    "R",
    "03_brazil_inflation_steady_state.R"
  ),
  local = new.env()
)


cat("\n============================================================\n")
cat("EMPIRICAL PIPELINE COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")