# =============================================================================
# run_all.R
#
# Run all parameter-estimation modules and then update the parameter registry.
# =============================================================================

rm(list = ls())


scripts <- c(
  "R/01_world_interest_rate.R",
  "R/02_foreign_output.R",
  "R/03_brazil_inflation_steady_state.R",
  "R/04_fiscal_processes.R",
  "R/04i_steady_state_fiscal_shares",
  "R/99_update_parameter_registry.R"
)


for (script in scripts) {
  
  cat("\n\n")
  cat("============================================================\n")
  cat("RUNNING:", script, "\n")
  cat("============================================================\n\n")
  
  
  if (!file.exists(script)) {
    
    stop(
      "Cannot find script: ",
      script
    )
  }
  
  
  script_environment <- new.env(
    parent = globalenv()
  )
  
  
  sys.source(
    script,
    envir = script_environment
  )
  
  
  rm(
    script_environment
  )
}


cat("\n============================================================\n")
cat("FULL PARAMETER PIPELINE COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")