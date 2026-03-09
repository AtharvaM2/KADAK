source("cargo_pricing_model.r")

cargo_results <- run_cargo_pipeline(
  exclude_types = character(),
  output_prefix = "cargo",
  coverage_note = "Baseline cargo cover",
  run_stress = TRUE
)

cat("\nBaseline cargo stress outputs refreshed.\n")
