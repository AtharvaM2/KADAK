source("cargo_pricing_model.r")

cargo_no_gold_platinum_results <- run_cargo_pipeline(
  exclude_types = c("gold", "platinum"),
  output_prefix = "cargo_no_gold_platinum",
  coverage_note = "Precious metals excluded",
  run_stress = TRUE
)

cat("\nNo-gold/platinum cargo stress outputs refreshed.\n")
