source("cargo_pricing_model.r")

cargo_no_gold_platinum_results <- run_cargo_pipeline(
  exclude_types = c("gold", "platinum"),
  output_prefix = "cargo_no_gold_platinum",
  coverage_note = "Precious metals excluded from base cargo product",
  run_stress = TRUE
)

# Concise recommendation file for report
recommendations <- data.frame(
  recommendation_id = c("R1", "R2", "R3", "R4"),
  recommendation = c(
    "Adopt base cargo cover excluding gold and platinum to reduce concentration risk.",
    "Offer optional precious-metals catastrophe rider with strict underwriting, high deductible, and co-insurance.",
    "Use solar-system specific triggers: debris/route for Helionis, radiation windows for Bayesia, orbital-shear triggers for Oryn Delta.",
    "Review pricing quarterly with telemetry-based hazard indices and refresh stress/capital metrics."
  ),
  rationale = c(
    "Gold+platinum dominate severity concentration; exclusion stabilizes capital volatility.",
    "Maintains client flexibility without forcing portfolio-wide risk transfer of extreme tails.",
    "Aligns product design directly with ENC system characteristics and case-study requirements.",
    "Supports scalability/adaptability objective and keeps model aligned with changing hazards."
  ),
  implementation_priority = c("High", "High", "High", "Medium")
)

write.csv(
  recommendations,
  "CL_outputs/cargo_no_gold_platinum_final_recommendations.csv",
  row.names = FALSE
)

cat("\nNo-gold/platinum cargo pricing workflow complete. Outputs written to CL_outputs/ with prefix cargo_no_gold_platinum_\n")
