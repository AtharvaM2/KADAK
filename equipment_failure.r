library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(stringr)
library(gridExtra)
library(scales)
library(tidyr)
library(MASS)
library(pscl)
library(VineCopula)
library(ggridges)
library(GGally)
library(evir)


##### PART ONE: EDA ############################################################
##### Data Setup ###############################################################
# ── Load ──────────────────────────────────────────────────────────────────────
equip_freq <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = 1)
equip_sev  <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = 2)

# ── Clean ─────────────────────────────────────────────────────────────────────

# ── 2a. Strip corrupted suffixes from categorical fields ──
clean_category <- function(x) {
  trimws(sub("_\\?\\?\\?[0-9]+.*$", "", x))
}

equip_freq$equipment_type <- clean_category(equip_freq$equipment_type)
equip_freq$solar_system   <- clean_category(equip_freq$solar_system)
equip_sev$equipment_type  <- clean_category(equip_sev$equipment_type)
equip_sev$solar_system    <- clean_category(equip_sev$solar_system)

# Normalise known mis-spellings
equip_freq$equipment_type <- dplyr::recode(equip_freq$equipment_type,
                                           "FexStram Carrier" = "FluxStream Carrier",
                                           "Flux Rider"       = "Fusion Transport",
                                           "ReglAggregators"  = "Mag-Lift Aggregator"
)
equip_sev$equipment_type <- dplyr::recode(equip_sev$equipment_type,
                                          "FexStram Carrier" = "FluxStream Carrier",
                                          "Flux Rider"       = "Fusion Transport",
                                          "ReglAggregators"  = "Mag-Lift Aggregator"
)

# Retain only valid solar systems
valid_systems <- c("Helionis Cluster", "Epsilon", "Zeta")
equip_freq <- equip_freq %>% filter(solar_system %in% valid_systems)
equip_sev  <- equip_sev  %>% filter(solar_system %in% valid_systems)

# ── 2b. Numeric range enforcement ────────────────────────────────────────────
equip_freq <- equip_freq %>%
  filter(
    equipment_age   >= 0   & equipment_age   <= 10,
    maintenance_int >= 100 & maintenance_int <= 5000,
    usage_int       >= 0   & usage_int       <= 24,
    exposure        >  0   & exposure        <= 1,
    claim_count     >= 0   & claim_count     <= 3
  ) %>%
  mutate(
    claim_count    = as.integer(round(claim_count)),
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system)
  )

equip_sev <- equip_sev %>%
  filter(
    equipment_age   >= 0     & equipment_age   <= 10,
    maintenance_int >= 100   & maintenance_int <= 5000,
    usage_int       >= 0     & usage_int       <= 24,
    exposure        >= 0     & exposure        <= 1,
    claim_amount    >= 11000 & claim_amount    <= 790000
  ) %>%
  mutate(
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system)
  )

cat("Frequency dataset rows after cleaning:", nrow(equip_freq), "\n")
cat("Severity dataset rows after cleaning:",  nrow(equip_sev),  "\n")

# NOTE: NAs are NOT dropped from the datasets here.
# Each plot filters them out locally so no NA bar/facet appears in any graph.

# ── Shared pastel palettes ────────────────────────────────────────────────────
# Solar system: soft peach / mint / baby pink (named for reliable mapping)
pal_system <- c(
  "Epsilon"          = "#FFCBA4",   # warm peach
  "Helionis Cluster" = "#A8D8C8",   # soft mint
  "Zeta"             = "#F9C6CF"    # baby pink
)

# Scatter/line colours: slightly deeper so they read on white background
pal_system_dark <- c(
  "Epsilon"          = "#E8885A",
  "Helionis Cluster" = "#5FB89A",
  "Zeta"             = "#D97AA6"
)

# Equipment type: 6 muted, distinct pastels
equip_types_sorted <- sort(unique(equip_sev$equipment_type))
pal_equip <- setNames(
  c("#FFCBA4", "#A8D8C8", "#F9C6CF", "#BFD7ED", "#D4C5E2", "#C8E6B0"),
  equip_types_sorted
)


##### EDA ######################################################################
# ── 1. Distribution of Claim Amount ──────────────────────────────────────────
p95 <- quantile(equip_sev$claim_amount, 0.95)

p1 <- ggplot(equip_sev, aes(x = claim_amount)) +
  geom_histogram(binwidth = 5000, fill = "#FFCBA4", color = "white", alpha = 0.9) +
  geom_density(aes(y = after_stat(count) * 5000), color = "#c95b1a", linewidth = 1) +
  coord_cartesian(xlim = c(0, p95)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = "Distribution of Claim Amount (95th percentile view)",
    subtitle = paste0("X-axis clipped at 95th pct (", scales::comma(round(p95)), ")"),
    x = "Claim Amount", y = "Count"
  ) +
  theme_minimal()

p1_log <- ggplot(equip_sev, aes(x = log(claim_amount))) +
  geom_histogram(binwidth = 0.2, fill = "#FFCBA4", color = "white", alpha = 0.9) +
  geom_density(aes(y = after_stat(count) * 0.2), color = "#c95b1a", linewidth = 1) +
  labs(title = "Distribution of Log Claim Amount", x = "Log(Claim Amount)", y = "Count") +
  theme_minimal()

grid.arrange(p1, p1_log, ncol = 2)


# ── 2. Equipment Type vs Solar System ────────────────────────────────────────
ggplot(equip_sev |> filter(!is.na(equipment_type), !is.na(solar_system)),
       aes(x = equipment_type, fill = solar_system)) +
  geom_bar(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Number of Equipment Claims by Type and Solar System",
       x = "Equipment Type", y = "Count", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())


# ── 3. Frequency of Claims by Equipment Type and Solar System ─────────────────
claims_summary <- equip_freq |>
  filter(!is.na(equipment_type), !is.na(solar_system)) |>
  group_by(equipment_type, solar_system) |>
  summarise(total_claims = sum(claim_count, na.rm = TRUE), .groups = "drop")

ggplot(claims_summary, aes(x = equipment_type, y = total_claims, fill = solar_system)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Frequency of Claims by Equipment Type and Solar System",
       x = "Equipment Type", y = "Total Claim Frequency", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())


# ── 4. Exposure-Adjusted Claim Rate ──────────────────────────────────────────
claim_rate_summary <- equip_freq |>
  filter(!is.na(equipment_type), !is.na(solar_system)) |>
  group_by(equipment_type, solar_system) |>
  summarise(
    total_claims   = sum(claim_count, na.rm = TRUE),
    total_exposure = sum(exposure,    na.rm = TRUE),
    claim_rate     = total_claims / total_exposure,
    .groups = "drop"
  )

ggplot(claim_rate_summary, aes(x = equipment_type, y = claim_rate, fill = solar_system)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Claim Frequency Rate (Exposure Adjusted)",
       x = "Equipment Type", y = "Claims per Exposure Unit", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ── 5. Average Claim Amount by Equipment Type ─────────────────────────────────
equip_summary <- equip_sev |>
  filter(!is.na(equipment_type)) |>
  group_by(equipment_type) |>
  summarise(mean_claim = mean(claim_amount, na.rm = TRUE))

ggplot(equip_summary,
       aes(x = fct_reorder(equipment_type, mean_claim), y = mean_claim,
           fill = equipment_type)) +
  geom_col() +
  scale_fill_manual(values = pal_equip) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Amount by Equipment Type",
       x = "Equipment Type", y = "Mean Claim Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# ── 6. Claim Frequency Rate by Equipment Type ─────────────────────────────────
equip_freq_rate <- equip_freq |>
  filter(!is.na(equipment_type)) |>
  group_by(equipment_type) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(equip_freq_rate |> filter(!is.na(equipment_type)),
       aes(x = fct_reorder(equipment_type, claim_rate), y = claim_rate,
           fill = equipment_type)) +
  geom_col() +
  scale_fill_manual(values = pal_equip) +
  labs(title = "Claim Frequency Rate by Equipment Type",
       x = "Equipment Type", y = "Claims per Exposure Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# ── 7. Equipment Age vs Claim Frequency (banded) ─────────────────────────────
equip_freq <- equip_freq |>
  mutate(age_band = cut(
    equipment_age,
    breaks = seq(0, 10, by = 2),
    labels = c("0-2 yrs", "2-4 yrs", "4-6 yrs", "6-8 yrs", "8-10 yrs"),
    include.lowest = TRUE
  ))

age_freq <- equip_freq |>
  filter(!is.na(age_band)) |>
  group_by(age_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(age_freq, aes(x = age_band, y = claim_rate, group = 1)) +
  geom_line(colour = "#E8885A", linewidth = 1.2) +
  geom_point(colour = "#E8885A", size = 3.5, fill = "#FFCBA4", shape = 21, stroke = 1.5) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Claim Frequency Rate by Equipment Age",
       x = "Equipment Age", y = "Claims per Exposure Year") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


# ── 8. Equipment Age vs Claim Severity (banded) ───────────────────────────────
equip_sev <- equip_sev |>
  mutate(age_band = cut(
    equipment_age,
    breaks = seq(0, 10, by = 2),
    labels = c("0-2 yrs", "2-4 yrs", "4-6 yrs", "6-8 yrs", "8-10 yrs"),
    include.lowest = TRUE
  ))

age_sev <- equip_sev |>
  filter(!is.na(age_band)) |>
  group_by(age_band) |>
  summarise(mean_severity = mean(claim_amount, na.rm = TRUE), .groups = "drop")

ggplot(age_sev, aes(x = age_band, y = mean_severity, group = 1)) +
  geom_line(colour = "#D97AA6", linewidth = 1.2) +
  geom_point(colour = "#D97AA6", size = 3.5, fill = "#F9C6CF", shape = 21, stroke = 1.5) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Average Claim Severity by Equipment Age",
       x = "Equipment Age", y = "Average Claim Amount") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


# ── 9. Maintenance Interval vs Claim Frequency ────────────────────────────────
equip_freq <- equip_freq |>
  mutate(maint_band = cut(
    maintenance_int,
    breaks = c(100, 500, 1000, 1500, 2000),
    labels = c("100-500", "500-1000", "1000-1500", "1500-2000"),
    include.lowest = TRUE, right = TRUE
  ))

maint_freq <- equip_freq |>
  filter(!is.na(maint_band)) |>
  group_by(maint_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(maint_freq, aes(x = maint_band, y = claim_rate)) +
  geom_col(fill = "#FFCBA4", width = 0.65) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Claim Frequency Rate by Maintenance Interval Band",
       x = "Maintenance Interval (Earth Hours)", y = "Claims per Exposure Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ── 10. Maintenance Interval vs Claim Severity ────────────────────────────────
equip_sev <- equip_sev |>
  mutate(maint_band = cut(
    maintenance_int,
    breaks = c(100, 500, 1000, 1500, 2000, 5000),
    labels = c("100-500", "500-1000", "1000-1500", "1500-2000", "2000-5000"),
    include.lowest = TRUE, right = TRUE
  ))

maint_sev <- equip_sev |>
  filter(!is.na(maint_band)) |>
  group_by(maint_band) |>
  summarise(
    mean_severity = mean(claim_amount, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(maint_band))

ggplot(maint_sev, aes(x = maint_band, y = mean_severity)) +
  geom_col(fill = "#F9C6CF", width = 0.65) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Average Claim Severity by Maintenance Interval Band",
       x = "Maintenance Interval (Earth Hours)", y = "Average Claim Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ── 11. Usage Intensity vs Claim Frequency (banded) ──────────────────────────
# Bands reflect operational duty cycles:
#   Low  =  0–8 hrs/day  (single shift)
#   Mid  = 8–16 hrs/day  (double shift)
#   High = 16–24 hrs/day (near-continuous)
equip_freq <- equip_freq |>
  mutate(usage_band = cut(
    usage_int,
    breaks = c(0, 8, 16, 24),
    labels = c("Low (0-8 hrs)", "Mid (8-16 hrs)", "High (16-24 hrs)"),
    include.lowest = TRUE
  ))

usage_freq <- equip_freq |>
  filter(!is.na(usage_band)) |>
  group_by(usage_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(usage_freq, aes(x = usage_band, y = claim_rate)) +
  geom_col(fill = "#A8D8C8", width = 0.65) +
  labs(title = "Claim Frequency Rate by Daily Usage Intensity Band",
       x = "Usage Intensity", y = "Claims per Exposure Year") +
  theme_minimal()


# ── 12. Usage Intensity vs Claim Severity (banded) ───────────────────────────
equip_sev <- equip_sev |>
  mutate(usage_band = cut(
    usage_int,
    breaks = c(0, 8, 16, 24),
    labels = c("Low (0-8 hrs)", "Mid (8-16 hrs)", "High (16-24 hrs)"),
    include.lowest = TRUE
  ))

usage_sev <- equip_sev |>
  filter(!is.na(usage_band)) |>
  group_by(usage_band) |>
  summarise(mean_severity = mean(claim_amount, na.rm = TRUE), .groups = "drop")

ggplot(usage_sev, aes(x = usage_band, y = mean_severity)) +
  geom_col(fill = "#D4C5E2", width = 0.65) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Daily Usage Intensity Band",
       x = "Usage Intensity", y = "Average Claim Amount") +
  theme_minimal()


# ── 13. Heatmap: Equipment Type vs Solar System ───────────────────────────────
heatmap_counts <- equip_sev |>
  filter(!is.na(equipment_type), !is.na(solar_system)) |>
  count(equipment_type, solar_system)

ggplot(heatmap_counts,
       aes(x = fct_reorder(equipment_type, n, .fun = sum),
           y = fct_reorder(solar_system,   n, .fun = sum),
           fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#E8F5F0", high = "#c95b1a") +
  labs(title = "Heatmap: Equipment Type vs Solar System",
       x = "Equipment Type", y = "Solar System", fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ── 14. Equipment Type Distribution Across Solar Systems ──────────────────────
# Per-system hue families (pastel), shades darken across equipment types
system_hues <- list(
  "Epsilon"          = c("#FFE8D6","#FFD4B5","#FFBE90","#FFAA6E","#F08E52","#D4703A"),
  "Helionis Cluster" = c("#D6EFE6","#B8E2D4","#96D4C0","#72C5AB","#4EB596","#2EA480"),
  "Zeta"             = c("#FCDDE8","#FAC4D8","#F8ABC6","#F490B3","#E8759E","#D45A88")
)

solar_systems_14 <- sort(unique(equip_sev$solar_system))
equip_types_14   <- sort(unique(equip_sev$equipment_type))
n_types_14       <- length(equip_types_14)

colour_vec <- unlist(lapply(solar_systems_14, function(ss) {
  pal    <- system_hues[[ss]]
  shades <- pal[seq_len(min(n_types_14, length(pal)))]
  setNames(shades, paste0(ss, "__", equip_types_14[seq_len(length(shades))]))
}))

equip_sev_plot <- equip_sev |>
  filter(!is.na(equipment_type), !is.na(solar_system)) |>
  mutate(fill_key = paste0(solar_system, "__", equipment_type))

ggplot(equip_sev_plot,
       aes(x = fct_infreq(equipment_type), fill = fill_key)) +
  geom_bar(color = "white", linewidth = 0.2) +
  facet_wrap(~ solar_system) +
  scale_fill_manual(values = colour_vec) +
  labs(title = "Equipment Type Distribution Across Solar Systems",
       x = "Equipment Type", y = "Number of Claims") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold"),
    legend.position    = "none"
  )

# ── 15. Correlation heatmap ───────────────────────────────────────────────────
num_vars <- equip_sev |>
  dplyr::select(claim_amount, equipment_age, maintenance_int, usage_int, exposure) |>
  cor(use = "complete.obs")

cor_long <- as.data.frame(as.table(num_vars)) |>
  rename(Correlation = Freq)

ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 4) +
  scale_fill_gradient2(low = "#A8D8C8", mid = "white", high = "#F9C6CF",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap of Continuous Variables", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ── 16. Boxplot: Log Claim Amount by Equipment Type ───────────────────────────
ggplot(equip_sev |> filter(!is.na(equipment_type)),
       aes(x    = fct_reorder(equipment_type, claim_amount, median),
           y    = log(claim_amount),
           fill = equipment_type)) +
  geom_boxplot(alpha = 0.85, outlier.size = 1.2, outlier.alpha = 0.4) +
  scale_fill_manual(values = pal_equip) +
  labs(title = "Log Claim Amount Distribution by Equipment Type",
       x = "Equipment Type", y = "Log(Claim Amount)") +
  theme_minimal() +
  theme(axis.text.x   = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# ── 17. Ridge plot: Log Claim Amount by Equipment Type ────────────────────────
ggplot(equip_sev |> filter(!is.na(equipment_type)),
       aes(x    = log(claim_amount),
           y    = fct_reorder(equipment_type, claim_amount, median),
           fill = equipment_type)) +
  geom_density_ridges(alpha = 0.78, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = pal_equip) +
  labs(title = "Claim Amount Distribution by Equipment Type (Ridge Plot)",
       x = "Log(Claim Amount)", y = "Equipment Type") +
  theme_minimal() +
  theme(legend.position = "none")

# ── 18. Ridge plot: Claim Amount by Equipment Type ────────────────────────────
ggplot(equip_sev |> filter(!is.na(equipment_type)),
       aes(x    = claim_amount,
           y    = fct_reorder(equipment_type, claim_amount, median),
           fill = equipment_type)) +
  geom_density_ridges(alpha = 0.78, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = pal_equip) +
  labs(title = "Claim Amount Distribution by Equipment Type (Ridge Plot)",
       x = "Claim Amount", y = "Equipment Type") +
  theme_minimal() +
  theme(legend.position = "none")


# ── 19. Ridge plot: Claim Amount by Solar System ──────────────────────────────
ggplot(equip_sev |> filter(!is.na(solar_system)),
       aes(x    = log(claim_amount),
           y    = fct_reorder(solar_system, claim_amount, median),
           fill = solar_system)) +
  geom_density_ridges(alpha = 0.78, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Claim Amount Distribution by Solar System (Ridge Plot)",
       x = "Log(Claim Amount)", y = "Solar System") +
  theme_minimal() +
  theme(legend.position = "none")


# ── 20. Ridge plot: Claim Amount by Solar System ──────────────────────────────
ggplot(equip_sev |> filter(!is.na(solar_system)),
       aes(x    = claim_amount,
           y    = fct_reorder(solar_system, claim_amount, median),
           fill = solar_system)) +
  geom_density_ridges(alpha = 0.78, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Claim Amount Distribution by Solar System (Ridge Plot)",
       x = "Claim Amount", y = "Solar System") +
  theme_minimal() +
  theme(legend.position = "none")




##### PART TWO: MODELLING ######################################################
##### Data Setup ###############################################################
 #### Frequency ###
eq_freq_raw <- readxl::read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = "freq")
eq_sev_raw  <- readxl::read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = "sev")

eq_freq <- eq_freq_raw

# ── 2a. Strip corrupted suffixes from categorical fields ──
clean_category <- function(x) {
  trimws(sub("_\\?\\?\\?[0-9]+.*$", "", x))
}

eq_freq$equipment_type <- clean_category(eq_freq$equipment_type)
eq_freq$solar_system   <- clean_category(eq_freq$solar_system)

eq_freq$equipment_type <- dplyr::recode(eq_freq$equipment_type,
                                        "FexStram Carrier"  = "FluxStream Carrier",
                                        "Flux Rider"        = "Fusion Transport",
                                        "ReglAggregators"   = "Mag-Lift Aggregator"
)

# Retain only the three valid solar systems
valid_systems <- c("Helionis Cluster", "Epsilon", "Zeta")
eq_freq <- eq_freq %>% filter(solar_system %in% valid_systems)

# ── 2b. Numeric range enforcement (per data dictionary) ──
eq_freq <- eq_freq %>%
  filter(
    equipment_age  >= 0  & equipment_age  <= 10,
    maintenance_int >= 100 & maintenance_int <= 5000,
    usage_int      >= 0  & usage_int      <= 24,
    exposure       >  0  & exposure       <= 1,
    claim_count    >= 0  & claim_count    <= 3
  ) %>%
  mutate(
    claim_count = as.integer(round(claim_count)),
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system)
  )

cat("Frequency dataset rows after cleaning:", nrow(eq_freq), "\n")

 #### Severity ###
eq_sev <- eq_sev_raw

eq_sev$equipment_type <- clean_category(eq_sev$equipment_type)
eq_sev$solar_system   <- clean_category(eq_sev$solar_system)

eq_sev$equipment_type <- dplyr::recode(eq_sev$equipment_type,
                                       "FexStram Carrier"  = "FluxStream Carrier",
                                       "Flux Rider"        = "Fusion Transport",
                                       "ReglAggregators"   = "Mag-Lift Aggregator"
)

cat("Severity dataset rows after cleaning:", nrow(eq_sev), "\n")

eq_sev <- eq_sev %>%
  filter(solar_system %in% valid_systems) %>%
  filter(
    equipment_age   >= 0     & equipment_age   <= 10,
    maintenance_int >= 100   & maintenance_int <= 5000,
    usage_int       >= 0     & usage_int       <= 24,
    exposure        >= 0     & exposure        <= 1,
    claim_amount    >= 11000 & claim_amount    <= 790000
  ) %>%
  mutate(
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system)
  )

cat("Severity dataset rows after cleaning:", nrow(eq_sev), "\n")

 ### Exploratory Analysis ###
# Zero-claim proportion
zero_pct <- mean(eq_freq$claim_count == 0) * 100
cat(sprintf("Zero-claim records: %.1f%%\n", zero_pct))

# Claim count distribution
table(eq_freq$claim_count)

# Average claim by equipment type
eq_sev %>%
  group_by(equipment_type) %>%
  summarise(mean_claim = mean(claim_amount),
            median_claim = median(claim_amount),
            n = n()) %>%
  arrange(desc(mean_claim))


##### Model Datasets: Predictors ###############################################
freq_predictors <- c("equipment_type", "equipment_age", "solar_system",
                     "maintenance_int", "usage_int", "exposure")

model_eq_freq <- eq_freq %>%
  dplyr::select(all_of(c(freq_predictors, "claim_count"))) %>%
  drop_na() %>%
  filter(exposure > 0)
freq_numeric <- c("equipment_age", "maintenance_int", "usage_int")
model_eq_freq[freq_numeric] <- scale(model_eq_freq[freq_numeric])
model_eq_freq$log_exposure <- log(model_eq_freq$exposure)

model_eq_sev <- eq_sev %>%
  dplyr::select(equipment_type, equipment_age, solar_system,
                maintenance_int, usage_int, exposure, claim_amount) %>%
  drop_na()

#### Frequency Modelling #######################################################
# ── 6a. Stepwise variable selection on Poisson baseline ──
null_freq <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_eq_freq
)

full_freq <- glm(
  claim_count ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_eq_freq
)
summary(full_freq)

step_back <- stepAIC(full_freq, direction = "backward", trace = FALSE)
summary(step_back)

step_fwd <- stepAIC(
  null_freq,
  scope = list(
    lower = ~1,
    upper = ~ equipment_type + equipment_age + solar_system +
      maintenance_int + usage_int + offset(log(exposure))
  ),
  direction = "forward",
  trace = FALSE
)
summary(step_fwd)

AIC(full_freq, step_back, step_fwd)

# ── 6b. Poisson GLM (leading frequency model) ──
pois_freq <- glm(
  claim_count ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_eq_freq
)
summary(pois_freq)

# ── 6c. Zero-Inflated Poisson (robustness check) ──
zip_freq <- zeroinfl(
  claim_count ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int + offset(log_exposure) |
    equipment_type,
  data = model_eq_freq,
  dist = "poisson"
)
summary(zip_freq)

AIC(pois_freq, zip_freq)

#### Severity Modelling ########################################################
# Scale numeric predictors for numerical stability
sev_numeric <- c("equipment_age", "maintenance_int", "usage_int", "exposure")
model_eq_sev[sev_numeric] <- scale(model_eq_sev[sev_numeric])

gamma_sev <- glm(
  claim_amount ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int,
  family = Gamma(link = "log"),
  data   = model_eq_sev
)
summary(gamma_sev)

lognormal_sev <- glm(
  log(claim_amount) ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int,
  family = gaussian(link = "identity"),
  data   = model_eq_sev
)
summary(lognormal_sev)

AIC(gamma_sev)
AIC(lognormal_sev)

#### Monte Carlo Simulation: Aggregate Losses ##################################
set.seed(2026)
n_sim <- 10000

lambda_hat <- predict(pois_freq, type = "response")
n_pol      <- length(lambda_hat)

# ── Severity inputs from Gamma GLM ──
sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(eq_sev$claim_amount) / sev_shape   # uses unscaled original mean

# ── Simulation loop ──
# For a Poisson frequency model we draw rpois() directly — no theta needed.
aggregate_loss <- numeric(n_sim)

for (s in 1:n_sim) {
  
  freq_draw    <- rpois(n_pol, lambda = lambda_hat)
  total_claims <- sum(freq_draw)
  
  if (total_claims > 0) {
    aggregate_loss[s] <- sum(rgamma(total_claims,
                                    shape = sev_shape,
                                    scale = sev_scale))
  }
}

# ── Risk Metrics ──
EF_mean_loss <- mean(aggregate_loss)
EF_sd_loss   <- sd(aggregate_loss)
EF_VaR_99    <- quantile(aggregate_loss, 0.99)
EF_TVaR_99   <- mean(aggregate_loss[aggregate_loss > EF_VaR_99])

cat("\n--- Equipment Failure Risk Metrics ---\n")
cat(sprintf("Mean Loss  : %.0f\n", EF_mean_loss))
cat(sprintf("SD Loss    : %.0f\n", EF_sd_loss))
cat(sprintf("VaR  99%%   : %.0f\n", EF_VaR_99))
cat(sprintf("TVaR 99%%   : %.0f\n", EF_TVaR_99))

#### Tail Behaviour ############################################################
# ── 9a. Empirical tail quantiles ──
tail_quantiles <- quantile(eq_sev$claim_amount,
                           probs = c(0.90, 0.95, 0.975, 0.99, 0.995, 1.00))
print(round(tail_quantiles))

# ── 9b. Log-Normal tail fit (MLE) ──
lnorm_fit <- fitdistr(eq_sev$claim_amount, "lognormal")
print(lnorm_fit)

lnorm_tail <- data.frame(
  quantile  = c(0.90, 0.95, 0.99),
  empirical = quantile(eq_sev$claim_amount, c(0.90, 0.95, 0.99)),
  lognormal = qlnorm(c(0.90, 0.95, 0.99),
                     meanlog = lnorm_fit$estimate["meanlog"],
                     sdlog   = lnorm_fit$estimate["sdlog"])
)
print(round(lnorm_tail))

# ── 9c. Gamma tail fit (MLE) ──
gamma_start <- list(shape = (mean(eq_sev$claim_amount) / sd(eq_sev$claim_amount))^2,
                    scale =  sd(eq_sev$claim_amount)^2  / mean(eq_sev$claim_amount))
gamma_fit <- fitdistr(eq_sev$claim_amount, "gamma", start = gamma_start)
print(gamma_fit)

gamma_tail <- data.frame(
  quantile  = c(0.90, 0.95, 0.99),
  empirical = quantile(eq_sev$claim_amount, c(0.90, 0.95, 0.99)),
  gamma     = qgamma(c(0.90, 0.95, 0.99),
                     shape = gamma_fit$estimate["shape"],
                     scale = gamma_fit$estimate["scale"])
)
print(round(gamma_tail))

# ── 9d. AIC comparison ──
x <- eq_sev$claim_amount

aic_lnorm <- -2 * sum(dlnorm(x,
                             meanlog = lnorm_fit$estimate["meanlog"],
                             sdlog   = lnorm_fit$estimate["sdlog"],
                             log = TRUE)) + 2 * 2
aic_gamma <- -2 * sum(dgamma(x,
                             shape = gamma_fit$estimate["shape"],
                             scale = gamma_fit$estimate["scale"],
                             log = TRUE)) + 2 * 2

cat(sprintf("AIC log-Normal : %.1f\n", aic_lnorm))
cat(sprintf("AIC Gamma      : %.1f\n", aic_gamma))
cat(sprintf("Preferred model: %s\n",
            ifelse(aic_lnorm < aic_gamma, "Log-Normal", "Gamma")))

# ── 9e. Mean excess plot (visual diagnostic) ──
meplot(eq_sev$claim_amount,
       main = "Mean Excess Plot – Equipment Failure (bounded at 790K)")

#### Tail Dependence: Copula ###################################################
freq_raw <- model_eq_freq$claim_count
freq_pos <- freq_raw[freq_raw > 0]
sev_raw  <- eq_sev$claim_amount

n_dep <- min(length(freq_pos), length(sev_raw))

dep_df <- data.frame(
  f = freq_pos[1:n_dep],
  s = sev_raw[1:n_dep]
)

# ── Pseudo-observations ──
set.seed(2026)
f_jitter <- dep_df$f + runif(n_dep, -0.5, 0.5)

u_dep <- rank(f_jitter) / (n_dep + 1)
v_dep <- rank(dep_df$s) / (n_dep + 1)

# Automatic copula family selection — no degenerate quadrants after jittering
fit_cop <- BiCopSelect(u_dep, v_dep, familyset = NA)
summary(fit_cop)

# ── Copula-based simulation ──
set.seed(2026)
n_sim_cop <- 500000

cop_sim  <- BiCopSim(n_sim_cop, fit_cop$family, fit_cop$par, fit_cop$par2)
u_sim    <- cop_sim[, 1]
v_sim    <- cop_sim[, 2]

# Map uniforms to marginals
# Poisson is the selected frequency model; use qpois() for the quantile transform.
lambda_all   <- predict(pois_freq, type = "response")

freq_cop_sim <- qpois(u_sim,
                      lambda = sample(lambda_all, n_sim_cop, replace = TRUE))

agg_cop <- vapply(freq_cop_sim, function(n_claims) {
  if (n_claims > 0L) sum(rgamma(n_claims, shape = sev_shape, scale = sev_scale))
  else 0
}, numeric(1))

EF_VaR_cop   <- quantile(agg_cop, 0.99)
EF_TVaR_cop  <- mean(agg_cop[agg_cop > EF_VaR_cop])

cat("\n--- Copula-Based Risk Metrics ---\n")
cat(sprintf("VaR  99%%  : %.0f\n", EF_VaR_cop))
cat(sprintf("TVaR 99%%  : %.0f\n", EF_TVaR_cop))

# Extreme quantiles
cat(sprintf("VaR  99.5%% (Solvency)     : %.0f\n", quantile(agg_cop, 0.995)))
cat(sprintf("VaR  99.9%% (Catastrophic) : %.0f\n", quantile(agg_cop, 0.999)))

# ── Copula family comparison plots ──
par(mfrow = c(2, 2))

fit_gauss <- BiCopEst(u_dep, v_dep, family = 1)
sim_gauss <- BiCopSim(5000, 1, fit_gauss$par)
plot(sim_gauss, main = "Gaussian Copula",
     xlab = "Frequency", ylab = "Severity", pch = 20, col = rgb(0, 0, 1, 0.2))

fit_clay <- BiCopEst(u_dep, v_dep, family = 3)
sim_clay <- BiCopSim(5000, 3, fit_clay$par)
plot(sim_clay, main = "Clayton Copula",
     xlab = "Frequency", ylab = "Severity", pch = 20, col = rgb(1, 0, 0, 0.2))

fit_gumb <- BiCopEst(u_dep, v_dep, family = 4)
sim_gumb <- BiCopSim(5000, 4, fit_gumb$par)
plot(sim_gumb, main = "Gumbel Copula",
     xlab = "Frequency", ylab = "Severity", pch = 20, col = rgb(0, 0.6, 0, 0.2))

fit_t <- BiCopEst(u_dep, v_dep, family = 2)
sim_t <- BiCopSim(5000, 2, fit_t$par, fit_t$par2)
plot(sim_t, main = "t-Copula",
     xlab = "Frequency", ylab = "Severity", pch = 20, col = rgb(0.5, 0, 0.5, 0.2))

par(mfrow = c(1, 1))

#### Segment Analysis by Equipment Type ########################################
# High-wear segment: Ion Pulverizer & Quantum Bore (high-energy, high-failure risk)
high_wear_types <- c("Ion Pulverizer", "Quantum Bore")

freq_hw   <- model_eq_freq %>% filter(equipment_type %in% high_wear_types)
sev_hw    <- model_eq_sev  %>% filter(equipment_type %in% high_wear_types)
freq_std  <- model_eq_freq %>% filter(!(equipment_type %in% high_wear_types))
sev_std   <- model_eq_sev  %>% filter(!(equipment_type %in% high_wear_types))

# Severity sub-models
sev_model_hw <- glm(claim_amount ~ equipment_age + solar_system +
                      maintenance_int + usage_int,
                    family = Gamma(link = "log"), data = sev_hw)
sev_model_std <- glm(claim_amount ~ equipment_age + solar_system +
                       maintenance_int + usage_int,
                     family = Gamma(link = "log"), data = sev_std)

shape_hw  <- 1 / summary(sev_model_hw)$dispersion
scale_hw  <- mean(eq_sev$claim_amount[eq_sev$equipment_type %in% high_wear_types]) / shape_hw

shape_std <- 1 / summary(sev_model_std)$dispersion
scale_std <- mean(eq_sev$claim_amount[!(eq_sev$equipment_type %in% high_wear_types)]) / shape_std

set.seed(2026)
n_seg <- 50000
agg_hw  <- numeric(n_seg)
agg_std <- numeric(n_seg)

mu_hw  <- mean(predict(pois_freq, newdata = freq_hw,  type = "response"))
mu_std <- mean(predict(pois_freq, newdata = freq_std, type = "response"))

for (s in 1:n_seg) {
  
  f_hw  <- rpois(1, lambda = mu_hw  * nrow(freq_hw))
  f_std <- rpois(1, lambda = mu_std * nrow(freq_std))
  
  if (f_hw  > 0) agg_hw[s]  <- sum(rgamma(f_hw,  shape = shape_hw,  scale = scale_hw))
  if (f_std > 0) agg_std[s] <- sum(rgamma(f_std, shape = shape_std, scale = scale_std))
}

segment_results <- data.frame(
  Metric        = c("Mean Loss", "VaR 99%", "TVaR 99%"),
  High_Wear     = c(mean(agg_hw),
                    quantile(agg_hw, 0.99),
                    mean(agg_hw[agg_hw > quantile(agg_hw, 0.99)])),
  Standard      = c(mean(agg_std),
                    quantile(agg_std, 0.99),
                    mean(agg_std[agg_std > quantile(agg_std, 0.99)]))
)
print(segment_results)

#### Stress Testing ############################################################
# Helper: simulate aggregate losses given a frequency vector and Gamma params
sim_agg <- function(freq_vec, sh, sc) {
  vapply(freq_vec, function(n) {
    if (n > 0L) sum(rgamma(n, shape = sh, scale = sc)) else 0
  }, numeric(1))
}

# Helper: compute risk metrics from a loss vector, returned as a one-row data frame
risk_metrics <- function(losses) {
  var99 <- as.numeric(quantile(losses, 0.99))
  data.frame(
    Mean_Loss = round(mean(losses)),
    VaR_99    = round(var99),
    TVaR_99   = round(mean(losses[losses > var99]))
  )
}

set.seed(2026)
base_freq <- rpois(n_sim, lambda = mean(lambda_hat))
loss_base <- sim_agg(base_freq, sev_shape, sev_scale)

# ── Sensitivity grid: ±10% on each assumption independently ──
shocks <- c(-0.50, -0.40, -0.30, -0.20, -0.10, 0.10, 0.20, 0.30, 0.40, 0.50)

# Assumption 1: Claim frequency (λ)
freq_sens <- lapply(shocks, function(s) {
  f_stressed <- rpois(n_sim, lambda = mean(lambda_hat) * (1 + s))
  cbind(data.frame(Assumption = "Frequency (lambda)",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(f_stressed, sev_shape, sev_scale)))
})

# Assumption 2: Severity scale (mean claim size)
sev_sens <- lapply(shocks, function(s) {
  cbind(data.frame(Assumption = "Severity (scale)",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(base_freq, sev_shape, sev_scale * (1 + s))))
})

# Assumption 3: Maintenance interval (longer interval → higher frequency)
maint_sens <- lapply(shocks, function(s) {
  f_stressed <- rpois(n_sim, lambda = mean(lambda_hat) * (1 + s))
  cbind(data.frame(Assumption = "Maintenance Interval",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(f_stressed, sev_shape, sev_scale)))
})

# Assumption 4: Usage intensity (more hours/day → higher frequency and severity)
usage_sens <- lapply(shocks, function(s) {
  f_stressed  <- rpois(n_sim, lambda = mean(lambda_hat) * (1 + s))
  sc_stressed <- sev_scale * (1 + s * 0.5)
  cbind(data.frame(Assumption = "Usage Intensity",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(f_stressed, sev_shape, sc_stressed)))
})

# Assumption 5: Equipment age (older fleet → higher frequency and severity)
age_sens <- lapply(shocks, function(s) {
  f_stressed  <- rpois(n_sim, lambda = mean(lambda_hat) * (1 + s))
  sc_stressed <- sev_scale * (1 + s * 0.5)
  cbind(data.frame(Assumption = "Equipment Age",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(f_stressed, sev_shape, sc_stressed)))
})

# ── Combine into single sensitivity table ──
baseline_row <- cbind(
  data.frame(Assumption = "Baseline", Shock = "0%"),
  risk_metrics(loss_base)
)

sensitivity_table <- rbind(
  baseline_row,
  do.call(rbind, freq_sens),
  do.call(rbind, sev_sens),
  do.call(rbind, maint_sens),
  do.call(rbind, usage_sens),
  do.call(rbind, age_sens)
)
rownames(sensitivity_table) <- NULL
print(sensitivity_table)

# ── Tornado chart: rank assumptions by VaR impact ──
tornado_data <- sensitivity_table %>%
  filter(Shock != "0%") %>%
  group_by(Assumption) %>%
  summarise(
    VaR_low  = min(VaR_99),
    VaR_high = max(VaR_99),
    Range    = VaR_high - VaR_low,
    .groups  = "drop"
  ) %>%
  arrange(Range)

baseline_VaR <- sensitivity_table$VaR_99[sensitivity_table$Shock == "0%"]

ggplot(tornado_data,
       aes(y = reorder(Assumption, Range))) +
  geom_segment(aes(x = VaR_low, xend = VaR_high,
                   yend = reorder(Assumption, Range)),
               linewidth = 6, colour = "#ffb07c", alpha = 0.7) +
  geom_vline(xintercept = baseline_VaR,
             linetype = "dashed", colour = "navy", linewidth = 0.8) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Tornado Chart – Sensitivity of VaR (99%) to ±10–50% Assumption Shocks",
       subtitle = "Dashed line = baseline VaR",
       x        = "VaR 99%",
       y        = NULL) +
  theme_minimal()

# ── Sensitivity line chart: VaR across full shock range per assumption ──
sens_plot_df <- sensitivity_table %>%
  filter(Shock != "0%") %>%
  mutate(Shock_num = as.numeric(sub("%", "", Shock)) / 100)

ggplot(sens_plot_df, aes(x = Shock_num, y = VaR_99,
                         colour = Assumption, group = Assumption)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = baseline_VaR,
             linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  scale_x_continuous(labels = scales::percent, breaks = seq(-0.5, 0.5, 0.1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "VaR (99%) Sensitivity – ±10% to ±50% Assumption Shocks",
       subtitle = "Dashed line = baseline VaR",
       x        = "Shock (%)",
       y        = "VaR 99%",
       colour   = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── Combined shock: all assumptions stressed simultaneously ──
loss_combined_up <- sim_agg(
  rpois(n_sim, lambda = mean(lambda_hat) * 1.10),
  sev_shape,
  sev_scale * 1.10
)
loss_combined_dn <- sim_agg(
  rpois(n_sim, lambda = mean(lambda_hat) * 0.90),
  sev_shape,
  sev_scale * 0.90
)

combined_table <- data.frame(
  Scenario  = c("All assumptions -10%", "Baseline", "All assumptions +10%"),
  Mean_Loss = c(mean(loss_combined_dn), mean(loss_base),    mean(loss_combined_up)),
  VaR_99    = c(quantile(loss_combined_dn, 0.99), quantile(loss_base, 0.99), quantile(loss_combined_up, 0.99)),
  TVaR_99   = c(
    mean(loss_combined_dn[loss_combined_dn > quantile(loss_combined_dn, 0.99)]),
    mean(loss_base[loss_base               > quantile(loss_base,        0.99)]),
    mean(loss_combined_up[loss_combined_up > quantile(loss_combined_up, 0.99)])
  )
)
combined_table[, -1] <- round(combined_table[, -1])
print(combined_table)

#### Multi-solar System Correlated Shock Model #################################

set.seed(2026)
n_shock <- 100000

system_params <- list(
  Helionis_Cluster = list(lambda = 0.10, shape = 2.0, scale = sev_scale * 1.0),
  Epsilon          = list(lambda = 0.14, shape = 1.8, scale = sev_scale * 1.2),
  Zeta             = list(lambda = 0.12, shape = 1.5, scale = sev_scale * 1.5)
)

run_system_sim <- function(freq_mult = 1, sev_mult = 1, apply_shock = FALSE) {
  
  shock <- if (apply_shock) {
    rbinom(n_shock, 1, 0.01) * rlnorm(n_shock, meanlog = 0, sdlog = 0.4)
  } else {
    rep(1, n_shock)
  }
  
  total <- numeric(n_shock)
  
  for (sys in names(system_params)) {
    p      <- system_params[[sys]]
    f_draw <- rpois(n_shock, p$lambda * freq_mult)
    
    if (apply_shock) f_draw <- f_draw + rbinom(n_shock, 1, 0.01) * rpois(n_shock, p$lambda * 4)
    
    sys_loss <- vapply(f_draw, function(n) {
      if (n > 0) sum(rgamma(n, shape = p$shape, scale = p$scale * sev_mult)) else 0
    }, numeric(1))
    
    total <- total + sys_loss * shock
  }
  return(total)
}

sys_baseline     <- run_system_sim()
sys_sev_stress   <- run_system_sim(sev_mult = 1.40)
sys_freq_stress  <- run_system_sim(freq_mult = 1.50)
sys_catastrophic <- run_system_sim(freq_mult = 1.50, sev_mult = 1.40, apply_shock = TRUE)

system_metrics <- function(losses, label) {
  var99 <- quantile(losses, 0.99)
  data.frame(
    Scenario  = label,
    Mean      = mean(losses),
    VaR_99    = var99,
    TVaR_99   = mean(losses[losses > var99]),
    Max_Loss  = max(losses)
  )
}

system_table <- rbind(
  system_metrics(sys_baseline,     "Baseline"),
  system_metrics(sys_sev_stress,   "Severity Stress (+40%)"),
  system_metrics(sys_freq_stress,  "Frequency Stress (+50%)"),
  system_metrics(sys_catastrophic, "Systemic Shock (Correlated)")
)
print(system_table)

# ── Visualise catastrophic scenario ──
sys_cat_nonzero <- sys_catastrophic[sys_catastrophic > 0]
cat(sprintf("Non-zero loss simulations: %d / %d (%.1f%%)\n",
            length(sys_cat_nonzero), length(sys_catastrophic),
            length(sys_cat_nonzero) / length(sys_catastrophic) * 100))

var99_cat <- as.numeric(system_table$VaR_99[4])

ggplot(data.frame(loss = sys_cat_nonzero), aes(x = loss)) +
  geom_density(fill = rgb(0.9, 0.3, 0.1, 0.4), colour = rgb(0.9, 0.3, 0.1),
               linewidth = 0.8) +
  geom_vline(xintercept = var99_cat,
             colour = "navy", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = var99_cat, y = Inf,
           label = "99% VaR", colour = "navy",
           hjust = -0.15, vjust = 1.5, size = 3.5) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Multi-System Catastrophic Loss Distribution – Equipment Failure",
       subtitle = sprintf("Conditional on shock occurring (%.1f%% of simulations)",
                          length(sys_cat_nonzero) / length(sys_catastrophic) * 100),
       x        = "Aggregate Loss (Đ)",
       y        = "Density") +
  theme_minimal()

# ── Overlay: all four scenarios as density curves ──
scenario_df <- rbind(
  data.frame(loss = sys_baseline[sys_baseline > 0],         Scenario = "Baseline"),
  data.frame(loss = sys_sev_stress[sys_sev_stress > 0],     Scenario = "Severity +40%"),
  data.frame(loss = sys_freq_stress[sys_freq_stress > 0],   Scenario = "Frequency +50%"),
  data.frame(loss = sys_catastrophic[sys_catastrophic > 0], Scenario = "Systemic Shock")
)

ggplot(scenario_df, aes(x = loss, colour = Scenario, fill = Scenario)) +
  geom_density(alpha = 0.15, linewidth = 0.8) +
  scale_x_continuous(labels = scales::comma) +
  scale_colour_manual(values = c("Baseline"       = "steelblue",
                                 "Severity +40%"  = "darkorange",
                                 "Frequency +50%" = "forestgreen",
                                 "Systemic Shock" = "firebrick")) +
  scale_fill_manual(values  = c("Baseline"       = "steelblue",
                                "Severity +40%"  = "darkorange",
                                "Frequency +50%" = "forestgreen",
                                "Systemic Shock" = "firebrick")) +
  labs(title    = "Loss Distribution by Scenario – Equipment Failure",
       subtitle = "Non-zero simulations only",
       x        = "Aggregate Loss (Đ)",
       y        = "Density",
       colour   = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

