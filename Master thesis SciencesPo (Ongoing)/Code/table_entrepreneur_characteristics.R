# Table: Characteristics of Entrepreneurs (2013-2020)
# This script creates tables showing demographic and socio-economic characteristics
# of entrepreneurs compared to non-entrepreneurs
# Uses arrow for efficient data processing

# Load required library for LaTeX export
library(xtable)

cat("Analyzing entrepreneur characteristics...\n")

# Step 1: Use entrepreneur_type from main script (already created in df_arrow)
characteristics_data <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2013-01-01"),
    date <= as.Date("2020-12-31")
  ) |>
  mutate(
    # Create trimester variable for time series
    year_quarter = paste0(format(date, "%Y"), "-Q", quarter(date)),
    # Rename for consistency with existing code
    category = entrepreneur_type
  ) |>
  select(date, year_quarter, category, ddipl, paiperc, cser, age3, extri)

# Collect the filtered data (should be smaller now)
characteristics_collected <- characteristics_data |>
  collect()

cat("Data collected. Creating summary tables...\n")

# ===== TABLE 1: Average characteristics over 2013-2020 =====

# Function to calculate weighted shares for a variable
calculate_weighted_shares <- function(data, var_name) {
  data |>
    filter(!is.na(!!sym(var_name))) |>
    group_by(category, !!sym(var_name)) |>
    summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
    group_by(category) |>
    mutate(
      total = sum(count),
      share = count / total * 100
    ) |>
    select(category, !!sym(var_name), share) |>
    pivot_wider(
      names_from = category,
      values_from = share,
      values_fill = 0
    )
}

# Calculate shares for each characteristic
cat("Calculating shares by education (ddipl)...\n")
shares_ddipl <- calculate_weighted_shares(characteristics_collected, "ddipl")

cat("Calculating shares by father's origin (paiperc)...\n")
shares_paiperc <- calculate_weighted_shares(characteristics_collected, "paiperc")

cat("Calculating shares by socio-professional category (cser)...\n")
shares_cser <- calculate_weighted_shares(characteristics_collected, "cser")

cat("Calculating shares by age category (age3)...\n")
shares_age3 <- calculate_weighted_shares(characteristics_collected, "age3")

# Add age range labels
shares_age3 <- shares_age3 |>
  mutate(
    age3 = case_when(
      age3 == 15 ~ "15-29",
      age3 == 30 ~ "30-39",
      age3 == 40 ~ "40-49",
      age3 == 50 ~ "50-59",
      age3 == 60 ~ "60+",
      TRUE ~ as.character(age3)
    )
  )

# Create output directory if it doesn't exist
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save average tables as CSV (backup)
write.csv(
  shares_ddipl,
  file.path(output_dir, "table_characteristics_education_avg.csv"),
  row.names = FALSE
)

write.csv(
  shares_paiperc,
  file.path(output_dir, "table_characteristics_father_origin_avg.csv"),
  row.names = FALSE
)

write.csv(
  shares_cser,
  file.path(output_dir, "table_characteristics_sociopro_avg.csv"),
  row.names = FALSE
)

write.csv(
  shares_age3,
  file.path(output_dir, "table_characteristics_age_avg.csv"),
  row.names = FALSE
)

# Export average tables to LaTeX format
xt_edu <- xtable(shares_ddipl,
                 caption = "Education Distribution by Entrepreneur Type (2013-2020 Average)",
                 label = "tab:characteristics_education_avg")
print(xt_edu,
      file = file.path(output_dir, "table_characteristics_education_avg.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top")
# Save formatted text version
capture.output(print(shares_ddipl), file = file.path(output_dir, "table_characteristics_education_avg.txt"))

xt_father <- xtable(shares_paiperc,
                    caption = "Father's Origin Distribution by Entrepreneur Type (2013-2020 Average)",
                    label = "tab:characteristics_father_origin_avg")
print(xt_father,
      file = file.path(output_dir, "table_characteristics_father_origin_avg.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top")
# Save formatted text version
capture.output(print(shares_paiperc), file = file.path(output_dir, "table_characteristics_father_origin_avg.txt"))

xt_cser <- xtable(shares_cser,
                 caption = "Socio-Professional Category Distribution by Entrepreneur Type (2013-2020 Average)",
                 label = "tab:characteristics_sociopro_avg")
print(xt_cser,
      file = file.path(output_dir, "table_characteristics_sociopro_avg.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top")
# Save formatted text version
capture.output(print(shares_cser), file = file.path(output_dir, "table_characteristics_sociopro_avg.txt"))

xt_age <- xtable(shares_age3,
                 caption = "Age Distribution by Entrepreneur Type (2013-2020 Average)",
                 label = "tab:characteristics_age_avg")
print(xt_age,
      file = file.path(output_dir, "table_characteristics_age_avg.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top")
# Save formatted text version
capture.output(print(shares_age3), file = file.path(output_dir, "table_characteristics_age_avg.txt"))

cat("\nAverage tables saved successfully!\n")
cat("Preview of education shares:\n")
print(shares_ddipl)

# ===== TABLE 2: Evolution by trimester 2013-2020 =====

cat("\n\nCalculating evolution by trimester...\n")

# Function to calculate weighted shares over time
calculate_weighted_shares_time <- function(data, var_name) {
  data |>
    filter(!is.na(!!sym(var_name))) |>
    group_by(year_quarter, category, !!sym(var_name)) |>
    summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
    group_by(year_quarter, category) |>
    mutate(
      total = sum(count),
      share = count / total * 100
    ) |>
    select(year_quarter, category, !!sym(var_name), share)
}

# Calculate evolution for each characteristic
cat("Calculating education evolution...\n")
evolution_ddipl <- calculate_weighted_shares_time(characteristics_collected, "ddipl")

cat("Calculating father's origin evolution...\n")
evolution_paiperc <- calculate_weighted_shares_time(characteristics_collected, "paiperc")

cat("Calculating socio-professional category evolution...\n")
evolution_cser <- calculate_weighted_shares_time(characteristics_collected, "cser")

cat("Calculating age category evolution...\n")
evolution_age3 <- calculate_weighted_shares_time(characteristics_collected, "age3")

# Add age range labels to evolution table
evolution_age3 <- evolution_age3 |>
  mutate(
    age3 = case_when(
      age3 == 15 ~ "15-29",
      age3 == 30 ~ "30-39",
      age3 == 40 ~ "40-49",
      age3 == 50 ~ "50-59",
      age3 == 60 ~ "60+",
      TRUE ~ as.character(age3)
    )
  )

# Transpose evolution tables: time horizontal, category+variable vertical
transpose_evolution_table <- function(data, var_name) {
  data |>
    unite("category_var", category, !!sym(var_name), sep = " - ") |>
    pivot_wider(
      names_from = year_quarter,
      values_from = share,
      values_fill = 0
    ) |>
    arrange(category_var)
}

# Create transposed versions for LaTeX export
evolution_ddipl_wide <- transpose_evolution_table(evolution_ddipl, "ddipl")
evolution_paiperc_wide <- transpose_evolution_table(evolution_paiperc, "paiperc")
evolution_cser_wide <- transpose_evolution_table(evolution_cser, "cser")
evolution_age3_wide <- transpose_evolution_table(evolution_age3, "age3")

# Save evolution tables as CSV (backup) - long format
write.csv(
  evolution_ddipl,
  file.path(output_dir, "table_characteristics_education_evolution_long.csv"),
  row.names = FALSE
)

# Save transposed (wide) format
write.csv(
  evolution_ddipl_wide,
  file.path(output_dir, "table_characteristics_education_evolution.csv"),
  row.names = FALSE
)

write.csv(
  evolution_paiperc_wide,
  file.path(output_dir, "table_characteristics_father_origin_evolution.csv"),
  row.names = FALSE
)

write.csv(
  evolution_cser_wide,
  file.path(output_dir, "table_characteristics_sociopro_evolution.csv"),
  row.names = FALSE
)

write.csv(
  evolution_age3_wide,
  file.path(output_dir, "table_characteristics_age_evolution.csv"),
  row.names = FALSE
)

# Export evolution tables to LaTeX format (using wide/transposed format)
xt_edu_evol <- xtable(evolution_ddipl_wide,
                      caption = "Education Distribution by Entrepreneur Type - Evolution by Quarter (2013-2020)",
                      label = "tab:characteristics_education_evolution")
print(xt_edu_evol,
      file = file.path(output_dir, "table_characteristics_education_evolution.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      size = "\\tiny",
      scalebox = 0.6)
# Save formatted text version
capture.output(print(evolution_ddipl_wide), file = file.path(output_dir, "table_characteristics_education_evolution.txt"))

xt_father_evol <- xtable(evolution_paiperc_wide,
                         caption = "Father's Origin Distribution by Entrepreneur Type - Evolution by Quarter (2013-2020)",
                         label = "tab:characteristics_father_origin_evolution")
print(xt_father_evol,
      file = file.path(output_dir, "table_characteristics_father_origin_evolution.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      size = "\\tiny",
      scalebox = 0.6)
# Save formatted text version
capture.output(print(evolution_paiperc_wide), file = file.path(output_dir, "table_characteristics_father_origin_evolution.txt"))

xt_cser_evol <- xtable(evolution_cser_wide,
                      caption = "Socio-Professional Category Distribution by Entrepreneur Type - Evolution by Quarter (2013-2020)",
                      label = "tab:characteristics_sociopro_evolution")
print(xt_cser_evol,
      file = file.path(output_dir, "table_characteristics_sociopro_evolution.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      size = "\\tiny",
      scalebox = 0.6)
# Save formatted text version
capture.output(print(evolution_cser_wide), file = file.path(output_dir, "table_characteristics_sociopro_evolution.txt"))

xt_age_evol <- xtable(evolution_age3_wide,
                      caption = "Age Distribution by Entrepreneur Type - Evolution by Quarter (2013-2020)",
                      label = "tab:characteristics_age_evolution")
print(xt_age_evol,
      file = file.path(output_dir, "table_characteristics_age_evolution.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      size = "\\tiny",
      scalebox = 0.6)
# Save formatted text version
capture.output(print(evolution_age3_wide), file = file.path(output_dir, "table_characteristics_age_evolution.txt"))

cat("\nEvolution tables saved successfully!\n")
cat("\nAll tables saved in:", output_dir, "\n")
cat("\nSummary:\n")
cat("- Average tables (2013-2020): 4 files (*_avg.csv)\n")
cat("- Evolution tables (by trimester): 4 files (*_evolution.csv)\n")
cat("\nCharacteristics analyzed:\n")
cat("  1. Education (ddipl)\n")
cat("  2. Father's origin (paiperc)\n")
cat("  3. Socio-professional category (cser)\n")
cat("  4. Age category (age3)\n")
cat("\nFor 4 groups:\n")
cat("  - Entrepreneurs (primary business only)\n")
cat("  - Hybrid Entrepreneurs (secondary business only)\n")
cat("  - Poly-Entrepreneurs (both)\n")
cat("  - Non-Entrepreneurs (active but no business)\n")
