# Table: Entrepreneurs by Sector and Time (2013 Q1 - 2020 Q4)
# This script creates a table showing the number of entrepreneurs by type and sector over time
# Uses arrow for efficient data processing

# Load required library for LaTeX export
library(xtable)

# Calculate weighted counts by quarter, sector, and entrepreneur type in arrow
# Include all 4 categories: Poly, Hybrid, Pure (Entrepreneurs), and Non-Entrepreneurs
results_table <- df_arrow |>
  filter(
    date >= as.Date("2013-01-01"),
    date <= as.Date("2020-12-31"),
    !is.na(nafg010n),
    acteu == 1  # Only active population
  ) |>
  mutate(
    # Create indicators for primary and secondary businesses
    has_primary = ifelse(autoent == 1, 1, 0),
    has_secondary = ifelse(autoenta == 1 | autoentb == 1 | autoentc == 1, 1, 0),

    # Classify into 4 categories (including Non-Entrepreneurs)
    entrepreneur_type = case_when(
      has_primary == 1 & has_secondary == 1 ~ "Poly-Entrepreneurs",
      has_primary == 1 & has_secondary == 0 ~ "Pure Entrepreneurs",
      has_primary == 0 & has_secondary == 1 ~ "Hybrid Entrepreneurs",
      TRUE ~ "Non-Entrepreneurs"
    )
  ) |>
  group_by(date, nafg010n, entrepreneur_type) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()  # Only collect aggregated result

# Calculate total hybrid entrepreneurs by sector for sorting
sector_order <- results_table |>
  filter(entrepreneur_type == "Hybrid Entrepreneurs") |>
  group_by(nafg010n) |>
  summarise(total_hybrid = sum(count), .groups = "drop") |>
  arrange(desc(total_hybrid))

# Create wide format table with sectors as rows and dates/types as columns
table_wide <- results_table |>
  mutate(
    year_quarter = paste0(format(date, "%Y"), "-Q", quarter(date)),
    nafg010n = factor(nafg010n, levels = sector_order$nafg010n)
  ) |>
  arrange(nafg010n, date, entrepreneur_type) |>
  pivot_wider(
    names_from = c(year_quarter, entrepreneur_type),
    values_from = count,
    values_fill = 0
  )

# Create output directory if it doesn't exist
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the table as CSV (backup)
write.csv(
  table_wide,
  file.path(output_dir, "table_entrepreneurs_by_sector.csv"),
  row.names = FALSE
)

# Export to LaTeX format
xt_wide <- xtable(table_wide,
                  caption = "Entrepreneurs by Sector and Time (2013 Q1 - 2020 Q4)",
                  label = "tab:entrepreneurs_by_sector")
print(xt_wide,
      file = file.path(output_dir, "table_entrepreneurs_by_sector.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      size = "\\small",
      scalebox = 0.8)

# Save formatted text version
capture.output(print(table_wide), file = file.path(output_dir, "table_entrepreneurs_by_sector.txt"))

# Create summary table: sectors as columns, entrepreneur categories as rows
# Show column shares (% within each sector)
summary_table <- results_table |>
  group_by(nafg010n, entrepreneur_type) |>
  summarise(total = sum(count), .groups = "drop") |>
  group_by(nafg010n) |>
  mutate(share = total / sum(total) * 100) |>
  ungroup() |>
  select(nafg010n, entrepreneur_type, share) |>
  pivot_wider(
    names_from = nafg010n,
    values_from = share,
    values_fill = 0
  ) |>
  # Ensure all 4 categories are present in the correct order
  mutate(entrepreneur_type = factor(entrepreneur_type,
    levels = c("Pure Entrepreneurs", "Hybrid Entrepreneurs", "Poly-Entrepreneurs", "Non-Entrepreneurs"))) |>
  arrange(entrepreneur_type)

# Save summary table as CSV (backup)
write.csv(
  summary_table,
  file.path(output_dir, "table_entrepreneurs_by_sector_summary.csv"),
  row.names = FALSE
)

# Export summary to LaTeX format
xt_summary <- xtable(summary_table,
                     caption = "Summary: Total Entrepreneurs by Sector (2013-2020)",
                     label = "tab:entrepreneurs_by_sector_summary")
print(xt_summary,
      file = file.path(output_dir, "table_entrepreneurs_by_sector_summary.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top")

# Save formatted text version
capture.output(print(summary_table), file = file.path(output_dir, "table_entrepreneurs_by_sector_summary.txt"))

cat("Tables saved successfully!\n")
cat(paste("Number of entrepreneur categories:", nrow(summary_table), "\n"))
cat(paste("Number of sectors (columns):", ncol(summary_table) - 1, "\n"))
cat("\nSummary table structure: Entrepreneur categories (rows) × Sectors (columns)\n")
cat("Values represent % share within each sector (column sums to 100%)\n")
print(summary_table)
