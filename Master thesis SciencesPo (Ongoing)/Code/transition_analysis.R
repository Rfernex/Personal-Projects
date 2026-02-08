# Transition Analysis: Share of Each Transition Type by Quarter
# This script analyzes the share of each transition type in the active population over time

# Load required library for LaTeX export
library(xtable)

cat("Starting transition analysis...\n")

# Step 1: Create individual identifier and prepare panel data
cat("Step 1: Creating individual identifiers and employment status...\n")
panel_data <- df_arrow |>
  filter(acteu == 1) |>
  mutate(
    # Create unique individual identifier
    individual_id = paste0(household, "_", individual),

    # Create simplified employment status variable (mutually exclusive categories)
    employment_status = case_when(
      # First, assign pluriactivity categories
      plur_type == "Wage + Wage" ~ "wage_wage",
      plur_type == "Wage + Self" ~ "wage_self",
      plur_type == "Self + Wage" ~ "self_wage",
      plur_type == "Self + Self" ~ "self_self",
      # Then, assign wage_only and self_only ONLY if not pluriactive
      plur_type %in% c("Not Pluriactive", "Undefined Pluriactive") & stc == 3 ~ "wage_only",
      plur_type %in% c("Not Pluriactive", "Undefined Pluriactive") & stc %in% c(1, 2) ~ "self_only",
      TRUE ~ NA_character_
    )
  ) |>
  select(individual_id, date, annee, trimester, employment_status, extri) |>
  filter(!is.na(employment_status)) |>
  collect()

cat(paste("   Total observations:", nrow(panel_data), "\n"))

# Step 2: Create lagged employment status to detect transitions
cat("\nStep 2: Creating lagged status for transition detection...\n")
panel_data <- panel_data |>
  arrange(individual_id, date) |>
  group_by(individual_id) |>
  mutate(
    employment_status_lag = lag(employment_status, 1),
    time_diff = as.numeric(difftime(date, lag(date), units = "days")),
    is_consecutive = (time_diff >= 80 & time_diff <= 100)  # ~3 months
  ) |>
  ungroup()

# Step 3: Identify transitions
cat("\nStep 3: Identifying transitions...\n")
transitions_data <- panel_data |>
  filter(
    !is.na(employment_status_lag),
    is_consecutive
  ) |>
  mutate(
    transitioned = (employment_status != employment_status_lag),
    transition_type = if_else(
      transitioned,
      paste0(employment_status_lag, " → ", employment_status),
      "No transition"
    )
  )

cat(paste("   Unique transition types:", length(unique(transitions_data$transition_type[transitions_data$transitioned])), "\n"))

# Step 4: Calculate share of each transition type by quarter
cat("\nStep 4: Calculating transition shares by quarter...\n")

# Total active population by quarter (for denominator)
total_active_by_quarter <- panel_data |>
  group_by(date) |>
  summarise(total_active = sum(extri, na.rm = TRUE), .groups = "drop")

# Count of each transition type by quarter
transition_shares <- transitions_data |>
  group_by(date, transition_type) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  left_join(total_active_by_quarter, by = "date") |>
  mutate(
    share = count / total_active * 100,
    year_quarter = paste0(format(date, "%Y"), "-Q", quarter(date))
  ) |>
  select(date, year_quarter, transition_type, count, total_active, share) |>
  arrange(date, desc(count))

# Create output directory
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the table as CSV (backup)
write.csv(
  transition_shares,
  file.path(output_dir, "table_transition_shares_by_quarter.csv"),
  row.names = FALSE
)

# Export to LaTeX format
xt_transitions <- xtable(transition_shares,
                         caption = "Transition Shares by Quarter - All Sectors",
                         label = "tab:transition_shares_by_quarter")
print(xt_transitions,
      file = file.path(output_dir, "table_transition_shares_by_quarter.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      size = "\\small",
      scalebox = 0.75)

# Save formatted text version
capture.output(print(transition_shares), file = file.path(output_dir, "table_transition_shares_by_quarter.txt"))

# Display summary
cat("\n========================================\n")
cat("Transition Analysis Summary\n")
cat("========================================\n\n")

cat(paste("Total observations:", nrow(transitions_data), "\n"))
cat(paste("Quarters analyzed:", length(unique(transition_shares$date)), "\n"))
cat(paste("Unique transition types:", length(unique(transition_shares$transition_type)), "\n\n"))

cat("Top 5 most common transitions (average across all quarters):\n")
top_transitions <- transition_shares |>
  group_by(transition_type) |>
  summarise(avg_share = mean(share), .groups = "drop") |>
  arrange(desc(avg_share)) |>
  head(5)
print(top_transitions)

cat("\n\nTable saved:\n")
cat("  table_transition_shares_by_quarter.csv\n")
cat("  Contains: date, year_quarter, transition_type, count, total_active, share (%)\n")

cat("\nTransition analysis completed successfully!\n")

# ===== PART 2: Transitions in Top 5 Sectors =====
cat("\n\n========================================\n")
cat("Part 2: Analyzing Top 5 Sectors\n")
cat("========================================\n\n")

# Step 5: Identify top 5 sectors with most hybrid entrepreneurs
cat("Step 5: Identifying top 5 sectors with most entrepreneurs...\n")
top_5_sectors_data <- df_arrow |>
  filter(date >= as.Date("2013-01-01"), !is.na(nafg010n)) |>
  mutate(
    has_primary = ifelse(autoent == 1, 1, 0),
    has_secondary = ifelse(autoenta == 1 | autoentb == 1 | autoentc == 1, 1, 0),
    is_hybrid = ifelse(has_primary == 0 & has_secondary == 1, 1, 0)
  ) |>
  filter(is_hybrid == 1) |>
  group_by(nafg010n) |>
  summarise(total_hybrid = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()

top_5_sectors <- top_5_sectors_data |>
  arrange(desc(total_hybrid)) |>
  slice(1:5) |>
  pull(nafg010n)

cat("   Top 5 sectors:\n")
print(top_5_sectors)

# Step 6: Get sector information for panel data
cat("\nStep 6: Adding sector information to panel data...\n")
panel_data_sectors <- df_arrow |>
  filter(acteu == 1) |>
  select(household, individual, date, nafg010n) |>
  collect() |>
  mutate(individual_id = paste0(household, "_", individual))

# Join with existing panel data
panel_data_with_sectors <- panel_data |>
  left_join(panel_data_sectors, by = c("individual_id", "date"))

# Step 7: Create transitions for panel data with sectors
cat("\nStep 7: Creating transitions data with sector information...\n")
transitions_data_sectors <- panel_data_with_sectors |>
  arrange(individual_id, date) |>
  group_by(individual_id) |>
  mutate(
    employment_status_lag = lag(employment_status, 1),
    time_diff = as.numeric(difftime(date, lag(date), units = "days")),
    is_consecutive = (time_diff >= 80 & time_diff <= 100)
  ) |>
  ungroup() |>
  filter(
    !is.na(employment_status_lag),
    is_consecutive,
    !is.na(nafg010n),
    nafg010n %in% top_5_sectors
  ) |>
  mutate(
    transitioned = (employment_status != employment_status_lag),
    transition_type = if_else(
      transitioned,
      paste0(employment_status_lag, " → ", employment_status),
      "No transition"
    )
  )

# Step 8: Calculate shares for top 5 sectors
cat("\nStep 8: Calculating transition shares for top 5 sectors...\n")
total_active_top5 <- panel_data_with_sectors |>
  filter(nafg010n %in% top_5_sectors) |>
  group_by(date) |>
  summarise(total_active = sum(extri, na.rm = TRUE), .groups = "drop")

transition_shares_top5 <- transitions_data_sectors |>
  group_by(date, transition_type) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  left_join(total_active_top5, by = "date") |>
  mutate(
    share = count / total_active * 100,
    year_quarter = paste0(format(date, "%Y"), "-Q", quarter(date))
  ) |>
  select(date, year_quarter, transition_type, count, total_active, share) |>
  arrange(date, desc(count))

# Save top 5 sectors table as CSV (backup)
write.csv(
  transition_shares_top5,
  file.path(output_dir, "table_transition_shares_top5_sectors.csv"),
  row.names = FALSE
)

# Export top 5 sectors to LaTeX format
xt_transitions_top5 <- xtable(transition_shares_top5,
                              caption = "Transition Shares by Quarter - Top 5 Sectors with Most Hybrid Entrepreneurs",
                              label = "tab:transition_shares_top5_sectors")
print(xt_transitions_top5,
      file = file.path(output_dir, "table_transition_shares_top5_sectors.tex"),
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      size = "\\small",
      scalebox = 0.75)

# Save formatted text version
capture.output(print(transition_shares_top5), file = file.path(output_dir, "table_transition_shares_top5_sectors.txt"))

cat("   Table saved: table_transition_shares_top5_sectors.csv\n")

# ===== PART 3: Plots of Specific Transitions =====
cat("\n\n========================================\n")
cat("Part 3: Creating Transition Plots\n")
cat("========================================\n\n")

# Define the 4 specific transitions to plot
key_transitions <- c(
  "wage_only → wage_self",
  "wage_self → self_only",
  "wage_only → self_only",
  "self_wage → self_only"
)

# Filter data for plots (2013 Q1 to 2020 Q4) and add standard errors
plot_data_all <- transition_shares |>
  filter(
    date >= as.Date("2013-01-01"),
    date <= as.Date("2020-12-31"),
    transition_type %in% key_transitions
  ) |>
  mutate(
    se = sqrt(share * (100 - share) / total_active) * 1.96  # 95% CI
  )

plot_data_top5 <- transition_shares_top5 |>
  filter(
    date >= as.Date("2013-01-01"),
    date <= as.Date("2020-12-31"),
    transition_type %in% key_transitions
  ) |>
  mutate(
    se = sqrt(share * (100 - share) / total_active) * 1.96  # 95% CI
  )

# Plot 1: All active population
cat("Creating plot 1: All active population...\n")
plot_transitions_all <- ggplot(plot_data_all, aes(x = date, y = share, color = transition_type, fill = transition_type)) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title = "Key Employment Transitions (2013-2020)",
    subtitle = "Share of active population - All sectors",
    x = "Year",
    y = "Share of Active Population (%)",
    color = "Transition Type",
    fill = "Transition Type",
    caption = "Shaded areas represent 95% confidence intervals."
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    values = c(
      "wage_only → wage_self" = "#E41A1C",
      "wage_self → self_only" = "#377EB8",
      "wage_only → self_only" = "#4DAF4A",
      "self_wage → self_only" = "#984EA3"
    ),
    labels = c(
      "wage_only → wage_self" = "Wage → Wage+Self",
      "wage_self → self_only" = "Wage+Self → Self",
      "wage_only → self_only" = "Wage → Self",
      "self_wage → self_only" = "Self+Wage → Self"
    )
  ) +
  scale_fill_manual(
    values = c(
      "wage_only → wage_self" = "#E41A1C",
      "wage_self → self_only" = "#377EB8",
      "wage_only → self_only" = "#4DAF4A",
      "self_wage → self_only" = "#984EA3"
    ),
    labels = c(
      "wage_only → wage_self" = "Wage → Wage+Self",
      "wage_self → self_only" = "Wage+Self → Self",
      "wage_only → self_only" = "Wage → Self",
      "self_wage → self_only" = "Self+Wage → Self"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(plot_transitions_all)
ggsave(
  file.path(output_dir, "graph_transitions_all_sectors.png"),
  plot = plot_transitions_all,
  width = 12,
  height = 7,
  dpi = 300
)

# Plot 2: Top 5 sectors
cat("Creating plot 2: Top 5 sectors...\n")
plot_transitions_top5 <- ggplot(plot_data_top5, aes(x = date, y = share, color = transition_type, fill = transition_type)) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title = "Key Employment Transitions (2013-2020)",
    subtitle = "Share of active population - Top 5 sectors with most hybrid entrepreneurs",
    x = "Year",
    y = "Share of Active Population (%)",
    color = "Transition Type",
    fill = "Transition Type",
    caption = "Shaded areas represent 95% confidence intervals."
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    values = c(
      "wage_only → wage_self" = "#E41A1C",
      "wage_self → self_only" = "#377EB8",
      "wage_only → self_only" = "#4DAF4A",
      "self_wage → self_only" = "#984EA3"
    ),
    labels = c(
      "wage_only → wage_self" = "Wage → Wage+Self",
      "wage_self → self_only" = "Wage+Self → Self",
      "wage_only → self_only" = "Wage → Self",
      "self_wage → self_only" = "Self+Wage → Self"
    )
  ) +
  scale_fill_manual(
    values = c(
      "wage_only → wage_self" = "#E41A1C",
      "wage_self → self_only" = "#377EB8",
      "wage_only → self_only" = "#4DAF4A",
      "self_wage → self_only" = "#984EA3"
    ),
    labels = c(
      "wage_only → wage_self" = "Wage → Wage+Self",
      "wage_self → self_only" = "Wage+Self → Self",
      "wage_only → self_only" = "Wage → Self",
      "self_wage → self_only" = "Self+Wage → Self"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(plot_transitions_top5)
ggsave(
  file.path(output_dir, "graph_transitions_top5_sectors.png"),
  plot = plot_transitions_top5,
  width = 12,
  height = 7,
  dpi = 300
)

cat("\n========================================\n")
cat("Complete Transition Analysis Summary\n")
cat("========================================\n\n")
cat("Tables saved:\n")
cat("  1. table_transition_shares_by_quarter.csv (all sectors)\n")
cat("  2. table_transition_shares_top5_sectors.csv (top 5 sectors)\n\n")
cat("Plots saved:\n")
cat("  1. graph_transitions_all_sectors.png\n")
cat("  2. graph_transitions_top5_sectors.png\n\n")
cat("Analysis completed successfully!\n")
