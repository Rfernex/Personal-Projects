# Graph 2: Evolution of Wage-Only and Self-Only Workers (2015-2020)
# This script creates TWO separate plots side by side with independent scales
# Uses arrow for efficient data processing

# === WAGE ONLY WORKERS ===
# Filter for wage only workers (stc == 3 AND plract != 1)
results_wage_only <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    stc == 3,
    plract != 1
  ) |>
  group_by(date) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()

# === SELF ONLY WORKERS ===
# Filter for self only workers (stc %in% c(1, 2) AND plract != 1)
results_self_only <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    stc %in% c(1, 2),
    plract != 1
  ) |>
  group_by(date) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()

# Calculate total active population
total_active_graph2 <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31")
  ) |>
  group_by(date) |>
  summarise(total = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()

# Calculate shares for wage only
results_wage_only <- results_wage_only |>
  left_join(total_active_graph2, by = "date") |>
  mutate(
    share = count / total * 100,
    se = sqrt(share * (100 - share) / total) * 1.96,  # 95% CI
    type = "Wage Only"
  )

# Calculate shares for self only
results_self_only <- results_self_only |>
  left_join(total_active_graph2, by = "date") |>
  mutate(
    share = count / total * 100,
    se = sqrt(share * (100 - share) / total) * 1.96,  # 95% CI
    type = "Self Only"
  )

# Create wage only plot
plot_wage <- ggplot(results_wage_only, aes(x = date, y = share)) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.2, fill = "#E41A1C") +
  geom_line(color = "#E41A1C", linewidth = 1) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title = "Wage-Only Workers",
    x = "Year",
    y = "Share of Active Population (%)"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Create self only plot
plot_self <- ggplot(results_self_only, aes(x = date, y = share)) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.2, fill = "#377EB8") +
  geom_line(color = "#377EB8", linewidth = 1) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title = "Self-Only Workers",
    x = "Year",
    y = "Share of Active Population (%)"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Combine plots side by side using patchwork (or gridExtra if patchwork not available)
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  plot_graph2 <- plot_wage + plot_self +
    plot_annotation(
      title = "Evolution of Wage-Only and Self-Only Workers (2015-2020)",
      subtitle = "Share of active population (excluding pluriactive individuals)",
      caption = "Shaded areas represent 95% confidence intervals.",
      theme = theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)
      )
    )
} else {
  library(gridExtra)
  plot_graph2 <- grid.arrange(
    plot_wage, plot_self,
    ncol = 2,
    top = "Evolution of Wage-Only and Self-Only Workers (2015-2020)"
  )
}

# Display the combined plot
print(plot_graph2)

# Create output directory if it doesn't exist
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the combined plot
ggsave(
  file.path(output_dir, "graph2_wage_self_only.png"),
  plot = plot_graph2,
  width = 16,
  height = 7,
  dpi = 300
)

cat("Graph 2 (combined wage and self only) completed successfully!\n")
