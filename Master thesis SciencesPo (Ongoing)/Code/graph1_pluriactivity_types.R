# Graph 1: Evolution of Pluriactivity Types (2015-2020)
# This script plots the share of 4 main pluriactivity types in the active population
# Uses arrow for efficient data processing

# Filter and aggregate in arrow (lazy evaluation)
results_graph1 <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    plur_type %in% c("Wage + Wage", "Wage + Self", "Self + Wage", "Self + Self")
  ) |>
  group_by(date, plur_type) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()  # Only collect the small aggregated result

# Calculate total active population for denominator
total_active_graph1 <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31")
  ) |>
  group_by(date) |>
  summarise(total = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()  # Only collect aggregated result

# Join and calculate shares
results_graph1 <- results_graph1 |>
  left_join(total_active_graph1, by = "date") |>
  mutate(
    share = count / total * 100,
    se = sqrt(share * (100 - share) / total) * 1.96  # 95% CI
  )

# Create the plot
plot_graph1 <- ggplot(results_graph1, aes(x = date, y = share, color = plur_type, fill = plur_type)) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title = "Evolution of Pluriactivity Types (2015-2020)",
    subtitle = "Share of active population by pluriactivity type",
    x = "Year",
    y = "Share of Active Population (%)",
    color = "Pluriactivity Type",
    fill = "Pluriactivity Type",
    caption = "Shaded areas represent 95% confidence intervals."
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Display the plot
print(plot_graph1)

# Create output directory if it doesn't exist
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the plot
ggsave(
  file.path(output_dir, "graph1_pluriactivity_types.png"),
  plot = plot_graph1,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Graph 1 completed successfully!\n")
