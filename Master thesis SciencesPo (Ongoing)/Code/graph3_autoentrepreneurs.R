# Graph 3: Evolution of Autoentrepreneurs (2015-2020)
# This script plots the evolution of entrepreneurs, hybrid entrepreneurs, and poly-entrepreneurs
# Uses entrepreneur_type variable created in main script
# Uses arrow for efficient data processing

# Use entrepreneur_type from main script and aggregate in arrow
results_graph3 <- df_arrow |>
  filter(
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    entrepreneur_type %in% c("Pure Entrepreneur", "Hybrid Entrepreneur", "Polyentrepreneur")
  ) |>
  group_by(date, entrepreneur_type) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()  # Only collect aggregated result

# Calculate SE for counts in R after collection
results_graph3 <- results_graph3 |>
  mutate(
    se = sqrt(count) * 1.96  # 95% CI assuming Poisson
  )

# Create the plot
plot_graph3 <- ggplot(results_graph3, aes(x = date, y = count, color = entrepreneur_type, fill = entrepreneur_type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = pmax(0, count - se), ymax = count + se), alpha = 0.2, color = NA) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title = "Evolution of Autoentrepreneurs by Type (2015-2020)",
    subtitle = "Number of individuals by entrepreneur category",
    x = "Year",
    y = "Number of Individuals (weighted)",
    color = "Entrepreneur Type",
    fill = "Entrepreneur Type",
    caption = "Entrepreneurs: primary business only; Hybrid: secondary business only; Poly: both primary and secondary.\nShaded areas represent 95% confidence intervals."
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c(
      "Pure Entrepreneur" = "#1B9E77",
      "Hybrid Entrepreneur" = "#D95F02",
      "Polyentrepreneur" = "#7570B3"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Pure Entrepreneur" = "#1B9E77",
      "Hybrid Entrepreneur" = "#D95F02",
      "Polyentrepreneur" = "#7570B3"
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

# Display the plot
print(plot_graph3)

# Create output directory if it doesn't exist
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the plot
ggsave(
  file.path(output_dir, "graph3_autoentrepreneurs.png"),
  plot = plot_graph3,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Graph 3 completed successfully!\n")
