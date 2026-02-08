# Graph: Evolution of Entrepreneur Types (2015-2020)
# This script plots the share of three entrepreneur types in the active population
# Split into two plots: Pure Entrepreneurs (separate scale) and Hybrid + Poly
# Uses arrow for efficient data processing

# Load patchwork for combining plots
if (!require("patchwork")) {
  if (!require("gridExtra")) {
    stop("Either patchwork or gridExtra package is required for combined plots")
  }
  use_patchwork <- FALSE
} else {
  use_patchwork <- TRUE
}

# Filter and aggregate in arrow
results_entrepreneur <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    entrepreneur_type %in% c("Hybrid Entrepreneur", "Pure Entrepreneur", "Polyentrepreneur")
  ) |>
  group_by(date, entrepreneur_type) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()  # Only collect aggregated result

# Calculate total active population
total_active_entrepreneur <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31")
  ) |>
  group_by(date) |>
  summarise(total = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()

# Join and calculate shares
results_entrepreneur <- results_entrepreneur |>
  left_join(total_active_entrepreneur, by = "date") |>
  mutate(
    share = count / total * 100,
    se = sqrt(share * (100 - share) / total) * 1.96  # 95% CI
  )

# Create two separate plots for different scales

# Plot 1: Pure Entrepreneurs only
data_pure <- results_entrepreneur |> filter(entrepreneur_type == "Pure Entrepreneur")

plot_pure <- ggplot(data_pure, aes(x = date, y = share)) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.2, fill = "#1B9E77", color = NA) +
  geom_line(linewidth = 1, color = "#1B9E77") +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title = "Pure Entrepreneurs",
    x = "Year",
    y = "Share of Active Population (%)"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Plot 2: Hybrid and Poly Entrepreneurs
data_hybrid_poly <- results_entrepreneur |>
  filter(entrepreneur_type %in% c("Hybrid Entrepreneur", "Polyentrepreneur"))

plot_hybrid_poly <- ggplot(data_hybrid_poly, aes(x = date, y = share, color = entrepreneur_type, fill = entrepreneur_type)) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title = "Hybrid & Poly Entrepreneurs",
    x = "Year",
    y = "Share of Active Population (%)",
    color = "Type",
    fill = "Type"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    values = c(
      "Hybrid Entrepreneur" = "#D95F02",
      "Polyentrepreneur" = "#7570B3"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Hybrid Entrepreneur" = "#D95F02",
      "Polyentrepreneur" = "#7570B3"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Combine plots side by side
if (use_patchwork) {
  plot_entrepreneur <- plot_pure + plot_hybrid_poly +
    plot_annotation(
      title = "Evolution of Entrepreneur Types (2015-2020)",
      subtitle = "Share of active population by entrepreneur type (separate scales)",
      caption = "Shaded areas represent 95% confidence intervals.",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )
} else {
  plot_entrepreneur <- gridExtra::grid.arrange(plot_pure, plot_hybrid_poly, ncol = 2,
    top = grid::textGrob("Evolution of Entrepreneur Types (2015-2020)",
                         gp = grid::gpar(fontface = "bold", fontsize = 16)))
}

# Display the combined plot
print(plot_entrepreneur)

# Create output directory if it doesn't exist
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the plot
ggsave(
  file.path(output_dir, "graph_entrepreneur_types.png"),
  plot = plot_entrepreneur,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Graph entrepreneur types completed successfully!\n")
