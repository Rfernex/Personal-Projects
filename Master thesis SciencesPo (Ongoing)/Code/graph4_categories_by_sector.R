# Graph 4: Evolution of Professional Categories by Sector (2015-2020)
# This script plots the evolution of all 6 professional categories in the top 5 sectors
# Uses arrow for efficient data processing

# Step 1: Identify top 5 sectors with most hybrid entrepreneurs in arrow
top_5_sectors_data <- df_arrow |>
  filter(date >= as.Date("2015-01-01"), !is.na(nafg010n)) |>
  mutate(
    has_primary = ifelse(autoent == 1, 1, 0),
    has_secondary = ifelse(autoenta == 1 | autoentb == 1 | autoentc == 1, 1, 0),
    is_hybrid = ifelse(has_primary == 0 & has_secondary == 1, 1, 0)
  ) |>
  filter(is_hybrid == 1) |>
  group_by(nafg010n) |>
  summarise(total_hybrid = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()  # Collect small result

top_5_sectors <- top_5_sectors_data |>
  arrange(desc(total_hybrid)) |>
  slice(1:5) |>
  pull(nafg010n)

cat("Top 5 sectors by hybrid entrepreneurs:\n")
print(top_5_sectors)

# Step 2: Create professional category variable and aggregate in arrow
results_graph4 <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    nafg010n %in% top_5_sectors
  ) |>
  mutate(
    professional_category = case_when(
      # First check for pluriactivity types
      plur_type == "Wage + Wage" ~ "Wage + Wage",
      plur_type == "Wage + Self" ~ "Wage + Self",
      plur_type == "Self + Wage" ~ "Self + Wage",
      plur_type == "Self + Self" ~ "Self + Self",
      # Then check for single job types
      stc == 3 ~ "Wage Only",
      stc %in% c(1, 2) ~ "Self Only",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(professional_category)) |>
  group_by(date, nafg010n, professional_category) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()  # Collect aggregated result

# Calculate total active population by sector and date
total_active <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    nafg010n %in% top_5_sectors
  ) |>
  group_by(date, nafg010n) |>
  summarise(total = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()

# Join and calculate shares
results_graph4 <- results_graph4 |>
  left_join(total_active, by = c("date", "nafg010n")) |>
  mutate(
    share = count / total * 100,
    se = sqrt(share * (100 - share) / total) * 1.96  # 95% CI
  )

# Create the faceted plot - facet by professional category, color by sector
plot_graph4 <- ggplot(results_graph4, aes(x = date, y = share, color = nafg010n, fill = nafg010n)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.15, color = NA) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.6) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3, fontface = "italic") +
  facet_wrap(~professional_category, scales = "free_y", ncol = 2) +
  labs(
    title = "Evolution of Professional Categories by Sector (2015-2020)",
    subtitle = "Top 5 sectors by professional category",
    x = "Year",
    y = "Share of Active Population (%)",
    color = "Sector",
    fill = "Sector",
    caption = "Shaded areas represent 95% confidence intervals. Each panel shows one professional category across the top 5 sectors."
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 7),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

# Display the plot
print(plot_graph4)

# Create output directory if it doesn't exist
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the plot
ggsave(
  file.path(output_dir, "graph4_categories_by_sector.png"),
  plot = plot_graph4,
  width = 14,
  height = 10,
  dpi = 300
)

cat("Graph 4 completed successfully!\n")
