# Graph 5: Gender Differential Trends in Pluriactivity (2015-2020)
# This script plots the evolution of 4 pluriactivity types by gender
# Uses arrow for efficient data processing

cat("Generating gender differential trends plot...\n")

# Filter and aggregate by gender in arrow
results_gender <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    plur_type %in% c("Wage + Wage", "Wage + Self", "Self + Wage", "Self + Self"),
    !is.na(sexe),
    sexe %in% c(1, 2)
  ) |>
  mutate(
    gender = case_when(
      sexe == 1 ~ "Men",
      sexe == 2 ~ "Women",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(date, gender, plur_type) |>
  summarise(count = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()

# Calculate total active population by gender and date
total_active_gender <- df_arrow |>
  filter(
    acteu == 1,
    date >= as.Date("2015-01-01"),
    date <= as.Date("2020-12-31"),
    !is.na(sexe),
    sexe %in% c(1, 2)
  ) |>
  mutate(
    gender = case_when(
      sexe == 1 ~ "Men",
      sexe == 2 ~ "Women",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(date, gender) |>
  summarise(total = sum(extri, na.rm = TRUE), .groups = "drop") |>
  collect()

# Join and calculate shares
results_gender <- results_gender |>
  left_join(total_active_gender, by = c("date", "gender")) |>
  mutate(
    share = count / total * 100,
    se = sqrt(share * (100 - share) / total) * 1.96  # 95% CI
  )

# Create the faceted plot by gender
plot_gender <- ggplot(results_gender, aes(x = date, y = share, color = plur_type, fill = plur_type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = share - se, ymax = share + se), alpha = 0.2, color = NA) +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = as.Date("2018-01-01"), y = Inf, label = "Micro reform",
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  facet_wrap(~gender, ncol = 2) +
  labs(
    title = "Gender Differential Trends in Multiple Jobholding (2015-2020)",
    subtitle = "Share of active population by pluriactivity type and gender",
    x = "Year",
    y = "Share of Active Population (%)",
    color = "Pluriactivity Type",
    fill = "Pluriactivity Type",
    caption = "Shaded areas represent 95% confidence intervals."
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c(
    "Wage + Wage" = "#4DAF4A",
    "Wage + Self" = "#984EA3",
    "Self + Wage" = "#FF7F00",
    "Self + Self" = "#A65628"
  )) +
  scale_fill_manual(values = c(
    "Wage + Wage" = "#4DAF4A",
    "Wage + Self" = "#984EA3",
    "Self + Wage" = "#FF7F00",
    "Self + Self" = "#A65628"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    plot.caption = element_text(hjust = 0, size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(color = guide_legend(nrow = 2))

# Display the plot
print(plot_gender)

# Create output directory if it doesn't exist
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the plot
ggsave(
  file.path(output_dir, "graph5_gender_trends.png"),
  plot = plot_gender,
  width = 14,
  height = 7,
  dpi = 300
)

cat("Graph 5 completed successfully!\n")
