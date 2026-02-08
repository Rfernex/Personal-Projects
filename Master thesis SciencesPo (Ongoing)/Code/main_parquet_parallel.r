# Load required packages
cat("========================================\n")
cat("Starting analysis pipeline (PARQUET VERSION)\n")
cat(paste("Start time:", Sys.time(), "\n"))
cat("========================================\n\n")

cat("[1/7] Loading packages...\n")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(arrow, haven, dplyr, survey, ggplot2, tidyr, scales, lubridate, xtable)
# Note: patchwork and gridExtra will be loaded by graph2_wage_self_only.R when needed
cat("      Packages loaded successfully!\n\n")

# Read Parquet file
cat("[2/7] Reading Parquet file...\n")
start_read <- Sys.time()
df_arrow <- read_parquet("/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Data (CONFIDENTIAL)/EEmploi_2002_2013/eec2010_2024.parquet",
                         as_data_frame = FALSE) |>  # Keep as arrow table
  rename(
    trimester = trim,
    individual = noi,
    household = ident
  ) |>
  select(
    nafemp2n, extrid, mchpub, fchpub, annee, achpub, nbtot, emp3nbh, emp2nbh,
    uneprof, schpubc, schpubb, schpuba, autoentc, autoentb, sexe, autoenta,
    nafemp3g010n, nafemp2g010n, chpubc, chpubb, plract, nafg010n, autoent,
    creaccp, hhc, empnbh, tam1b, extri, household, statutr, statut, stat2,
    stnred, sousempl, lnais, tppred, empnbh6, hhc6, cser, acteu, ddipl, totnbh,
    stplc, stche, stc, statuts4, statuts3, statuts2, soub, soua, sou, raistp,
    nbtemp, echpub, dispplc, cstplc, creacc, chpubs4, chpubs3, chpubs2,
    chpub3, chpub2, chpub, autsal, am2nb, am1nb, age, nafemp3n, trimester,
    individual, inform, nafemp2g004n, nafg004n, css2, css3, css4,
    paiperc, csp, age3
  ) |>
  mutate(
    date = as.Date(paste0(annee, "-", (trimester - 1) * 3 + 1, "-01"))
  )
end_read <- Sys.time()
cat(paste("      Data loaded in", round(difftime(end_read, start_read, units = "secs"), 1), "seconds\n\n"))

# Create categorical variables (Arrow-optimized, already efficient)
cat("[3/7] Creating categorical variables...\n")
start_vars <- Sys.time()

df_arrow <- df_arrow |>
  mutate(
    plur_cat = case_when(
      plract != 1 ~ 0L,
      plract == 1 & uneprof == 2 & (nbtemp == 1 | is.na(nbtemp)) ~ 1L,
      plract == 1 & uneprof == 1 & nbtemp %in% c(2, 3) ~ 2L,
      plract == 1 & uneprof == 2 & nbtemp %in% c(2, 3) ~ 3L,
      plract == 1 ~ 99L,
      TRUE ~ NA_integer_
    ),
    plur_cat = factor(plur_cat,
      levels = c(0, 1, 2, 3, 99),
      labels = c("Not Pluriactive", "Multi-Job/Single Emp", "Single Job/Multi-Emp",
                 "Multi-Job/Multi-Emp", "Undefined Pluriactive")
    )
  ) |>
  mutate(
    plur_type = case_when(
      plract != 1 ~ 0L,
      plract == 1 & uneprof == 1 & nbtemp %in% c(2, 3) ~ 1L,
      plract == 1 & uneprof == 2 & stc == 3 &
        (statuts2 %in% c(1, 2) | statuts3 %in% c(1, 2) | statuts4 %in% c(1, 4)) ~ 2L,
      plract == 1 & uneprof == 2 & stc %in% c(1, 2) &
        (statuts2 %in% c(1, 2) | statuts3 %in% c(1, 2) | statuts4 %in% c(1, 4)) ~ 4L,
      plract == 1 ~ 99L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    plur_type = case_when(
      plur_type == 99 & plract == 1 & uneprof == 2 & stc %in% c(1, 2) &
        ((!is.na(statuts2) & statuts2 == 3) | (!is.na(statuts3) & statuts3 == 4) |
         (!is.na(statuts4) & statuts4 == 4)) ~ 1L,
      plur_type == 99 & plract == 1 & uneprof == 2 & stc == 3 &
        ((!is.na(statuts2) & statuts2 == 3) | (!is.na(statuts3) & statuts3 == 4) |
         (!is.na(statuts4) & statuts4 == 4)) ~ 3L,
      TRUE ~ plur_type
    ),
    plur_type = factor(plur_type,
      levels = c(0, 1, 2, 3, 4, 99),
      labels = c("Not Pluriactive", "Wage + Wage", "Wage + Self",
                 "Self + Wage", "Self + Self", "Undefined Pluriactive")
    )
  ) |>
  mutate(
    plur_type_INSEE = case_when(
      plract != 1 ~ 0L,
      plract == 1 & uneprof == 1 & nbtemp %in% c(2, 3) ~ 1L,
      plract == 1 & uneprof == 2 & stat2 == 2 &
        (statuts2 %in% c(1, 2, 4) | statuts3 %in% c(1, 2, 4) | statuts4 %in% c(1, 2, 4)) ~ 2L,
      plract == 1 & uneprof == 2 & stat2 == 1 &
        (statuts2 %in% c(1, 2, 4) | statuts3 %in% c(1, 2, 4) | statuts4 %in% c(1, 2, 4)) ~ 4L,
      plract == 1 ~ 99L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    plur_type_INSEE = case_when(
      plur_type_INSEE == 99 & plract == 1 & uneprof == 2 & stat2 == 2 &
        as.numeric(as.character(plur_type)) == 99 &
        (!is.na(statuts2) | !is.na(statuts3) | !is.na(statuts4)) ~ 1L,
      plur_type_INSEE == 99 & plract == 1 & uneprof == 2 & stat2 == 1 &
        as.numeric(as.character(plur_type)) == 99 &
        (!is.na(statuts2) | !is.na(statuts3) | !is.na(statuts4)) ~ 3L,
      TRUE ~ plur_type_INSEE
    ),
    plur_type_INSEE = factor(plur_type_INSEE,
      levels = c(0, 1, 2, 3, 4, 99),
      labels = c("Not Pluriactive", "Salaried + Salaried", "Salaried + Non-Salaried",
                 "Non-Salaried + Salaried", "Non Salaried + Non Salaried", "Undefined Pluriactive")
    )
  ) |>
  mutate(
    plur_ae_type = case_when(
      plract != 1 ~ 0L,
      plract == 1 & autoent == 2 & (autoenta == 1 | autoentb == 1 | autoentc == 1) ~ 2L,
      plract == 1 & autoent == 1 & (autoenta == 1 | autoentb == 1 | autoentc == 1) ~ 4L,
      plract == 1 ~ 99L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    plur_ae_type = case_when(
      plur_ae_type == 99 & plract == 1 & autoent == 2 ~ 1L,
      plur_ae_type == 99 & plract == 1 & autoent == 1 ~ 3L,
      TRUE ~ plur_ae_type
    ),
    plur_ae_type = factor(plur_ae_type,
      levels = c(0, 1, 2, 3, 4, 99),
      labels = c("Non-Pluriactive", "Other+Other", "Other+Entrepreneur",
                 "Entrepreneur+Other", "Entrepreneur+Entrepreneur", "Undefined")
    )
  )

end_vars <- Sys.time()
cat(paste("      Variables created in", round(difftime(end_vars, start_vars, units = "secs"), 1), "seconds\n\n"))

# Create entrepreneur type categories (mutually exclusive, evaluated in order)
cat("[3.5/7] Creating entrepreneur type categories...\n")
df_arrow <- df_arrow |>
  mutate(
    entrepreneur_type = case_when(
      # Polyentrepreneurs: primary AE + secondary AE (checked FIRST to avoid double counting)
      autoent == 1 & (autoenta == 1 | autoentb == 1 | autoentc == 1) ~ "Polyentrepreneur",
      # Hybrid Entrepreneurs: (non-AE primary + AE secondary) OR (AE primary + pluriactive but not polyentrepreneur)
      (autoent == 2 & (autoenta == 1 | autoentb == 1 | autoentc == 1)) |
      (autoent == 1 & plract == 1) ~ "Hybrid Entrepreneur",
      # Pure Entrepreneurs: AE primary, not pluriactive, not polyentrepreneur
      autoent == 1 ~ "Pure Entrepreneur",
      # Not an entrepreneur
      TRUE ~ "Not Entrepreneur"
    )
  )
cat("      Entrepreneur types created!\n\n")

cat("[4/7] Generating graphs and tables...\n")
cat("      Running scripts sequentially to manage memory\n\n")

# Define script directory and list
script_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Code/R_Code_New"
all_scripts <- c(
  "graph1_pluriactivity_types.R",
  "graph2_wage_self_only.R",
  "graph3_autoentrepreneurs.R",
  "table_entrepreneurs_by_sector.R",
  "graph4_categories_by_sector.R",
  "table_entrepreneur_characteristics.R",
  "graph5_gender_trends.R",
  "graph_entrepreneur_types.R",
  "transition_analysis.R"
)

# Run scripts sequentially
start_graphs <- Sys.time()
for (script in all_scripts) {
  cat(paste0("   Running ", script, "...\n"))
  script_path <- file.path(script_dir, script)
  source(script_path)
}
end_graphs <- Sys.time()

cat(paste("\n      All graphs/tables completed in",
          round(difftime(end_graphs, start_graphs, units = "mins"), 1), "minutes\n\n"))

cat("\n========================================\n")
cat("Pipeline completed successfully!\n")
cat(paste("End time:", Sys.time(), "\n"))
cat(paste("Total execution time:", round(difftime(Sys.time(), start_read, units = "mins"), 1), "minutes\n"))
cat("========================================\n")
