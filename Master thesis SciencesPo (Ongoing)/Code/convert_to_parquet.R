# One-time conversion: .dta to .parquet
# This script converts the large Stata file to Parquet format for faster loading
# Uses parquetize for optimized conversion

cat("========================================\n")
cat("Converting .dta to .parquet format\n")
cat("========================================\n\n")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(parquetize)

# Input and output paths
dta_path <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Data (CONFIDENTIAL)/EEmploi_2002_2013/eec2010_2024.dta"
parquet_path <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/M2/Master thesis/Data (CONFIDENTIAL)/EEmploi_2002_2013/eec2010_2024.parquet"

cat("Converting Stata file to Parquet format...\n")
cat("This may take 20-40 minutes for a 23GB file...\n\n")
start_time <- Sys.time()

# Direct conversion using parquetize - more memory efficient
table_to_parquet(
  path_to_file = dta_path,
  path_to_parquet = parquet_path,
  compression = "snappy"
)

write_time <- Sys.time()
cat(paste("\n  Conversion completed in", round(difftime(write_time, start_time, units = "mins"), 1), "minutes\n\n"))

# Check file sizes
dta_size <- file.info(dta_path)$size / 1024^3  # GB
parquet_size <- file.info(parquet_path)$size / 1024^3  # GB

cat("========================================\n")
cat("Conversion completed!\n")
cat(paste("Original .dta size:", round(dta_size, 2), "GB\n"))
cat(paste("Parquet size:", round(parquet_size, 2), "GB\n"))
cat(paste("Compression ratio:", round(dta_size / parquet_size, 2), "x\n"))
cat(paste("Total time:", round(difftime(write_time, start_time, units = "mins"), 1), "minutes\n"))
cat("========================================\n\n")
