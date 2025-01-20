# SCRAPING SURVEY TABLES

# Install and load required packages
if (!requireNamespace("rvest", quietly = TRUE)) {
  install.packages("rvest")}
if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")}

library(rvest)
library(httr)
library(dplyr)

# Set the webpage URL
webpage_url <- "https://www.boj.or.jp/en/research/o_survey/index.htm"
output_directory <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/CSS/Project BoJ/Data/pre-processed"  # Specify your directory

# Read the webpage and get the first CSV link
webpage <- read_html(webpage_url)

# Extract all links and get the first CSV one
first_csv_link <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  .[grepl("\\.csv$", ., ignore.case = TRUE)] %>%
  .[1]

# Make the link absolute if it's relative
if (!grepl("^http", first_csv_link)) {
  first_csv_link <- paste0("https://www.boj.or.jp", first_csv_link)
}

# Download the CSV file
file_name <- basename(first_csv_link)
destfile <- file.path(output_directory, file_name)

tryCatch({
  GET(first_csv_link, write_disk(destfile, overwrite = TRUE))
  message("Downloaded: ", destfile)
}, error = function(e) {
  message("Error downloading file: ", e$message)
})

# Import CSV files with Japanese encoding (Shift-JIS)
survey_table <- read_csv(file.path(output_directory, "survey03.csv"), 
                         locale = locale(encoding = "SHIFT-JIS"))

#formats the matrix to remove useless rows and columns 
filtered_rows <- survey_table %>%
  filter(row_number() %in% c(1,5) | grepl("^\\d{4}", ...1))  
filtered_survey_table <- filtered_rows[, c(2,5:30)] %>%   select_if(~!all(is.na(.)))
rm(filtered_rows)

# Create function to extract question dataframe
create_question_df <- function(data, cols) {
  # Extract relevant columns including date column
  question_df <- data[, c(1, cols)]
  
  # Get the headers from row 2
  headers <- as.character(question_df[2, ])
  
  # Replace NA in first column with "Dates"
  headers[1] <- "Dates"
  
  # Set the column names
  colnames(question_df) <- make.names(headers, unique = TRUE)
  
  # Remove rows 1 and 2 (question and options)
  question_df <- question_df[-(1:2), ]
  
  # Keep only rows with years and remove NA rows
  question_df <- question_df %>%
    filter(grepl("^\\d{4}", Dates)) %>%
    filter(!is.na(Dates))
  
  # Remove any columns that are all NA
  question_df <- question_df %>%
    select_if(~!all(is.na(.)))
  
  # Remove any row that contains NA or are empty
  question_df <- question_df %>%
    filter_all(any_vars(!is.na(.) & . != "-"))
  
  return(question_df)
}

# Create separate dataframes for each question
past_situation_df <- create_question_df(filtered_survey_table, 2:4) %>%
  mutate(
    month_year = format(parse_date_time(Dates, "Y m!"), "%Y-%m")
  ) %>%
  select(month_year, everything(), -Dates) %>%  # Move month_year first and drop Dates
  arrange(month_year)

current_situation_df <- create_question_df(filtered_survey_table, 12:16) %>%
  mutate(
    month_year = format(parse_date_time(Dates, "Y m!"), "%Y-%m")
  ) %>%
  select(month_year, everything(), -Dates) %>%  # Move month_year first and drop Dates
  arrange(month_year)

future_situation_df <- create_question_df(filtered_survey_table, 17:19)%>%
  mutate(
    month_year = format(parse_date_time(Dates, "Y m!"), "%Y-%m")
  ) %>%
  select(month_year, everything(), -Dates) %>%  # Move month_year first and drop Dates
  arrange(month_year)

perception_rates_df <- create_question_df(filtered_survey_table, 21:22)%>%
  mutate(
    month_year = format(parse_date_time(Dates, "Y m!"), "%Y-%m")
  ) %>%
  select(month_year, everything(), -Dates) %>%  # Move month_year first and drop Dates
  arrange(month_year)

rm(webpage)

output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/CSS/Project BoJ/Data/processed"

# Write CSV files to the specified directory
write.csv(past_situation_df, file.path(output_dir, "survey_past_situation_df.csv"), row.names = FALSE)
write.csv(current_situation_df, file.path(output_dir, "survey_current_situation_df.csv"), row.names = FALSE)
write.csv(future_situation_df, file.path(output_dir, "survey_future_situation_df.csv"), row.names = FALSE)
write.csv(perception_rates_df, file.path(output_dir, "survey_perception_rates_df.csv"), row.names = FALSE)


