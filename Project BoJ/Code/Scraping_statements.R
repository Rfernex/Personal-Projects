
# Install and load required packages
if (!requireNamespace("pdftools", quietly = TRUE)) {
  install.packages("pdftools")
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")
}

library(pdftools)
library(stringr)
library(httr)

# SCRAPING MONETARY POLICY STATEMENTS

output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/CSS/Project BoJ/Data/processed"

# Set main urls
url_boj_decision <- paste0("https://www.boj.or.jp/en/mopo/mpmdeci/state_",1998:2023,"/index.htm")

# Collect the urls of each decision statement
collect_boj_decision_urls <- function(x) {
  page <- read_html(x)
  urls <- tibble(link = page |>
                   html_elements(".js-tbl a") |>
                   html_attr("href")) |>
    mutate(link = str_c("https://www.boj.or.jp", link))
  return(urls) 
}

boj_decision_urls <- map(url_boj_decision, safely(collect_boj_decision_urls), .progress = T) |> 
  map_df("result")

# Splits the url matrix into two separate matrixes depending on whether scraping is done on a regular url or on a PDF
htm_links <- boj_decision_urls[grepl("\\.htm$", boj_decision_urls$link), , drop = FALSE]
pdf_links <- boj_decision_urls[grepl("\\.pdf$", boj_decision_urls$link), , drop = FALSE]


# Collect the content of each decision statement NO PDF
collect_decision_content <- function(x) {
  # Get html code of page
  page <- read_html(x)
  
  tibble(
    # Keep the URL
    url = x,
    
    # Extract text using the entry-content class and all its paragraphs
    text = html_elements(page, "#contents p, #contents li") %>%
      html_text2(),
    
    # Extract date using the correct class and format
    date = html_element(page, ".mod_outer > p:nth-child(1)") %>%
      html_text2() %>%
      str_extract("[A-Za-z]+ \\d{1,2}, \\d{4}") %>%
      mdy()
  )
}

# Scrape all statements and format the result table 
decision_content_HTM_df <- map(htm_links$link, safely(collect_decision_content), .progress = T) |> 
  map_df("result") %>%
  group_by(url) %>%
  summarise(all_text = paste(text, collapse = " ")) %>% mutate(
    date = str_extract(all_text, "[A-Za-z]+(\\s|)\\d{1,2}, \\d{4}"),
    all_text = str_replace(all_text, "^.*\\d{4}\\n", "") 
    ) %>% rename(text = all_text)

rm(htm_links)

# Collect the content of each decision statement PDF

# Function to extract date and text separately
extract_boj_content <- function(pdf_url) {
  # Create a temporary file
  temp_file <- tempfile(fileext = ".pdf")
  
  # Download PDF with proper SSL handling
  GET(pdf_url, write_disk(temp_file), config(ssl_verifypeer = FALSE))
  
  # Extract text from PDF
  tryCatch({
    # Read all pages
    pdf_text_content <- pdf_text(temp_file)
    
    # Combine all pages and clean initial whitespace
    full_text <- paste(pdf_text_content, collapse = "\n")
    full_text <- str_trim(full_text)
    
    # Extract date
    date <- str_extract(full_text, "\\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}\\b")
    
    # Clean the text:
    # 1. Remove multiple spaces
    clean_text <- str_replace_all(full_text, "\\s+", " ")
    # 2. Remove empty brackets
    clean_text <- str_replace_all(clean_text, "\\$$\\s*\\$$", "")
    # 3. Clean up newlines and extra spaces
    clean_text <- str_replace_all(clean_text, "\\n\\s*\\n", "\n")
    # 4. Properly escape dollar signs
    clean_text <- str_replace_all(clean_text, "\\$", "")
    
    # Structure the output (only date and text)
    result <- list(
      url = pdf_url,
      date = date,
      text = clean_text
    )
    
  }, error = function(e) {
    result <- list(
      url = pdf_url,
      date = NA,
      text = paste("Error in PDF extraction:", e$message)
    )
  })
  
  # Clean up
  unlink(temp_file)
  
  return(result)
}

# Scrape all statements
decision_content_PDF_df <- map(pdf_links$link, safely(extract_boj_content), .progress = T) |> 
  map_df("result") %>% mutate(text = str_replace(text, "[A-Za-z]+(\\s|)\\d{1,2},?\\s+\\d{4}", "")) %>% relocate(text, .before = date)

rm(pdf_links)

# Concatenates output tables
decision_content_df <- bind_rows(decision_content_HTM_df, decision_content_PDF_df, ) %>%  mutate(
  date = mdy(date),  # Convert to Date format
  month_year = format(date, "%Y-%m")  # Create month-year as YYYY-MM
) %>% arrange(month_year)

# Write CSV files to the specified directory
write.csv(decision_content_df, file.path(output_dir, "Statements_boj_df.csv"), row.names = FALSE)
