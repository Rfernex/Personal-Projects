# Install and load required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidytext)) install.packages("tidytext")
if (!require(rvest)) install.packages("rvest")
if (!require(lubridate)) install.packages("lubridate")
if (!require(purrr)) install.packages("purrr")
if (!require(dplyr)) install.packages("dplyr")

# Load libraries
library(tidyverse)
library(tidytext)
library(rvest)
library(lubridate)
library(purrr)
library(dplyr)

# SCRAPING SPEECHES

# Sets output directory
output_dir <- "/Users/rfernex/Documents/Education/SciencesPo/Courses/CSS/Project BoJ/Data/processed"

# Retrieves main urls 
url_boj_speech_after2011 <- paste0("https://www.boj.or.jp/en/about/press/koen_",2011:2023,"/index.htm")
url_boj_speech_before2011 <- paste0("https://www2.boj.or.jp/archive/en/announcements/press/koen_",1998:2010,"/index.htm")

# Single function to collect URLs
collect_boj_speech_urls <- function(x, base_url) {
  page <- read_html(x)
  tibble(link = page |>
           html_elements(".js-tbl a") |>
           html_attr("href")) |>
    mutate(link = str_c(base_url, link))
}

# Process each set separately
boj_speech_before2011_urls <- map(url_boj_speech_before2011, 
                                  ~safely(collect_boj_speech_urls)(.x, "https://www2.boj.or.jp"), 
                                  .progress = T) %>%
  map_df("result")

boj_speech_after2011_urls <- map(url_boj_speech_after2011, 
                                 ~safely(collect_boj_speech_urls)(.x, "https://www.boj.or.jp"), 
                                 .progress = T) %>%
  map_df("result")





# Collects the content of each statement AFTER 2011 

collect_speech_content_a2011 <- function(x) {
  # Get HTML code of the page
  page <- read_html(x)
  
  tibble(
    # Keep the URL
    url = x,
    
    # Extract text using the #contents ID and all its paragraphs
    text = html_elements(page, "#contents p, #contents li") %>%
      html_text2() %>%
      paste(collapse = " "),
    
    # Extract date using the correct class and format
    date = html_elements(page, "#contents") %>%
      html_text2() %>%
      str_extract("(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}") %>%
      mdy()
  )
}

speech_content_a2011_df <- map(boj_speech_after2011_urls$link, safely(collect_speech_content_a2011), .progress = T) |> 
  map_df("result")

# Collects the content of each statement BEFORE 2011 

htm_links <- boj_speech_before2011_urls[grepl("\\.htm$", boj_speech_before2011_urls$link), , drop = FALSE] #retains only html links (some documents are in pdf)

collect_speech_content_b2011 <- function(x) {
  # Get HTML code of the page
  page <- read_html(x)
  
  # First get the intro paragraph that contains date and speaker
  intro_text <- page %>%
    html_elements("body") %>%
    html_text2() %>%
    str_extract("This article is excerpted and translated from.*?\\d{4}\\.")
  
  tibble(
    # Keep the URL
    url = x,
    
    # Extract text while excluding submenus (focus on main content only)
    text = html_elements(page, "#contents-skip p") %>%
      html_text2() %>%
      paste(collapse = " "),
    
    # Extract date from intro text
    date = html_elements(page, ".main > p") %>%
      html_text2() %>%
      str_extract("[A-Za-z]+(\\s|)\\d{1,2}, \\d{4}") %>%
      .[!is.na(.)] %>%
      .[1] %>%  # Take only the first match
      mdy()
  )
}

speech_content_b2011_df <- map(htm_links$link, safely(collect_speech_content_b2011), .progress = T) |> 
  map_df("result")

#test <-html_elements(read_html("https://www2.boj.or.jp/archive/en/announcements/press/koen_2010/ko1012e.htm"), ".main > p" ) %>%
#  html_text2() %>%
#  str_extract("[A-Za-z]+(\\s|)\\d{1,2}, \\d{4}") %>%
#  .[!is.na(.)] %>%
#  .[1]

# Concatenates both output tables
speech_content_df <- bind_rows(speech_content_b2011_df, speech_content_a2011_df) %>% 
  filter(!is.na(date)) %>%
  mutate(
    date = ymd(date),  # Use ymd() instead since dates are in YYYY-MM-DD format
    month_year = format(date, "%Y-%m")  # Create month-year as YYYY-MM
  ) %>%
  filter(!is.na(date))

# Exports output to csv
write.csv(speech_content_df, file.path(output_dir, "speech_boj_df.csv"), row.names = FALSE)
