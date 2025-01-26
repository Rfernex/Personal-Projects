
install.packages(tydiverse)
install.packages(tidytext)

### RETRIEVES DATA FROM NEWSPAPERS ###

#retrieves pages urls

asahi_pol_urls <- str_c("https://www.asahi.com/ajw/politics/", c(6:58))
yomiuri_pol_urls <- str_c("https://japannews.yomiuri.co.jp/politics/politics-government/page/", c(16:206),"/")
sankei_pol_urls <- str_c("https://japan-forward.com/category/politics-security/page/", c(14:172), "/")

# retrieves article urls 

## Asahi Shimbun

# Function to collect article URLs
collect_asahi_urls <- function(x) {
    page <- read_html(x)
    
    # Extract links that match the article pattern
    links <- page %>%
        html_elements("a") %>%  # Select all anchor tags
        html_attr("href") %>%  # Extract href attributes
        unique() %>%  # Remove duplicates
        .[str_detect(., "^/ajw/articles/")]  # Filter for article links
    
    # Create a tibble and add the domain to complete the URLs
    link_tibble <- tibble(link = links) %>%
        mutate(link = paste0("https://www.asahi.com", link))
    
    return(link_tibble)
}
#creates a list of article urls
article_asahi_urls <- map(asahi_pol_urls, safely(collect_asahi_urls), .progress = T) |> 
    map_df("result")

##Yomiuri Shimbun

collect_yomiuri_urls <- function(x) {
    page <- read_html(x)
    tibble(link = page %>%
               html_elements("main a") %>%  # Get all links in main
               html_attr("href") %>%
               .[str_detect(., "^https://japannews\\.yomiuri\\.co\\.jp/politics/politics-government/\\d{8}-\\d+")])  # Filter for article URLs
}

article_yomiuri_urls <- map(yomiuri_pol_urls, safely(collect_yomiuri_urls), .progress = T) %>%
    map_df("result") %>%
    distinct(link, .keep_all = TRUE)

## Sankei Shimbun

collect_sankei_urls <- function(x) {
    page <- read_html(x)
    
    # Only get links from articles in the main content area
    link <- tibble(link = page %>%
                       html_elements("main#main article h4 > a") %>%  # Specifically target main content articles
                       html_attr("href"))
    return(link)
}

article_sankei_urls <- map(sankei_pol_urls, safely(collect_sankei_urls), .progress = T) |> 
    map_df("result")

# retrieves content from the articles

## Asahi Shimbun

collect_Asahi_content <- function(x) {
    page <- read_html(x)
    
    tibble(
        # Title extraction remains the same
        title = html_element(page, ".ArticleTitle") %>%
            html_text2() %>%
            str_extract("^[^\\n]+") %>%
            str_trim(),
        
        # Refined text extraction
        text = html_elements(page, ".ArticleText p") %>%  # Focus on paragraphs within ArticleText class
            html_text2() %>%
            # Enhanced filtering of unwanted text - removed problematic escape sequences
            .[!str_detect(., paste0(
                "^(Share|Tweet|Print|THE ASAHI SHIMBUN|This article|Photo by|[(]AP Photo[)]|",
                "[(]Provided by|[(]Captured from|Image:|File photo|Asahi Shimbun file photo)"
            ))] %>%
            # Remove paragraphs that are within image containers - simplified pattern
            .[!map_lgl(., ~any(str_detect(., "^[A-Z][a-z]+ by|^Photo")))] %>%
            # Remove empty paragraphs and those that are too short (likely captions)
            .[nchar(.) > 50] %>%
            paste(collapse = " "),
        
        # Improved date extraction to handle multiple formats
        date = html_element(page, ".EnLastUpdated") %>%  # Changed to specific date class
            html_text2() %>%
            str_extract("[A-Za-z]+ \\d{1,2}, \\d{4}") %>%  # More flexible date pattern
            parse_date_time(orders = c("mdy")) %>%  # Using parse_date_time instead of mdy
            as.Date()  # Convert to Date format
    )
}

asahi_content_df <- map(article_asahi_urls$link, safely(collect_Asahi_content), .progress = T) |> 
    map_df("result")

## Yomiuri Shimbun

collect_yomiuri_content <- function(x) {
    # Get html code of page
    page <- read_html(x)
    
    tibble(
        # Extract title - using direct h1 selector
        title = html_element(page, "h1") %>%
            html_text2(),
        
        # Extract text - using p elements and filtering unwanted content
        text = html_elements(page, "p") %>%
            html_text2() %>%
            # Filter out unwanted paragraphs
            .[!str_detect(., paste0(
                "^(Please disable|To use this site|This website uses|Users accessing|By clicking|Accept all|",
                "The Japan News|© 2025|Our weekly)"
            ))] %>%
            # Remove byline
            .[!str_detect(., "^By .* / Yomiuri Shimbun")] %>%
            # Remove date line
            .[!str_detect(., "^\\d{2}:\\d{2} JST")] %>%
            # Remove empty paragraphs and those that are too short
            .[nchar(.) > 20] %>%
            paste(collapse = " "),
        
        # Extract date - using postdate class and proper format
        date = html_element(page, ".postdate") %>%
            html_text2() %>%
            str_extract("[A-Za-z]+ \\d{1,2}, \\d{4}") %>%
            mdy()
    )
}

yomiuri_content_df <- map(article_yomiuri_urls$link, safely(collect_yomiuri_content), .progress = T) |> 
    map_df("result")

## Sankei Shimbun

collect_sankei_content <- function(x) {
    # Get html code of page
    page <- read_html(x)
    
    tibble(
        # Extract title using the correct class
        title = html_element(page, "h1.entry-title") %>%
            html_text2(),
        
        # Extract text using the entry-content class and all its paragraphs
        text = html_elements(page, ".entry-content p") %>%
            html_text2() %>%
            # Filter out short paragraphs
            .[nchar(.) > 20] %>%
            paste(collapse = " "),
        
        # Extract date using the correct class and format
        date = html_element(page, ".item-metadata.posts-date") %>%
            html_text2() %>%
            mdy()
    )
}

sankei_content_df <- map(article_sankei_urls$link, safely(collect_sankei_content), .progress = T) |> 
    map_df("result")

# concatenate results in a single matrix that contains all three journals

asahi_content_df$source <- "Asahi"
yomiuri_content_df$source <- "Yomiuri"
sankei_content_df$source <- "Sankei"

combined_content_df <- bind_rows(asahi_content_df, yomiuri_content_df, sankei_content_df) %>%
    mutate(
        id = str_c(source, "_", date, "_", row_number())
    )

#exports data to a separate folder for further analysis on python

# First, create the complete path correctly
current_dir <- getwd()  # Gets "/Users/rfernex/Documents/Education/SciencesPo/Courses/CSS/CSS course project"
export_dir <- file.path(current_dir, "Personal project", "Raw_Data")

# Export the files using the complete path
write.csv(asahi_content_df, file.path(export_dir, "Asahi.csv"), row.names = FALSE)
write.csv(yomiuri_content_df, file.path(export_dir, "Yomiuri.csv"), row.names = FALSE)
write.csv(sankei_content_df, file.path(export_dir, "Sankei.csv"), row.names = FALSE)
write.csv(combined_content_df, file.path(export_dir, "combined.csv"), row.names = FALSE)

# Print confirmation
cat("Files exported to:", export_dir)

# generates a random sample for labeling

# Set seed for reproducibility
set.seed(123)


# Define proportions


# Sample rows by category
sampled_data <- do.call(rbind, lapply(names(proportions), function(cat) {
    subset_data <- data[data$Category == cat, ]
    subset_data[sample(nrow(subset_data), round(nrow(subset_data) * proportions[cat])), ]
}))