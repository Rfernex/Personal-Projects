
### PERFORMS A TF-IDF ANALYSIS ON THE SCRAPED DATA ###


# First, filter articles mentioning Kishida and sort by date
articles_Kishida_df <- combined_content_df %>% 
    filter(str_detect(text, "[Kk]ishida")) %>% 
    arrange(date)

# Then count occurrences per text mentioning Kishida and creates different groups depending on the number of occurences
articles_Kishida_df <- articles_Kishida_df %>%
    mutate(
        occurences = str_count(text, "[Kk]ishida"),
        mention_group = case_when(
            occurences > 10 ~ "High_threshold",
            occurences > 5 ~ "Medium_threshold",
            occurences > 3 ~ "Low_threshold",
            TRUE ~ "Very_low_threshold"
        )
    )

# Using tf-idf to find the right cutoff number for occurences of Kishida 

# Load required libraries

library(tidytext)
library(SnowballC)
library(stopwords)

# Tokenize the text
articles_Kishida_tokens <- articles_Kishida_df %>% 
    unnest_tokens(word, text, token = "words", drop = FALSE) %>%
    # Remove numbers and common stop words
    filter(!str_detect(word, "[:digit:]")) %>%
    filter(!word %in% stopwords::stopwords()) %>% filter(nchar(word) > 2)

## Test for identification of problematic words
summarized_tokens <- articles_Kishida_tokens %>% filter(occurences > 5) %>% filter(str_detect(word, "milt"))

# Calculate tf-idf with word counts
Kishida_tf_idf <- articles_Kishida_tokens %>% 
    count(mention_group, word) %>% 
    bind_tf_idf(word, mention_group, n) 

# Create the visualization
Kishida_tf_idf_plot <- Kishida_tf_idf %>% 
    filter(n >= 2) %>% # Filter out words that appear less than 2 times (you can adjust this threshold)
    group_by(mention_group) %>%
    arrange(desc(tf_idf)) %>%  # Sort within groups
    slice(1:15) %>%  
    ggplot(aes(tf_idf, fct_reorder(word, tf_idf))) +
    geom_col(aes(fill = mention_group)) +
    # Add count labels to the end of each bar
    geom_text(aes(label = paste0("(n=", n, ")"), x = tf_idf + 0.01), 
              hjust = 0, size = 3) +
    facet_wrap(~mention_group, ncol = 1, scales = "free") +
    labs(
        x = "TF-IDF Score", 
        y = "Words",
        title = "Most Distinctive Words by Kishida Mention Frequency",
        subtitle = "Word count shown in parentheses",
        fill = "Mention Group"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "cm")
    ) +
    scale_fill_brewer(palette = "Set2")

Kishida_tf_idf_plot

# Save as high-quality PNG
ggsave(
    "kishida_tf_idf_analysis_with_counts.png",
    plot = Kishida_tf_idf_plot,
    width = 12,
    height = 15,
    dpi = 300,
    bg = "white"
)