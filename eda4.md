---
title: "eda4 - Bigrams"
author: "Adam Chandler"
date: "3/1/2018"
output: 
  html_document: 
    keep_md: yes
---



# Prepare data


```r
# Load libraries

library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library(tidytext)
library(knitr)
library(stringr)
library(kableExtra)
data("stop_words")
library(igraph)
```

```
## 
## Attaching package: 'igraph'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     as_data_frame, groups, union
```

```
## The following objects are masked from 'package:purrr':
## 
##     compose, simplify
```

```
## The following object is masked from 'package:tidyr':
## 
##     crossing
```

```
## The following object is masked from 'package:tibble':
## 
##     as_data_frame
```

```
## The following objects are masked from 'package:stats':
## 
##     decompose, spectrum
```

```
## The following object is masked from 'package:base':
## 
##     union
```

```r
library(ggraph)


# load libraries
# Remove responses with 0 feature total score and normalize(i.e., linked data)
clean_features_text <- function(ld_survey) {
  ld_survey_nonzero <- ld_survey %>%
    filter(features_total_score > 0)
  ld_survey_cleaned <- ld_survey_nonzero
  ld_survey_cleaned$ld_features <- str_replace_all(ld_survey_cleaned$ld_features, "linked data", "linked_data")
  ld_survey_cleaned$ld_features <- str_replace_all(ld_survey_cleaned$ld_features, "linked open data", "linked_data")
  ld_survey_cleaned$ld_features <- str_replace_all(ld_survey_cleaned$ld_features, "lod", "linked_data")
  return(ld_survey_cleaned)
}

# Load data
ld_survey <- read_csv("data/ld_survey_anonymized_20180301.csv")
```

```
## Parsed with column specification:
## cols(
##   response_id = col_character(),
##   linked_data_benefits = col_character(),
##   reviewer1_benefits_quality = col_integer(),
##   reviewer2_benefits_quality = col_integer(),
##   reviewer3_benefits_quality = col_integer(),
##   reviewer4_benefits_quality = col_integer(),
##   features_score_total = col_integer()
## )
```

```r
names(ld_survey) <- c("response_id", "ld_features", "rev1_score", "rev2_score", "rev3_score", "rev4_score", "features_total_score")

# clean data
ld_survey_cleaned <- clean_features_text(ld_survey)
```


# Create bigrams (after removing stop words)


```r
bigrams <- ld_survey_cleaned %>%
  unnest_tokens(bigram, ld_features, token = "ngrams", n = 2) %>% 
  select(response_id, bigram) %>% 
  arrange(response_id)

bigrams_count <- bigrams %>%
  count(bigram, sort = TRUE)

bigram_counts_separated <- bigrams %>%
  select(bigram) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

bigram_counts_separated %>% 
  filter(str_detect(word1, "linked_data" ) | str_detect(word2, "linked_data"))
```

```
## # A tibble: 22 x 3
##          word1       word2     n
##          <chr>       <chr> <int>
##  1 linked_data environment     2
##  2 linked_data       users     2
##  3   pervasive linked_data     2
##  4      austen linked_data     1
##  5      enable linked_data     1
##  6  government linked_data     1
##  7    leverage linked_data     1
##  8  leveraging linked_data     1
##  9 linked_data     applied     1
## 10 linked_data       based     1
## # ... with 12 more rows
```

```r
bigram_counts_separated
```

```
## # A tibble: 929 x 3
##        word1       word2     n
##        <chr>       <chr> <int>
##  1    linked        data    15
##  2   library       users    11
##  3   library collections     8
##  4    search     engines     7
##  5    search     results     5
##  6   special collections     5
##  7 authority     records     4
##  8 knowledge       graph     4
##  9    linked        jazz     4
## 10   subject    headings     4
## # ... with 919 more rows
```

# Graph bigrams


```r
bigram_graph <- bigram_counts_separated %>%
  filter(n > 1) %>%
  graph_from_data_frame()


set.seed(2017)
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.5) +
  theme_void()
```

![](eda4_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
ggsave(plot = last_plot(), filename = "output/ld_study_bigraph.png", height = 7, width = 10, units = "in", dpi = 300)
```


