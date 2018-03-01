---
title: "eda1"
author: "Adam Chandler"
date: "3/1/2018"
output: 
  html_document: 
    keep_md: yes
---




```r
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
library(readr)
```



```r
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
glimpse(ld_survey)
```

```
## Observations: 88
## Variables: 7
## $ response_id                <chr> "R_3RwFMkqFX9JmfW1", "R_2UhZKI64KvR...
## $ linked_data_benefits       <chr> "1) If a research topic has locatio...
## $ reviewer1_benefits_quality <int> 2, 3, 3, 2, 1, 2, 1, 2, 1, 1, 0, 0,...
## $ reviewer2_benefits_quality <int> 2, 2, 1, 2, 0, 1, 1, 2, 2, 2, 2, 2,...
## $ reviewer3_benefits_quality <int> 2, 3, 1, 3, 2, 1, 1, 2, 1, 1, 2, 4,...
## $ reviewer4_benefits_quality <int> 2, 4, 4, 3, 3, 2, 2, 3, 1, 1, 1, 2,...
## $ features_score_total       <int> 8, 12, 9, 10, 6, 6, 5, 9, 5, 5, 5, ...
```

