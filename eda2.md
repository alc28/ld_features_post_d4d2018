---
title: "eda2"
author: "Adam Chandler"
date: "3/1/2018"
output: 
  html_document: 
    keep_md: yes
---




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
names(ld_survey) <- c("response_id", "ld_features", "reviewer1_features_score", "reviewer2_features_score", "reviewer3_features_score", "reviewer4_features_score", "features_total_score")
```


# Highest score


```r
ld_survey %>%
  top_n(1, wt = features_total_score) %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> response_id </th>
   <th style="text-align:left;"> ld_features </th>
   <th style="text-align:right;"> reviewer1_features_score </th>
   <th style="text-align:right;"> reviewer2_features_score </th>
   <th style="text-align:right;"> reviewer3_features_score </th>
   <th style="text-align:right;"> reviewer4_features_score </th>
   <th style="text-align:right;"> features_total_score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> R_2UhZKI64KvRAZO4 </td>
   <td style="text-align:left;"> 15cBOOKTRADE is using linked data to create maps to show the distribution of early printed resources. Library users will be able to trace libraries of individual collectors, or trace the paths individual books from their printing to the present.
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
</tbody>
</table>

# Lowest scores



```r
ld_survey %>%
  top_n(-1, wt = features_total_score) %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> response_id </th>
   <th style="text-align:left;"> ld_features </th>
   <th style="text-align:right;"> reviewer1_features_score </th>
   <th style="text-align:right;"> reviewer2_features_score </th>
   <th style="text-align:right;"> reviewer3_features_score </th>
   <th style="text-align:right;"> reviewer4_features_score </th>
   <th style="text-align:right;"> features_total_score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> R_25BgvIUv1wkD1va </td>
   <td style="text-align:left;"> I don't know of any. </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R_3HNsyo6HY6bKhcr </td>
   <td style="text-align:left;"> I cannot provide two useful examples. </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R_1r1Gmn7yuNTS9ma </td>
   <td style="text-align:left;"> Nothing more than they can already do now.  This is a clear case of the emperor having no clothing, and/or a giant bill of goods being sold to unsuspecting and tech ignorant library personnel. </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R_2wzzmuRKCzj7pGz </td>
   <td style="text-align:left;"> None.
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R_1MXvzZQHnCaFu1s </td>
   <td style="text-align:left;"> I do not think Wikipedia is a reliable source of information, so I would not connect patrons to that resource. </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R_2ePgUE1yRGs6fbY </td>
   <td style="text-align:left;"> Get drowned in irrelevant data. See #2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R_2ctPumKlvoRc9S8 </td>
   <td style="text-align:left;"> Not something I would do. </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>