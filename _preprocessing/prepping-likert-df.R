###############################################################################
###############################################################################
## 121 - India Sociolinguistics (isl) Analyses
## Ruthe Foushee
## 17 August 2023, for Cognitive Development submission
## Subset data, write XXXXXX.csv
###############################################################################
###############################################################################
library(here)
here::i_am(here('0_preprocessing/prepping-associations-df.R'))

### load libraries and functions
source(here('isl-resources.R'))
###############################################################################
### read in data files
# hand-entered and lightly cleaned data with demographic info
d <- read.csv(here('data/r_dfs/india_socioling_data_final.csv'))
###############################################################################
p <- d %>% filter(question_type == "likert")

write.csv(, here('data/r_dfs/_data.csv'))

