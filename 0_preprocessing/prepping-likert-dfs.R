###############################################################################
###############################################################################
## 121 - India Sociolinguistics (isl) Analyses
## Clean manually-entered data for likert-scale items and create new dfs
## Ruthe Foushee
## 17 August 2023, for Cognitive Development submission
## Subset data, reverse-code, write likert_item_data.csv and _wide counterpart
###############################################################################
###############################################################################
library(here)
here::i_am('0_preprocessing/prepping-likert-dfs.R')

### load libraries and functions
source(here('isl-resources.R'))
###############################################################################
### read in data files
# hand-entered and lightly cleaned data with demographic info
d <- read.csv(here('data/r_dfs/full_cleaned/india_socioling_data_final.csv'))
###############################################################################
likert_questions <- c("french", "tell_a_lot", "tell_education", 
                      "tell_where_from", "ancestors", "indian")

essentialism_questions <- c("french", "ancestors", "indian")

nonessentialism_questions <- c("tell_a_lot", "tell_education", 
                               "tell_where_from")

l <- d %>% filter(question_type %in% likert_questions) %>%
  dplyr::select(c("study_name", "school", "id", "standard", "standard_num",
                  "child_age", "child_age_centered", "child_sex", 
                  "child_religion", "question_type", "response")) %>%
  distinct(id, question_type, response, .keep_all=T)

l$response <- as.numeric(as.character(l$response))
l <- l %>% filter(!is.na(response))

l$response_coded <- l$response
l[l$question_type=="french",]$response_coded <- 
  6-l[l$question_type=="french",]$response

write.csv(l, here('data/r_dfs/subsets/likert_item_data.csv'))

e <- l %>%
  dplyr::select(!c(response)) %>%
  spread(question_type, response_coded) 

#library(performance)
e %>%
  dplyr::select(all_of(essentialism_questions)) %>%
  performance::cronbachs_alpha(.[complete.cases(.),])
# alpha=-0.035 

# performance::cronbachs_alpha(e[nonessentialism_questions])
# 0.26

write.csv(e, here('data/r_dfs/subsets/likert_items_recoded_wide.csv'))