###############################################################################
###############################################################################
## 121 - India Sociolinguistics (isl) Analyses
## Ruthe Foushee
## 17 August 2023, for Cognitive Development submission
## Subset data, write associations_data.csv
###############################################################################
###############################################################################
library(here)
here::i_am('0_preprocessing/prepping-associations-df.R')

### load libraries and functions
source(here('isl-resources.R'))
###############################################################################
### read in data files
# hand-entered and lightly cleaned data with demographic info
d <- read.csv(here('data/r_dfs/full_cleaned/india_socioling_data_final.csv'))
###############################################################################
a <- d %>% filter(question_type=="associations",
                  !is.na(language),
                  language!="") %>%
  distinct(., .keep_all = T) %>%
  dplyr::select('study_name', 'id', 'child_sex', 'child_religion', 
                'child_age', 'child_age_centered', 
                  'school', 'standard',  'standard_num', 'sequence',
                  'question_type', 'language', 'speaker_gender', 
                  'origin', 'religion', 'wealth', 'army_navy_language',
                  'coded_wealth', 'no_opinion_geo', 'no_opinion_religion',
                'no_opinion_wealth', 'gujarat', 'india', 'foreign',
                'richer', 'poorer', 'same', 'hindu', 'buddhist',
                'muslim', 'jain', 'christian')

a$language <- factor(
  a$language, levels=c("gujarati", "hindi", "urdu", 
                       "marathi", "tamil", "english_indian", 
                       "english_american", "chinese"), 
  labels=language_labels)

a$geographic_origin <- factor(
  a$origin, levels=c("Gujarat", "India", "foreign", "idk"), 
  labels=origin_labels)

write.csv(a, here('data/r_dfs/subsets/associations_data.csv'))
