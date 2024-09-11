###############################################################################
###############################################################################
## 121 - India Sociolinguistics (isl) Analyses
## Clean manually-entered data for face selection trials and create new df
## Ruthe Foushee
## 17 August 2023, for Cognitive Development submission
## Subset data, add binary face selection columns, write face_selection_data.csv
###############################################################################
###############################################################################
library(here)
here::i_am('0_preprocessing/prepping-faces-df.R')

### load libraries and functions
source(here('isl-resources.R'))
###############################################################################
### read in data files
# hand-entered and lightly cleaned data with demographic info
d <- read.csv(here('data/r_dfs/full_cleaned/india_socioling_data_final.csv'))
###############################################################################
face_columns <- c("hindu_face", "muslim_face", "dravidian_face", "asian_face", 
                  "white_face", "no_opinion_face")

f <- d %>% filter(question_type=="faces") %>%
  dplyr::select(c("study_name", "school", "id", "standard", "standard_num",
                  "child_age", "child_age_centered", "child_sex", "child_religion", 
                  "mother_tongue", "home_language", "friends_language", 
                  "sequence", "question_type", 
                  "language", "army_navy_language", 
                  "presentation", "speaker_gender", 
                  "face", all_of(face_columns))) 

f$language <- factor(
  f$language, levels=c(language_vars, "english"),
  labels=c(language_labels, "English"))

f$num_faces <- f$hindu_face + f$muslim_face + f$dravidian_face +
  f$asian_face + f$white_face

write.csv(f, here('data/r_dfs/subsets/face_selection_data.csv'))
