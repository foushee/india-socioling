###############################################################################
###############################################################################
## 121 - India Sociolinguistics (isl) Analyses
## Clean manually-entered data for language identification trials, create new df
## Ruthe Foushee
## 17 August 2023, for Cognitive Development submission
## Subset data, make composite familiarity var,
## write language_identification_data.csv and language_familiarity_data.csv
###############################################################################
###############################################################################
library(here)
here::i_am('0_preprocessing/prepping-identification-dfs.R')

### load libraries and functions
source(here('isl-resources.R'))
###############################################################################
### read in data files
# hand-entered and lightly cleaned data with demographic info
d <- read.csv(here('data/r_dfs/full_cleaned/india_socioling_data_final.csv'))
# child languages data --- for composite 'familiarity' variable
c <- read.csv(here('data/r_dfs/subsets/child_language_data.csv'))
###############################################################################
id_cols <- c("study_name", "school", "id", "standard", "standard_num",
             "child_age", "child_age_centered", "child_sex", 
             "child_religion", 
             "mother_tongue", "home_language", "friends_language", 
             "sequence", "question_type", 
             "language", "army_navy_language", 
             "presentation", "speaker_gender", "response", "correct")

i <- d %>% filter(question_type=="id") %>%
  dplyr::select(all_of(id_cols)) 
i$correct[i$response==""] <- NA

i$language <- factor(
  i$language, levels=language_vars, 
  labels=language_labels)

i$id_correct <- i$correct 

i <- i %>%
  dplyr::select(c(id_cols[1:19], "id_correct"))

write.csv(i, here('data/r_dfs/subsets/language_identification_data.csv'))

#### New, "Language Familiarity" Variable
child_lang_cols <- c("id", language_name_vars)

lists_stimuli_langs_df <- cls %>%
  dplyr::select(any_of(child_lang_cols)) %>%
  gather(language, lists_lang, -id) %>%
  group_by(id, language) %>%
  summarize(knows=na.max(lists_lang)) %>%
  mutate(language=str_to_sentence(language))

lists_stimuli_langs_df$language[
  lists_stimuli_langs_df$language=="English"] <- "English (India)"

fam_df <- merge(i[c("language", "army_navy_language", "id", "id_correct")], 
                lists_stimuli_langs_df, 
                by=c("id", "language"), all=T) %>%
  group_by(id, language) %>%
  mutate(familiar=ifelse(id_correct>0 | knows>0, 1, 0)) 

fam_df$familiar[fam_df$id_correct==0 & is.na(fam_df$knows)] <- 0

write.csv(fam_df, here('data/r_dfs/subsets/language_familiarity.csv'))
# i <- read.csv(here('data/r_dfs/subsets/language_familiarity.csv'))
# 
# cor.test(i$knows, i$id_correct)
# i$language <- factor(i$language)
# i$id <- factor(i$id)
# 
# lists_stimuli_langs_df$language <- factor(lists_stimuli_langs_df$language, 
#                                           levels=language_name_vars,
#                                           labels = language_name_labels)
# 
# lists_stimuli_langs_df$language <- as.factor(lists_stimuli_langs_df$language)
# id_df$language <- as.factor(id_df$language)
# lists_stimuli_langs_df$language <- str_to_title(lists_stimuli_langs_df$language)
# twoknows_df <- merge(lists_stimuli_langs_df, id_df, 
#                      by=c("id", "language"), all=T) %>%
#   #group_by(id, language) %>%
#   mutate(familiar=knows>0 | id_correct>0) %>%
#   dplyr::select(id, language, knows, id_correct, familiar)
# 
# 
# write.csv(twoknows_df, here("data/r_dfs/subsets/language_familiarity.csv"))
# 
# lf_df <- read.csv(here("data/language_familiarity.csv"))
# knows_eng <- lf_df$id[lf_df$language=="english" & lf_df$familiar==1]
# 
# lf_df$english_american <- NA
# lf_df$english_indian <- NA
# 
# d <- merge(lf_df, d, by=c("id", "language"), all=T)
