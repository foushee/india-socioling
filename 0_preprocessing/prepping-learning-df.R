###############################################################################
###############################################################################
## 121 - India Sociolinguistics (isl) Analyses
## Clean manually-entered data for learning prediction trials and create new df
## Ruthe Foushee
## 17 August 2023, for Cognitive Development submission
## Subset data, write learning_predictions_data.csv
###############################################################################
###############################################################################
library(here)
here::i_am('0_preprocessing/prepping-learning-df.R')

### load libraries and functions
source(here('isl-resources.R'))
###############################################################################
### read in data files
# hand-entered and lightly cleaned data with demographic info
d <- read.csv(here('data/r_dfs/full_cleaned/india_socioling_data_final.csv'))
###############################################################################
no_opinion_face_cols <- c('no_opinion_face_hindu',
                     'no_opinion_face_muslim', 'no_opinion_face_dravidian',
                     'no_opinion_face_asian', 'no_opinion_face_white')
learning_columns <- c('hindu_face', 'muslim_face', 
                      'dravidian_face', 'asian_face', 'white_face',
                      'no_opinion_face', all_of(no_opinion_face_cols))
learning_languages <- c('gujarati', 'hindi', 'tamil', 'english', 'chinese')

# 25 rows per pp, 5 languages x 5 faces
l <- d %>% filter(question_type=='learning',
                  language %in% learning_languages) %>%
  dplyr::select(c('study_name', 'school', 'id', 'standard', 'standard_num',
                  'child_age', 'child_age_centered', 'child_sex', 'child_religion', 
                  'mother_tongue', 'home_language', 'friends_language', 
                  'sequence', 'question_type', 
                  'language', 'army_navy_language', 
                  'presentation', 'speaker_gender', all_of(learning_columns))) %>%
  gather(., face, rating_num, -study_name, -language, -army_navy_language,
         -standard, -standard_num, -speaker_gender, -presentation, - sequence,
         -question_type,
         -id, -child_age, -child_age_centered, -child_sex, -child_religion, 
         -school, -mother_tongue, -home_language, -friends_language,
         -no_opinion_face,
         -no_opinion_face_asian, -no_opinion_face_hindu, 
         -no_opinion_face_muslim, -no_opinion_face_dravidian,
         -no_opinion_face_white) %>%
  group_by(face) %>%
  #filter(rating>0) %>%
  dplyr::select('study_name', 'id', 'school', 'standard', 'standard_num',
                'child_age', 'child_age_centered', 
                'child_sex', 'child_religion', 'language', 'army_navy_language', 
                'question_type', 'presentation', 'speaker_gender', 'sequence',
                'face', 'rating_num', no_opinion_face_cols) %>%
  mutate(rating_cat = plyr::mapvalues(rating_num, 
    from=c(0, 1, 2), to=c("Not at all", "Medium", "Very well"))) %>%
  arrange(standard)  

l$rating_cat_w_idk <- l$rating_cat
#table(l$rating_cat_w_idk[is.na(l$rating_cat)])
l$rating_cat_w_idk[
  is.na(l$rating_cat) & l$no_opinion_face_hindu==1] <- "No opinion"
l$rating_cat_w_idk[
  is.na(l$rating_cat) & l$no_opinion_face_muslim==1] <- "No opinion"
l$rating_cat_w_idk[
  is.na(l$rating_cat) & l$no_opinion_face_dravidian==1] <- "No opinion"
l$rating_cat_w_idk[
  is.na(l$rating_cat) & l$no_opinion_face_white==1] <- "No opinion"
l$rating_cat_w_idk[
  is.na(l$rating_cat) & l$no_opinion_face_asian==1] <- "No opinion"

l[!is.na(l$rating_cat) & l$no_opinion_face_asian==1,] %>% View(.)
#l$language <- factor(
#  l$language, 
#  levels=language_name_vars, 
#  labels=language_name_labels)

#l$face<- factor(l$face,
#                levels=face_vars[1:5],
#                labels=c('hindu', 'muslim', 'dravidian', 'white', 'asian'))

write.csv(l, here('data/r_dfs/subsets/learning_predictions_data.csv'))

