###############################################################################
###############################################################################
## 121 - India Sociolinguistics (isl) Analyses
## Clean manually-entered data (purely for documentation; do not run)
## Ruthe Foushee
## 17 August 2023, for Cognitive Development submission
## Read in data, remove duplicate rows, rename columns
###############################################################################
###############################################################################
### load libraries and functions
library(here)
source(here('isl-resources.R')) 
###############################################################################
### read in data files
# hand-entered data with demographic info
d_original <- read.csv(here('data/raw/india_socioling_data_entry_final.csv'))
###############################################################################
d_temp <- d_original[,-1] %>%
  distinct(., .keep_all=T)

# renaming/fixing inconsistencies
d_temp$sequence <- d_temp$question
d_temp$home_language <- d_temp$home
d_temp$friends_language <- d_temp$friends
d_temp$child_sex <- d_temp$gender
d_temp$child_age <- d_temp$age
d_temp$question_type[d_temp$question_type=="speaker_like"] <- "speaker_like_me"
d_temp$study_name <- "india-socioling"
d_temp$language[d_temp$language=="english_american" & 
                  d_temp$presentation=="label"] <- "english"
d_temp$presentation[d_temp$question_type=="learning"] <- "label"
d_temp$speaker_gender[d_temp$question_type=="id"] <- "female"

write.csv(d_temp, here('data/r_dfs/d_temp.csv'))

d_full <- d_temp[, -which(
  names(d_temp) %in% c("Subject.ID", "Religion", "Gender", "X", "X.2", 
                       "question", "gender", "age", "home", "friends"))]
names(d_full) <- tolower(names(d_full))
d_full$home_language <- gsub("and ", "", d_full$home_language)
d_full$home_language <- gsub(",", "", d_full$home_language)
d_full$friends_language <- gsub("and ", "", d_full$friends_language)
d_full$friends_language <- gsub(",", "", d_full$friends_language)
d_full$study_id <- 121
d_full$id[d_full$id=="Z05A43" & d_full$dot=="1/17/19"] <- "Z05A4X"
d_full$roll_number[d_full$id=="Z05A4X"] <- "4X"

#### Faces data ####
## fixing typos
d_full$face <- gsub("[,]", "", d_full$face)
d_full$face <- gsub("and ", "", d_full$face)
d_full$face <- gsub("chinese", "asian", d_full$face)
d_full$face <- gsub("^sian", "asian", d_full$face)
d_full$face <- gsub(" $", "", d_full$face)
d_full$face <- gsub("english", "white", d_full$face)

## standardizing order when multiple faces were selected (alphabetized)
d_full$face <- gsub("dravidian asian", "asian dravidian", d_full$face)
d_full$face <- gsub(
  "dravidian hindu asian", "asian dravidian hindu", d_full$face)
d_full$face <- gsub("hindu asian", "asian hindu", d_full$face)
d_full$face <- gsub("hindu asian muslim", "asian hindu muslim", d_full$face)
d_full$face <- gsub("hindu dravidian", "dravidian hindu", d_full$face)
d_full$face <- gsub(
  "hindu dravidian muslim", "dravidian hindu muslim", d_full$face)
d_full$face <- gsub(
  "hindu white dravidian", "dravidian hindu white", d_full$face)
d_full$face <- gsub(
  "muslim dravidian", "dravidian muslim", d_full$face)
d_full$face <- gsub(
  "muslim dravidian hindu", "dravidian hindu muslim", d_full$face)
d_full$face <- gsub(
  "muslim hindu", "hindu muslim", d_full$face)
d_full$face <- gsub(
  "muslim white asian", "asian muslim white", d_full$face)
d_full$face <- gsub(
  "white asian", "asian white", d_full$face)
d_full$face <- gsub(
  "white asian dravidian", "asian dravidian white", d_full$face)
d_full$face <- gsub(
  "white dravidian", "dravidian white", d_full$face)
d_full$face <- gsub(
  "white dravidian muslim", "dravidian muslim white", d_full$face)

## new binary face columns: 1 if selected, 0 if not, NA if not answered
d_full$asian_face <- case_match(
  d_full$face,
  c("asian", 
    "asian dravidian", "asian hindu", "asian muslim", "asian white",
    "asian hindu muslim", "asian dravidian hindu", "asian dravidian white", "
    asian muslim white") ~ 1,
  ""~NA,
  .default=0
)

d_full$dravidian_face <- case_match(
  d_full$face,
  c("dravidian", "dravidian hindu", "asian dravidian", "dravidian hindu", 
    "asian dravidian hindu", "dravidian hindu muslim", "dravidian hindu white", 
    "dravidian muslim", "dravidian hindu muslim", "dravidian white", 
    "asian dravidian white", "dravidian muslim white") ~ 1,
  ""~NA,
  .default=0
)


d_full$hindu_face <- case_match(
  d_full$face,
  c("hindu", 
    "asian hindu", "dravidian hindu", "hindu muslim", "hindu white",
    "asian dravidian hindu", "asian hindu muslim", 
    "dravidian hindu muslim", "dravidian hindu white") ~ 1,
  ""~NA,
  .default=0
)

d_full$muslim_face <- case_match(
  d_full$face,
  c("muslim", 
    "asian muslim", "dravidian muslim", "hindu muslim", "muslim white",
    "asian muslim white", "asian hindu muslim", 
    "dravidian hindu muslim", "dravidian white muslim") ~ 1,
  ""~NA,
  .default=0
)

d_full$white_face <- case_match(
  d_full$face,
  c("white", 
           "asian white", "dravidian white", "hindu white", "muslim white",
           "asian muslim white", "asian dravidian white", 
           "dravidian hindu white", "dravidian white muslim") ~ 1,
  ""~NA,
  .default=0
)

d_full$no_opinion_face <- case_match(
  d_full$face,
  c("idk", "no opinion") ~ 1,
  .default=0
)

## ratings for learning questions (1-4, 4="no opinion")
d_full[d_full$question_type=="learning", ]$hindu_face <- NA
d_full[d_full$question_type=="learning", ]$muslim_face <- NA
d_full[d_full$question_type=="learning", ]$dravidian_face <- NA
d_full[d_full$question_type=="learning", ]$asian_face <- NA
d_full[d_full$question_type=="learning", ]$white_face <- NA
d_full[d_full$question_type=="learning", ]$no_opinion_face <- 0

d_full[d_full$question_type=="learning" & d_full$hindu %in% 1:3, ]$hindu_face <-
  d_full[d_full$question_type=="learning"  & d_full$hindu %in% 1:3, ]$hindu

d_full[d_full$question_type=="learning" & d_full$muslim %in% 1:3, ]$muslim_face <-
  d_full[d_full$question_type=="learning" & d_full$muslim %in% 1:3, ]$muslim

d_full[d_full$question_type=="learning" & d_full$dravidian %in% 1:3, ]$dravidian_face <-
  d_full[d_full$question_type=="learning" & d_full$dravidian %in% 1:3, ]$dravidian

d_full[d_full$question_type=="learning" & d_full$asian %in% 1:3, ]$asian_face <-
  d_full[d_full$question_type=="learning" & d_full$asian %in% 1:3, ]$asian

d_full[d_full$question_type=="learning" & d_full$white %in% 1:3, ]$white_face <-
  d_full[d_full$question_type=="learning" & d_full$white %in% 1:3, ]$white

d_full[d_full$question_type=="learning" & 
         d_full$hindu %in% c("idk", "4"), ]$no_opinion_face <- 1
d_full[d_full$question_type=="learning" & 
         d_full$muslim %in% c("idk", "4"), ]$no_opinion_face <- 1
d_full[d_full$question_type=="learning" & 
         d_full$dravidian %in% c("idk", "4"), ]$no_opinion_face <- 1
d_full$no_opinion_face[d_full$question_type=="learning" & 
         d_full$asian==4] <- 1
d_full$no_opinion_face[d_full$question_type=="learning" & 
                         d_full$white==4] <- 1

d_full$no_opinion_face_hindu[d_full$question_type=="learning"] <- 
  case_match(
    d_full$hindu[d_full$question_type=="learning"],
    c("idk", "4") ~ 1,
    ""~NA,
    .default=0
    )

d_full$no_opinion_face_muslim[d_full$question_type=="learning"] <- 
  case_match(
    d_full$muslim[d_full$question_type=="learning"],
    c("idk", "4") ~ 1,
    ""~NA,
    .default=0
    )

d_full$no_opinion_face_dravidian[d_full$question_type=="learning"] <- 
  case_match(
    d_full$dravidian[d_full$question_type=="learning"],
    c("idk", "4") ~ 1,
    ""~NA,
    .default=0
  )

d_full$no_opinion_face_asian[d_full$question_type=="learning"] <- 
  case_match(
    d_full$asian[d_full$question_type=="learning"],
    4 ~ 1,
    c(0, NA)~NA,
    .default=0
  )

d_full$no_opinion_face_white[d_full$question_type=="learning"] <- 
  case_match(
    d_full$asian[d_full$question_type=="learning"],
    4 ~ 1,
    c(0, NA)~NA,
    .default=0
  )

#### Geographic origin data ####
## fixing typos/relabeling variable
d_full$origin <- tolower(d_full$origin)
d_full$origin[d_full$origin=="guajrat"] <- "Gujarat"
d_full$origin[d_full$origin=="gujarat "] <- "Gujarat"
d_full$origin[d_full$origin=="gujarat"] <- "Gujarat"
d_full$origin[d_full$origin=="india"] <- "India"
d_full$origin[d_full$origin=="outside_india"] <- "foreign"
d_full$origin[d_full$origin=="outide_india"] <- "foreign"

## new binary origin columns: 1 if selected, 0 if not, NA if not answered
d_full$gujarat <- case_match(
  d_full$origin,
  "Gujarat" ~ 1,
  ""~NA,
  .default=0
)

d_full$india <- case_match(
  d_full$origin,
  "India" ~ 1,
  ""~NA,
  .default=0
)

d_full$foreign <- case_match(
  d_full$origin,
  "foreign" ~ 1,
  ""~NA,
  .default=0
)

d_full$no_opinion_geo <- case_match(
  d_full$origin,
  "idk" ~ 1,
  ""~NA,
  .default=0
)

#### Religion data ####
## fixing typos
d_full$religion <- gsub(" $", "", d_full$religion)
d_full$religion <- gsub("and ", "", d_full$religion)
d_full$religion <- gsub(",", "", d_full$religion)
d_full$religion[d_full$religion=="budduhist"] <- "buddhist"
d_full$religion[d_full$religion=="chrisitan"] <- "christian"
d_full$religion[d_full$religion=="hindi"] <- "hindu"

## new binary religion columns: 1 if selected, 0 if not, NA if not answered
d_full$hindu <- case_match(
  d_full$religion,
  c("hindu", "hindu muslim") ~ 1,
  ""~NA,
  .default=0
)

d_full$muslim <- case_match(
  d_full$religion,
  c("muslim", "hindu muslim") ~ 1,
  ""~NA,
  .default=0
)

d_full$jain <- case_match(
  d_full$religion,
  c("jain", "jain buddhist") ~ 1,
  ""~NA,
  .default=0
)

d_full$buddhist <- case_match(
  d_full$religion,
  c("buddhist", "jain buddhist") ~ 1,
  ""~NA,
  .default=0
)

d_full$christian <- case_match(
  d_full$religion,
  c("christian") ~ 1,
  ""~NA,
  .default=0
)

d_full$no_opinion_religion <- case_match(
  d_full$religion,
  c("idk") ~ 1,
  ""~NA,
  .default=0
)

#### Wealth data ####
d_full$wealth[d_full$wealth=="less"] <- "poorer"
d_full$wealth[d_full$wealth=="more"] <- "richer"
d_full$wealth[d_full$wealth=="rich"] <- "richer"
d_full$wealth[d_full$wealth=="poor"] <- "poorer"

## new binary wealth columns: 1 if selected, 0 if not, NA if not answered
d_full$richer <- case_match(
  d_full$wealth,
  "richer" ~ 1,
  ""~NA,
  .default=0
)

d_full$poorer <- case_match(
  d_full$wealth,
  "poorer" ~ 1,
  ""~NA,
  .default=0
)

d_full$same <- case_match(
  d_full$wealth,
  "same" ~ 1,
  ""~NA,
  .default=0
)

d_full$no_opinion_wealth <- case_match(
  d_full$wealth,
  "idk" ~ 1,
  ""~NA,
  .default=0
)

d_full$coded_wealth <- plyr::mapvalues(d_full$wealth, from=c(
  "poorer", "same", "richer", "idk"), to=c(
    "-1", "0", "1", NA))
d_full$coded_wealth <- as.numeric(as.character(d_full$coded_wealth))

#### Child language data ####
## fixing typos
d_full$home_language[d_full$home_language=="gujarat"] <- "gujarati"
d_full$why[d_full$response=="punjabi from my friends"] <- "from my friends"
d_full$response[d_full$response=="punjabi from my friends"] <- "punjabi"
d_full$response <- gsub(" $", "", d_full$response)
d_full$response[d_full$response=="arbic"] <- "arabic"
d_full$response[d_full$response=="engilsh"] <- "english"
d_full$response[d_full$response=="punjab"] <- "punjabi"
d_full$response[d_full$response=="gujarti"] <- "gujarati"
d_full$response[d_full$response=="gujarat"] <- "gujarati"
d_full$response[d_full$response=="gujarathi"] <- "gujarati"
d_full$response[d_full$response=="gujarathi"] <- "gujarati"
d_full$response[d_full$response=="sindi"] <- "sindhi"

#### Removing duplicate rows ###
d_full <- d_full %>%
  distinct(.) %>%
  dplyr::select("study_id", "study_name", "id", "dob", "dot", 
                "child_age", "child_sex", "child_religion", 
                "school", "standard", "order", "block_order", 
                "mother_tongue", "home_language", "friends_language", 
                "sequence", "question_type", "presentation", "language", 
                "speaker_gender", "response", "face", "why", "correct", 
                "origin",  "gujarat", "india", "foreign", "no_opinion_geo", 
                "religion", "hindu", "muslim", "christian", "buddhist", "jain", 
                "no_opinion_religion", 
                "wealth", "poorer", "same", "richer", "no_opinion_wealth", 
                "coded_wealth", 
                "hindu_face", "muslim_face", "dravidian_face", "asian_face", 
                "white_face", "no_opinion_face", "no_opinion_face_hindu", 
                "no_opinion_face_muslim", "no_opinion_face_dravidian", 
                "no_opinion_face_asian", "no_opinion_face_white")

d_full[d_full$question_type=="learning",]$hindu_face <- as.numeric(d_full[
  d_full$question_type=="learning",]$hindu_face)-1

d_full[d_full$question_type=="learning",]$muslim_face <- as.numeric(d_full[
  d_full$question_type=="learning",]$muslim_face)-1

d_full[d_full$question_type=="learning",]$dravidian_face <- as.numeric(d_full[
  d_full$question_type=="learning",]$dravidian_face)-1

d_full[d_full$question_type=="learning",]$asian_face <- as.numeric(d_full[
  d_full$question_type=="learning",]$asian_face)-1

d_full[d_full$question_type=="learning",]$white_face <- as.numeric(d_full[
  d_full$question_type=="learning",]$white_face)-1

d_full$language <- as.factor(d_full$language)
d_full$army_navy_language <- plyr::mapvalues(d_full$language,
                                              from=c("hindi", "urdu", 
                                                     "gujarati", "marathi",
                                                     "tamil", "english", 
                                                     "english_indian",
                                                     "english_american", 
                                                     "chinese"),
                                              to=c("hindi", "urdu", 
                                                   "gujarati", "marathi",
                                                   "tamil", "english", 
                                                   "english",
                                                   "english", 
                                                   "chinese"))

d_full$standard_num <- as.numeric(d_full$standard)

# mean-centered age 
d_full$child_age_centered <- d_full$child_age - na.mean(d_pps$child_age)

# df with 1 row per pp
d_pps <- d_full %>% distinct(id, school, standard, child_age, 
                             child_sex, child_religion)

write.csv(d_full[,-1], 
          here('data/r_dfs/full_cleaned/india_socioling_data_final.csv'))
write.csv(d_pps[,-1], 
          here('data/r_dfs/full_cleaned/india_socioling_single_row_pp_data.csv'))
