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
d_full$study_name <- "india-socioling"
  
d_full$standard[d_full$standard==8] <- 7

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
d_full$presentation[d_full$question_type=="learning"] <- "label"

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

# df with 1 row per pp
d_pps <- d %>% distinct(id, school, standard, child_age, 
                        child_sex, child_religion)
# mean-centered age 
d_full$child_age_centered <- d_full$child_age - mean(d_pps$child_age)

write.csv(d_full[,-1], here('data/r_dfs/india_socioling_data_final.csv'))
write.csv(d_pps[,-1], here('data/r_dfs/india_socioling_single_row_pp_data.csv'))

d_full %>%
  group_by(id, sequence, question_type) %>%
  mutate(nrows=n()) %>%
  filter(nrows>1, !is.na(sequence), response!="") %>%
  group_by(question_type, id) %>%
  summarize(n=n())

d_full %>%
  filter(question_type=="faces") %>%
  group_by(id, question_type, sequence) %>%
  mutate(nrows=n()) %>%
  filter(nrows>1, !is.na(sequence), response!="")

#duplicated_ids <- c("Z07A02", "Z07A06", "Z07A41", "Z07B12")

#### Child language data ####
## "languages and how you learned" table 
chilangs_learning_table <- d_full %>%
  filter(question_type %in% c("lang1", "lang2", "lang3", "lang4", "lang5",
                              "speak", "lang home and friends", "understand")) %>%
  mutate(question=question_type, languages=response, how_learned=why) %>%
  dplyr::select(id, standard, question, languages, how_learned)
  
chilangs_learning_table$id <- as.factor(chilangs_learning_table$id)

chilangs_df_temp <- d_full %>% distinct(id, .keep_all=T) %>%
  dplyr::select("id", "standard", 
                "mother_tongue", "home_language", "friends_language") %>%
  gather(., question, languages, -id, -standard) 
chilangs_df_temp$id <- as.factor(chilangs_df_temp$id)

chilangs_df <- merge(chilangs_df_temp, chilangs_learning_table, 
                     by=c("id", "standard", "question", "languages"), all=T)

chilangs_df$languages <- gsub(" $", "", chilangs_df$languages)
chilangs_df$languages <- gsub("and ", "", chilangs_df$languages)
chilangs_df$languages <- gsub(",", "", chilangs_df$languages)

chilangs_df$alph_order <- chilangs_df$languages

chilangs_df$alph_order <- gsub(
  "gujarat ", "gujarati ", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarat$", "gujarati", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "punjab ", "punjabi ", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "punjab$", "punjabi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati english", "english gujarati", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati english hindi", "english gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "marathi hindi gujarati", "gujarati hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "marathi hindi", "hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "marathi gujarati", "gujarati marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi marathi gujarati", "gujarati hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi gujarati marathi", "gujarati hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi gujarati marathi english", "english gujarati hindi marathi", 
  chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati hindi marathi english", "english gujarati hindi marathi", 
  chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi english gujarati", "english gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi english marathi", "english hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi gujarati", "gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati english hindi marathi", "english gujarati hindi marathi", 
  chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati hindi english", "english gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindigujarati", "gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujaratihindi", "gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hind english gujarati", "english gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi sindhi english gujarati", "english gujarati hindi sindhi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "sindi", "sindhi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi/urdu", "hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi english", "english hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "punjabi marathi", "marathi punjabi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "urdu english gujarati hindi", "english gujarati hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "urdu english", "english urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "urdu marathi", "marathi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "tamil marathi", "marathi tamil", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "sindhi english gujarati hindi", "english gujarati hindi sindhi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "urdu marathi", "marathi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "urdu english hindi gujarati", "english gujarati hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "punjabi english gujarati hindi marathi", 
  "english gujarati hindi marathi punjabi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "polish english gujarati hindi", "english gujarati hindi polish", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "marwadi", "marwari", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "kathiyawadi gujarati", "gujarati kathiyawadi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english bengali gujarati", "bengali english gujarati", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english gujarati hindi bengali", "bengali english gujarati hindi", 
  chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english gujarati hindi bengali marathi", "bengali english gujarati hindi marathi", 
  chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english gujarati marathi hindi", "english gujarati hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english hindi bengali", "bengali english hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english hindi urdu gujarati", "english gujarati hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english urdu gujarati hindi", "english gujarati hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english urdu hindi", "english hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati hindi english urdu", "english gujarati hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati hindi sindhi english", "english gujarati hindi sindhi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati marathi english hindi", "english gujarati hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati marathi hindi", "gujarati hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati polish hindi", "gujarati hindi polish", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gujarati sindhi english hindi", "english gujarati hindi sindhi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "gurarti english hindi", "english gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi english urdu", "english hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi english urdu gujarati", "english hindi gujarati urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi marathi english", "english hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi marathi english gujarati", "english gujarati hindi marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi sindhi gujarati", "hindi gujarati sindhi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi/english little marwari", "english hindi marwari", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "marathi arbic", "arabic marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "marathi english", "english marathi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english gujaratii hindi", "english gujarati hindi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "guajrati", "gujarati", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english hindi urdu gujarati", "english gujarati hindi urdu", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "hindi gujarati sindhi", "gujarati hindi sindhi", chilangs_df$alph_order)
chilangs_df$alph_order <- gsub(
  "english hindi marathi gujarati", "english gujarati hindi marathi", chilangs_df$alph_order)

chilangs_df$gujarati <- case_match(
  chilangs_df$alph_order,
  c("gujarati", "english gujarati", "gujarati hindi", "english gujarati hindi", 
    "english gujarati hindi sindhi", "english gujarati hindi marathi",
    "gujarati hindi sindhi", "gujarati hindi marathi", "gujarati marathi",
    "gujarati hindi polish", "gujarati hindi urdu", "gujarati kathiyawadi",
    "gujarati polish","bengali english gujarati", "gujarati sindhi", 
    "bengali english gujarati hindi", "bengali english gujarati hindi marathi",
    "english gujarati hindi kathiyawadi", "english gujarati hindi marathi punjabi",
    "english gujarati hindi polish", "english gujarati hindi urdu"
    ) ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$hindi <- case_match(
  chilangs_df$alph_order,
  c("bengali english gujarati hindi", "bengali english gujarati hindi marathi",
    "bengali english hindi", "english gujarati hindi kathiyawadi", 
    "english gujarati hindi marathi punjabi", "english gujarati hindi polish",
    "english gujarati hindi urdu", "english hindi marwari", "english hindi memon",
    "english hindi urdu", "gujarati hindi polish", "gujarati hindi sindhi",
    "gujarati hindi urdu", "hindi urdu", "hindi", "english hindi", 
    "gujarati hindi", "english gujarati hindi", 
    "english gujarati hindi sindhi", "english gujarati hindi marathi",
    "english hindi marathi", "gujarati hindi marathi", "hindi marathi",
    "hindi sindhi", "hindi urdu", "hindi punjabi", "hindi marwari", 
    "gujarati hindi sindhi") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$english <- case_match(
  chilangs_df$alph_order,
  c("bengali english gujarati", "bengali english gujarati hindi", 
    "bengali english gujarati hindi marathi", "bengali english hindi",
    "english", "english hindi", "english gujarati hindi", "english gujarati",
    "english gujarati hindi kathiyawadi", "english gujarati hindi marathi punjabi",
    "english gujarati hindi polish", "english gujarati hindi urdu",
    "english hindi marathi", "english hindi marwari", "english hindi memon",
    "english hindi urdu", "english marathi", "english urdu", "foreigners english",
    "english gujarati hindi sindhi", "english gujarati hindi marathi") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$marathi <- case_match(
  chilangs_df$alph_order,
  c("arabic marathi", "bengali english gujarati hindi marathi", 
    "english gujarati hindi marathi punjabi", "english marathi",
    "marathi", "english gujarati hindi marathi", "english hindi marathi",
    "gujarati hindi marathi", "gujarati marathi", "hindi marathi", "marathi punjabi",
    "marathi tamil", "marathi telgu", "marathi urdu") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$urdu <- case_match(
  chilangs_df$alph_order,
  c("english gujarati hindi urdu", "english hindi urdu", "english urdu", 
    "gujarati hindi urdu", "urdu", "hindi urdu", "marathi urdu", "punjabi urdu") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$sindhi <- case_match(
  chilangs_df$alph_order,
  c("sindhi", "english gujarati hindi sindhi", "gujarati hindi sindhi",
    "hindi sindhi", "gujarati sindhi") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$punjabi <- case_match(
  chilangs_df$alph_order,
  c("punjabi", "punjabi urdu", "marathi punjabi", "hindi punjabi", 
    "english gujarati hindi marathi punjabi") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$bengali <- case_match(
  chilangs_df$alph_order,
  c("bengali", "bengali english gujarati", "bengali english gujarati hindi",
    "bengali english gujarati hindi marathi", "bengali english hindi") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$marwari <- case_match(
  chilangs_df$alph_order,
  c("marwari", "hindi marwari", "english hindi marwari") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$kathiyawadi <- case_match(
  chilangs_df$alph_order,
  c("kathiyawadi", "english gujarati hindi kathiyawadi", "gujarati kathiyawadi"
    ) ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$malyam <- case_match(
  chilangs_df$alph_order,
  c("malyam") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$memon <- case_match(
  chilangs_df$alph_order,
  c("memon", "english hindi memon") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$tamil <- case_match(
  chilangs_df$alph_order,
  c("marathi tamil", "tamil") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$rajasthani <- case_match(
  chilangs_df$alph_order,
  c("rajasthani") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$arabic <- case_match(
  chilangs_df$alph_order,
  c("arabic", "arabic marathi") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$bihari <- case_match(
  chilangs_df$alph_order,
  c("bihari") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$telgu <- case_match(
  chilangs_df$alph_order,
  c("marathi telgu") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$other <- case_match(
  chilangs_df$alph_order,
  c("gujarati polish", "gujarati hindi polish", "english gujarati hindi polish",
    "polish", "sanskrit", "french") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df <- chilangs_df %>%
  mutate(langs_sum=gujarati + hindi + english + urdu + marathi + punjabi + sindhi + 
           tamil + telgu + bengali + arabic + bihari + rajasthani + 
           malyam + kathiyawadi + marwari + memon + other) %>% 
  filter(!is.na(languages),
         languages!="")

#family_responses <- c("because of dad","brother", "brother and sisters", 
#                      "brother", "by father", "by mother", "by mother and teachers",
#                      )

write.csv(chilangs_df, here('data/r_dfs/child_languages.csv'))

