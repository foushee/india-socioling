###############################################################################
###############################################################################
## 121 - India Sociolinguistics (isl) Analyses
## Clean manually-entered child language data and create new df
## Ruthe Foushee
## 17 August 2023, for Cognitive Development submission
## Subset data, standardize and categorize string entries, 
## write child_language_data.csv
###############################################################################
###############################################################################
library(here)
here::i_am('0_preprocessing/prepping-child-languages-df.R')

### load libraries and functions
source(here('isl-resources.R'))
###############################################################################
### read in data files
# hand-entered and lightly cleaned data with demographic info
d <- read.csv(here('data/r_dfs/full_cleaned/india_socioling_data_final.csv'))
###############################################################################
#### Child language data ####
## "languages and how you learned" table 
chilangs_learning_table <- d %>%
  filter(question_type %in% c("lang1", "lang2", "lang3", "lang4", "lang5",
                              "speak", "lang home and friends", "understand")
         ) %>%
  mutate(question=question_type, languages=response, how_learned=why) %>%
  dplyr::select(id, standard, question, languages, how_learned)

chilangs_learning_table$id <- as.factor(chilangs_learning_table$id)

chilangs_df_temp <- d %>% distinct(id, .keep_all=T) %>%
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

chilangs_df$polish <- case_match(
  chilangs_df$alph_order,
  c("gujarati polish", "gujarati hindi polish", "english gujarati hindi polish",
    "polish") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$sanskrit <- case_match(
  chilangs_df$alph_order,
  c("sanskrit") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df$french <- case_match(
  chilangs_df$alph_order,
  c("french") ~ 1,
  ""~NA,
  .default=0
)

chilangs_df <- chilangs_df %>%
  mutate(langs_sum=gujarati + hindi + english + urdu + marathi + punjabi + sindhi + 
           tamil + telgu + bengali + arabic + bihari + rajasthani + 
           malyam + kathiyawadi + marwari + memon + 
           polish + sanskrit + french) %>% 
  filter(!is.na(languages),
         languages!="")

# classify "how learned" responses
family_responses <- c("because of dad","brother", "brother and sisters", 
                      "brother", "by father", "by mother", "by mother and teachers",
                      "by my father", "by my mother and father", "by my parents",
                      "by parents", "by sister", "dad", "family", "father", 
                      "friends/sister", "from beginning with parents", 
                      "from brother",
                      "from friends, brothers, sister", "from mother", 
                      "from mother, father, teacher", "from parents", 
                      "from school and parents", "from sister, brother",
                      "grandfather", "grandfather, grandmother", "grandmother",
                      "grandparents", "mom",
                      "mother", "mother and father", "mother, father, and teacher",
                      "my kaki", "my mother, father, grandparents", 
                      "parent", "parents", "parents ", 
                      "school, brother, mother, father", 
                      "school, father and mother",
                      "sister", "sister and brother")

school_responses <- c("by coming to school", "by mother and teachers", 
                      "by school", "by teacher", "by teachers", "by teaching",
                      "classes", "from mother, father, teacher", 
                      "from school", "from school and parents", 
                      "from school and tuition",
                      "from society and school", "from the society and school", 
                      "in school", "in school with friend's teacher", 
                      "mother, father, and teacher",  "my school", "my teacher",
                      "my teacher and classmate", "past school", "school", 
                      "school, brother, mother, father", 
                      "school, father and mother",
                      "schools and teachers", "teacher", "teachers", 
                      "we learn in school")

friends_responses <- c("by friends", "by learning to friend", "friend", "friends", 
                       "friends/sister", "frm friend", "from friend", "from friends",
                       "from my friend", "from my friends", "my friends", 
                       "neighbour friends")

neighborhood_responses <- c("by neighbours", "by society", "from my neighbour",
                            "from neighbourhood", "from neighbours", "from neighours",
                            "from society", "from society and school", 
                            "from the society and school", "neighborhood", "neighbour",
                            "neighbour friends", "neighbours", "people in state")

birth_responses <- c("birth", "forever", "from beggining", "from begin", 
                     "from beginning", "from beginning with parents", "from start",
                     "from the start", "in the beginning", "start/birth")

crosscat_responses <- c("from friends, brothers, sister", 
                        "from beginning with parents",
                        "friends/sister", "by mother and teachers", 
                        "from mother, father, teacher", "from school and parents",
                        "from society and school", "from the society and school",
                        "mother, father, and teacher", "neighbour friends",
                        "school, brother, mother, father", 
                        "school, father and mother")

other_responses <- c("from others (i do not speak)", "from saint", "I learn it", 
                     "mother tongue", "myself", "staff")

chilangs_df$learned_family <- case_match(
  chilangs_df$how_learned,
  family_responses ~ 1,
  c(NA,"")~NA,
  .default=0
)

chilangs_df$learned_school <- case_match(
  chilangs_df$how_learned,
  school_responses ~ 1,
  c(NA,"")~NA,
  .default=0
)

chilangs_df$learned_birth <- case_match(
  chilangs_df$how_learned,
  birth_responses ~ 1,
  c(NA,"")~NA,
  .default=0
)

chilangs_df$learned_neighborhood <- case_match(
  chilangs_df$how_learned,
  neighborhood_responses ~ 1,
  c(NA,"")~NA,
  .default=0
)

chilangs_df$learned_friends <- case_match(
  chilangs_df$how_learned,
  friends_responses ~ 1,
  c(NA,"")~NA,
  .default=0
)

chilangs_df$learned_other <- case_match(
  chilangs_df$how_learned,
  other_responses ~ 1,
  c(NA,"")~NA,
  .default=0
)

chilangs_df$learned_multiple <- case_match(
  chilangs_df$how_learned,
  crosscat_responses ~ 1,
  c(NA,"")~NA,
  .default=0
)

chilangs_df$how_learned_coded <- NA
chilangs_df$how_learned_coded[chilangs_df$learned_family==1 & 
                                chilangs_df$learned_multiple==0] <- "family"
chilangs_df$how_learned_coded[chilangs_df$learned_school==1 & 
                                chilangs_df$learned_multiple==0] <- "school"
chilangs_df$how_learned_coded[chilangs_df$learned_neighborhood==1 & 
                                chilangs_df$learned_multiple==0] <- "neighborhood"
chilangs_df$how_learned_coded[chilangs_df$learned_friends==1 & 
                                chilangs_df$learned_multiple==0] <- "friends"
chilangs_df$how_learned_coded[chilangs_df$learned_birth==1 & 
                                chilangs_df$learned_multiple==0] <- "birth"
chilangs_df$how_learned_coded[chilangs_df$learned_other==1 & 
                                chilangs_df$learned_multiple==0] <- "other"
chilangs_df$how_learned_coded[chilangs_df$learned_multiple==1] <- "multiple"


write.csv(chilangs_df, here('data/r_dfs/subsets/child_language_data.csv'))
