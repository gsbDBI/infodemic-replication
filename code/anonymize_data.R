# Packages ----
seed <- readLines('../data/seed.txt') # private
set.seed(seed)


library(dplyr)
library(lubridate)
library(readr)

# Read in data ----
files <- list.files('../data', 
                    pattern = '^Social_Impact_Research_Initiative_[0-9].*[0-9].csv$', 
                    full.names = TRUE)

files_evaluation <- list.files('../data', 
                               pattern = '^Social_Impact_Research_Initiative_evaluation.*[0-9].csv$', 
                               full.names = TRUE)

# Round 1 data
(INPUT_FILENAME <- files[which.max(file.info(files)$mtime)])
# Round 2 data
(INPUT_FILENAME_EVALUATION <- files_evaluation[which.max(file.info(files_evaluation)$mtime)])
df <- suppressWarnings(read_csv(INPUT_FILENAME))
df_evaluation <- suppressWarnings(read_csv(INPUT_FILENAME_EVALUATION))

gs_files <- list.files('../data', 
                       pattern = 'Active - Treatment_Probs', 
                       full.names = TRUE)

(INPUT_FILENAME2 <- gs_files[which.max(file.info(gs_files)$mtime)])
df_gs <- suppressWarnings(read_csv(INPUT_FILENAME2, col_names = c('messenger user id', paste0('probs_', 0:39)) ))


gs_files_full <- list.files('../data', 
                            pattern = 'Active - Full_Data', 
                            full.names = TRUE)

(INPUT_FILENAME3 <- gs_files_full[which.max(file.info(gs_files_full)$mtime)])
df_gsfull <- suppressWarnings(read_csv(INPUT_FILENAME3))




# Account for missingness ----
# last observation that we have the probability for
mindate <- ymd_hms(df_evaluation$news_time_start[which(df_evaluation$`messenger user id` == 4787401554621625)])

# first observation that we have the probability for
maxdate <- ymd_hms(df_evaluation$news_time_start[which(df_evaluation$`messenger user id` == 6375895035757572)])

idsoi <- df_evaluation$`messenger user id`[which( (ymd_hms(df_evaluation$news_time_start) > mindate) & (ymd_hms(df_evaluation$news_time_start)  < maxdate))]

# account for these observations
table(idsoi %in% df_gs$`messenger user id`)
idsoi <- idsoi[which(!(idsoi %in% df_gs$`messenger user id`))]


#' Check for overlap 
#' 

# First index in evaluation split for full data
eidx_full <- which(df_gsfull$user_id == 5637321389672841)
# First index in evaluation split for probs data
eidx <- which(df_gs$`messenger user id` == 6149393868434861)

# We will exclude these observations, as they were not used when doing model updating
# Observations in the full data spreadsheet not in the probability spreadsheet
table(unique(df_gsfull$user_id) %in% df_gs$`messenger user id`)
table(unique(df_gsfull$user_id[eidx_full:nrow(df_gsfull)]) %in% df_gs$`messenger user id`[eidx:nrow(df_gs)])
# Observations in the probability spreadsheet not in the full data spreadsheet
table(unique(df_gs$`messenger user id`) %in% df_gsfull$user_id)
table(df_gs$`messenger user id`[eidx:nrow(df_gs)] %in% unique(df_gsfull$user_id[eidx_full:nrow(df_gsfull)]))

#' Check parsing failures; `read_csv` determines column type based on the first 1k observations. If later observations have a different type, parsing failures occur. 

#' Ignore parsing failures related to `cv_job_desc`, which was asked in earlier versions of the survey, but which no longer exists. Also ignore `duplicates_*` variables, which are related to internal calculations in the News/Media section of the survey, and `blocked date`, which doesn't exist for most respondents. 

# Check parsing failures

parsing_failures <- problems(df)
parsing_failures

dfprob <- df[parsing_failures$row, ]

# Filter out irrelevant/testing responses. 

df <- df %>% filter(tolower(consent_response) %in% 'yes', #consented
                    !is.na(treatment_r), # received treatment
                    date(`signed up`)>'2021-2-26', # after testing
                    # no researchers
                    !(`last name` %in% c('Rosenzweig', 'Offer-Westort', 'Ruiz', 'Li')
                      & `first name` %in% c('Leah', 'Molly', 'Ricardo', 'James')),
                    # user with error
                    !`messenger user id` %in% c(4106556386022673)) %>% 
  mutate(across(c(cv_hhold, crt_1, crt_2, crt_3, age_check), as.numeric))

df_evaluation <- df_evaluation %>% filter(tolower(consent_response) %in% 'yes', #consented
                                          !is.na(treatment_r), # received treatment
                                          date(`signed up`)>'2021-2-26', # after testing
                                          # no researchers
                                          !(`last name` %in% c('Rosenzweig', 'Offer-Westort', 'Ruiz', 'Li')
                                            & `first name` %in% c('Leah', 'Molly', 'Ricardo', 'James')),
                                          # Not in learning split
                                          !(`messenger user id` %in% df$`messenger user id`))%>% 
  mutate(across(c(cv_hhold, crt_1, crt_2, crt_3, age_check, duplicates_1), as.numeric),
         across(c(cov_tf_3, messenger_post_3, digital_phishing, timeline_post2_4, cov_tf_2, timeline_post_4,
                  cv_religion_other), as.character))


dff <- bind_rows(
  list(learning = df,
       evaluation = df_evaluation),
  .id = 'split'
)

df_gsfull <- df_gsfull %>% 
  filter(!duplicated(user_id)) # keep first response

df_gs <- df_gs %>% 
  # filter(`messenger user id` %in% df_gsfull$user_id) %>%  # keep only final data
  filter(!duplicated(`messenger user id`, fromLast=FALSE)) %>%  # keep first probability
  arrange(match(`messenger user id`, df_gsfull$user_id)) %>%  # order as in full data set
  mutate(ID = as.numeric(factor(`messenger user id`, 
                                levels = `messenger user id`)))

# save treatment probabilities
df <- dff %>% 
  inner_join(., df_gs, by = c('messenger user id')) %>% 
  bind_rows(., 
            filter(dff, (`messenger user id` %in% idsoi))) %>% 
  unique() %>% 
  arrange(ID) %>% 
  mutate(ID = seq_along(ID))


# when treatment is not assigned by server, use balanced probabilities
df <- df %>% 
  mutate_at(vars(paste0(paste0('probs_', 0:39))), 
            funs(case_when((is.na(treatment_on_time) | treatment_on_time !=1) ~ 1/40,
                           TRUE ~ .)))


# First batch is balanced random assignment
# Second batch starts ~1666
batch2 <- df$ID[which(df$`messenger user id` == 3742795819147217)]

# Third batch starts ~2777
batch3 <- df$ID[which(df$`messenger user id` == 3954224757971941)]

# Fourth batch starts, ~3888
batch4 <- df$ID[which(df$`messenger user id` == 3742417315846928)]

# Fifth batch starts, 'on' policy
batch5 <- df$ID[which(df$`messenger user id` == 3926303810741472)]

# Revised fifth batch
batch5r <- df$ID[which(df$`messenger user id` == 5637321389672841)]

df <- df %>% 
  mutate(batch = case_when(
    ID < batch2 ~ 1,
    ID < batch3 ~ 2,
    ID < batch4 ~ 3,
    ID < batch5 ~ 4,
    ID < batch5r ~ 5,
    TRUE ~ 6
  ))

df$batch[which(df$probs_0==0.025 & df$split == 'learning')] <- 1
df$batch[which(df$probs_0==0.25)] <- 5
df$batch[which((df$batch == 5) & (df$probs_0 !=0.25) )] <- 4
df$batch[which(df$split == 'learning' & df$batch == 6)] <- 4

df <- df %>%
  filter(batch !=5) %>% 
  mutate(batch = case_when(batch ==6 ~ 5,
                           TRUE ~ batch)) %>% 
  arrange(batch) %>%
  mutate(ID = seq_along(ID))



# Blind data ----


# recode userids
ids <- unique(df$`messenger user id`)
keys <- sample(length(ids))

df$ID <- keys[match(df$`messenger user id`, ids)]


df <- df %>% 
  filter(!duplicated(phone)) %>% 
  mutate(ID = seq_along(ID))

df <-
  df[, !(
    names(df) %in% c(
      'first name',
      'last name',
      'middle name',
      'internal user id',
      'messenger user id',
      'profile pic url',
      'chatfuel.user.id',
      names(df)[grep('^phone', names(df))]
    )
  ), ]

# replace any instances of phone numbers
df <- data.frame(lapply(df, function(x) {
  gsub(
    '0[6-9][0-9][0-9][0-9][0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[0-9]?|2[5|3]4[0-9][0-9][0-9][0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[0-9]?',
    999999,
    x
  )
}))

NA_cols <- colnames(df)[sapply(df, function(x)all(is.na(x)))]
df <- df[,!colnames(df)%in%NA_cols]

write.csv(df,  '../data/misinformation-data-anonymized.csv', row.names = FALSE)
