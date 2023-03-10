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

dropped_batch <- read_csv('../data/dropped_batch_ids.csv')
dropped_other <- read_csv('../data/other_dropped_ids.csv')


# Filter out irrelevant/testing responses. 
df <- df |> filter(#tolower(consent_response) %in% 'yes', #consented
  #!is.na(treatment_r), # received treatment
  date(`signed up`)>'2021-2-26', # after testing
  # !duplicated(phone, incomparables = NA),
  !(`messenger user id` %in% dropped_batch$id),
  !(`messenger user id` %in% dropped_other$id),
  # no researchers
  !(`last name` %in% c('Rosenzweig', 'Offer-Westort', 'Ruiz', 'Li')
    & `first name` %in% c('Leah', 'Molly', 'Ricardo', 'James')),
  # user with error
  !`messenger user id` %in% c(4106556386022673)) |> 
  mutate(across(c(cv_hhold, crt_1, crt_2, crt_3, age_check), as.numeric))

users_engaged <- nrow(df)
df <- df |> filter(tolower(consent_response) %in% 'yes')
users_consented <- nrow(df)

df <- df |> filter(!is.na(treatment_r))# received treatment
users_treated <- nrow(df)

write.table(data.frame(round = 1, users_engaged, users_consented, users_treated), 
            '../data/user_count.csv', 
            sep = ',',
            row.names = FALSE,
            append = FALSE)

df_evaluation <- df_evaluation |> filter(#tolower(consent_response) %in% 'yes', #consented
  #!is.na(treatment_r), # received treatment
  date(`signed up`)>'2021-2-26', # after testing
  # !duplicated(phone, incomparables = NA),
  !(`messenger user id` %in% dropped_batch$id),
  !(`messenger user id` %in% dropped_other$id),
  # no researchers
  !(`last name` %in% c('Rosenzweig', 'Offer-Westort', 'Ruiz', 'Li')
    & `first name` %in% c('Leah', 'Molly', 'Ricardo', 'James')),
  # Not in learning split
  !(`messenger user id` %in% df$`messenger user id`))|> 
  mutate(across(c(cv_hhold, crt_1, crt_2, crt_3, age_check, duplicates_1), as.numeric),
         across(c(cov_tf_3, messenger_post_3, digital_phishing, timeline_post2_4, cov_tf_2, timeline_post_4,
                  cv_religion_other), as.character))

users_engaged <- nrow(df_evaluation)
df_evaluation <- df_evaluation |> filter(tolower(consent_response) %in% 'yes')
users_consented <- nrow(df_evaluation)

df_evaluation <- df_evaluation |> filter(!is.na(treatment_r))# received treatment
users_treated <- nrow(df_evaluation)

write.table(data.frame(round = 2, users_engaged, users_consented, users_treated), 
            '../data/user_count.csv', 
            col.names = FALSE,
            sep = ',',
            row.names = FALSE,
            append = TRUE)

kept_ids = read_csv('../data/kept_ids.csv')


table(kept_ids$id %in% df$`messenger user id` | kept_ids$id %in% df_evaluation$`messenger user id` )
