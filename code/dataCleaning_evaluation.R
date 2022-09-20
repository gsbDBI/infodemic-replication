## ----packages, message = FALSE----------------------------------------------------------------------------
library(dataMaid) # write codebook
library(dplyr) # data manipulation
library(grf) # model prediction
library(readr) # read in files
library(stringr) # data cleaning


set.seed(94305)
source('utils.R')

dir.create(file.path('..', 'tables'), showWarnings = FALSE)
dir.create(file.path('..', 'figures'), showWarnings = FALSE)


## ----data-------------------------------------------------------------------------------------------------
files <- list.files('../data', 
                    pattern = '^misinformation-data.*.csv$', 
                    full.names = TRUE)

(INPUT_FILENAME <- files[which.max(file.info(files)$mtime)])

df <- suppressWarnings(read_csv(INPUT_FILENAME))


## ----variables--------------------------------------------------------------------------------------------
df <- df %>% 
  mutate_if(is.character, tolower)

df <- within(df, {
  # Male
  male = as.integer(cv_gender %in% 'male')
  male[which(is.na(male))] = 0
  
  # Age
  age_flag = 1 * (as.numeric(cv_age)>=120 | is.na(cv_age))
  age = case_when(as.numeric(cv_age)<120 ~ as.numeric(cv_age),
                  TRUE ~ mean(as.numeric(cv_age[which(as.numeric(cv_age)<120)])))
  age_check_flag = 1*(cv_age != age_check | is.na(cv_age) | is.na(age_check))
  
  # Education
  ed = as.integer(cv_education)
  ed[which(is.na(ed))] = 0
  ed_flag = 1 * (ed %in% 0)
  
  # Urban/rural
  urban = 1 * (urban_rural %in% 'mostly urban')
  
  # Religion
  cv_religion[which(is.na(cv_religion))] = 'NA'
  rel_none = 1 * (cv_religion %in% 'none')
  rel_christian = 1 * (cv_religion %in% 'christian')
  rel_muslim = 1 * (cv_religion %in% 'muslim')
  rel_traditionalist = 1 * (cv_religion %in% 'traditionalist')
  rel_other = 1 - (rel_none + rel_christian + rel_muslim + rel_traditionalist)
  # Christian denomination
  denom_pentecostal = 1 * grepl('pentecostal', cv_denom, ignore.case = TRUE)
  # Religiosity
  religiosity_flag = 0
  religiosity = sapply(cv_religion_freq, function(x) switch(x ,
                                                            'never' = 1,
                                                            '< once a month' = 2, 
                                                            '1-3 times a month' = 3,
                                                            'once a week' = 4,
                                                            '> once a week' = 5,
                                                            'daily' = 6,
                                                            0))
  religiosity_flag = as.integer(religiosity %in% 0)
  
  # Belief in God's Control
  god = 1 * (cv_gods_control %in% '1')
  
  # Locus of Control
  locus = as.integer(cv_locus_of_control)
  locus[which(is.na(locus))] =  0
  locus_flag = 1 * (locus %in% 0)
  
  # Index of Scientific questions
  cv_science1[which(is.na(cv_science1))] = '0'
  cv_science2[which(is.na(cv_science2))] = '0'
  science = 1 * (cv_science1 %in% '1') + 1 * (cv_science2 %in% '1')
  science_flag = 1 - (1 * (cv_science1 %in% '1') + 1 * (cv_science1 %in% '2')) * (
    1 * (cv_science2 %in% '1') + 1 * (cv_science2 %in% '2'))
  
  # Digital literacy index
  digital_phishing = suppressWarnings(as.integer(digital_phishing)*
                                        (as.integer(digital_phishing)<5))
  digital_phishing[which(is.na(digital_phishing))] = 0
  digital_hashtag = suppressWarnings(as.integer(digital_hashtag)*
                                       (as.integer(digital_hashtag)<5))
  digital_hashtag[which(is.na(digital_hashtag))] = 0
  digital_jpg = suppressWarnings(as.integer(digital_jpg)*
                                   (as.integer(digital_jpg)<5))
  digital_jpg[which(is.na(digital_jpg))] = 0
  digital_malware = suppressWarnings(as.integer(digital_malware)*
                                       (as.integer(digital_malware)<5))
  digital_malware[which(is.na(digital_malware))] = 0
  digital_cache = suppressWarnings(as.integer(digital_cache)*
                                     (as.integer(digital_cache)<5))
  digital_cache[which(is.na(digital_cache))] = 0
  digital_rss = suppressWarnings(as.integer(digital_rss)*
                                   (as.integer(digital_rss)<5))
  digital_rss[which(is.na(digital_rss))] = 0
  
  digital_friends[which(is.na(digital_friends))] = 0
  digital_life[which(is.na(digital_life))] = 0
  digital_work[which(is.na(digital_work))] = 0
  
  dli = (
    # First element of DLI
    (digital_phishing + digital_hashtag + digital_jpg + digital_malware + 
       digital_cache + digital_rss)/5
    # Second element of DLI, friends
    + 2 * (digital_friends %in% 'somewhat agree') + 4 * (
      digital_friends %in% 'somewhat disagree') + 6 * (
        digital_friends %in% 'strongly disagree')
    # Third element of DLI, life
    + 2 * (digital_life %in% 'somewhat disagree') + 4 * (digital_life %in% 'somewhat agree') + 6 * (
      digital_life %in% 'strongly agree')
    # Fourth element of DLI, work
    + 2 * (digital_work %in% 'somewhat disagree') + 4 * (digital_work %in% 'somewhat agree') + 6 * (
      digital_work %in% 'strongly agree'))
  
  # Facebook posts
  fb_post_flag = 0
  fb_post = sapply(fb_post, function(x) switch(x,
                                               '1-2' = 1,
                                               '3-4' = 2,
                                               '5+' = 3,
                                               0))
  fb_post_flag = as.integer(fb_post %in% 0)
  
  fb_msg_flag = 0
  fb_msg = sapply(fb_msg, function(x) switch(x,
                                             '1-3' = 1,
                                             '4-6' = 2,
                                             'more than 6 times' = 3,
                                             0))
  fb_msg_flag = as.integer(fb_msg %in% 0)
  
  # Cognitive reflection test
  crt = 1 * (crt_1 %in% '5') + 1 * (crt_2 %in% '5') + 1 * (crt_3 %in% '47')
  crt[which(is.na(crt))] = 0
  
  # Household index
  hhi = (
    1 * (cv_radio %in% 'i/my household owns') +  #
      1 * (cv_tv %in% 'i/my household owns') +  #
      1 * (cv_moto %in% 'i/my household owns') +  #
      1 * (cv_computer %in% 'i/my household owns') +  #
      1 * (cv_bank %in% 'i/my household owns') +  #
      1 * (cv_phone %in% 'i/my household owns') +  #
      1 * (cv_bike %in% 'i/my household owns')
  )
  hhi[which(is.na(hhi))] = 0
  hhi_flag = (1 - (
    (1 * (cv_radio %in% 'i/my household owns') + 1 * (cv_radio %in% 'do not own')) *  #
      (1 * (cv_tv %in% 'i/my household owns') + 1 * (cv_tv %in% 'do not own')) *  #
      (1 * (cv_moto %in% 'i/my household owns') + 1 * (cv_moto %in% 'do not own')) *  #
      (1 * (cv_computer %in% 'i/my household owns') + 1 * (cv_computer %in% 'do not own')) *  #
      (1 * (cv_bank %in% 'i/my household owns') + 1 * (cv_bank %in% 'do not own')) *  #
      (1 * (cv_phone %in% 'i/my household owns') + 1 * (cv_phone %in% 'do not own')) *  #
      (1 * (cv_bike %in% 'i/my household owns') + 1 * (cv_bike %in% 'do not own')))
  )
  
  hhi_flag[which(is.na(hhi_flag))] = 1
  
  # Job with cash income
  cash = 1 * (cv_job %in% 'yes')
  cash[which(is.na(cash))] = 0
  
  # Number of people in household
  hh = suppressWarnings(as.integer(cv_hhold)*(as.integer(cv_hhold)>0 & 
                                                as.integer(cv_hhold)<50))
  hh[is.na(hh)] = 0
  hh_flag = 1 * (hh %in% 0)
  
  # Political party affiliation
  pol =  ifelse(party != 'none', 
                1 * (party %in% '1: jubilee') + 1 * (party %in% '1: apc'), 
                1 * (party_voted %in% '1: voted kenyatta') + 
                  1 * (party_voted %in% '1: voted buhari'))
  pol[which(is.na(pol))] = 0
  
  
  # Concern regarding COVID-19
  cov_concern = 1 * (cv_risk %in% 'not at all worried') + 2 * (cv_risk %in% 'somewhat worried') + 3 * (
    cv_risk %in% 'very worried')
  cov_concern[which(is.na(cov_concern))] = 0
  cov_concern_flag = 1 * (cov_concern %in% 0)
  
  # COVID-19 information
  cov_info = 1 * (cov_tf_1 %in% 'true') + 1 * (cov_tf_2 %in% 'true') + 1 * (cov_tf_3 %in% 'true')
  cov_info[which(is.na(cov_info))] = 0
  
  # Perceived government efficacy on COVID-19
  cov_efficacy = 1 * (cv_efficacy %in% 'very poorly') + 2 * (cv_efficacy %in% 'somewhat poorly') + 3 * (
    cv_efficacy %in% 'somewhat well') + 4 * (cv_efficacy %in% 'very well')
  cov_efficacy[which(is.na(cov_efficacy))] = 0
  cov_efficacy_flag = 1 * (cov_efficacy %in% 0)
  
  # Indicators for individual stimulus
  # False stimuli from respective countries
  stimf1 = (1 * (dv_stimulus_pre1 %in% 'fn1') + 1 * (dv_stimulus_pre2 %in% 'fn1') +
              1 * (dv_stimulus_pre3 %in% 'fn1') + 1 * (dv_stimulus_pre4 %in% 'fn1'))
  
  stimf2 = (1 * (dv_stimulus_pre1 %in% 'fn2') + 1 * (dv_stimulus_pre2 %in% 'fn2') +
              1 * (dv_stimulus_pre3 %in% 'fn2') + 1 * (dv_stimulus_pre4 %in% 'fn2') +
              1 * (dv_stimulus_pre1 %in% 'fk2') + 1 * (dv_stimulus_pre2 %in% 'fk2') +
              1 * (dv_stimulus_pre3 %in% 'fk2') + 1 * (dv_stimulus_pre4 %in% 'fk2'))
  
  stimf3 = (1 * (dv_stimulus_pre1 %in% 'fn3') + 1 * (dv_stimulus_pre2 %in% 'fn3') +
              1 * (dv_stimulus_pre3 %in% 'fn3') + 1 * (dv_stimulus_pre4 %in% 'fn3') +
              1 * (dv_stimulus_pre1 %in% 'fk3') + 1 * (dv_stimulus_pre2 %in% 'fk3') +
              1 * (dv_stimulus_pre3 %in% 'fk3') + 1 * (dv_stimulus_pre4 %in% 'fk3'))
  
  stimf4 = (1 * (dv_stimulus_pre1 %in% 'fn4') + 1 * (dv_stimulus_pre2 %in% 'fn4') +
              1 * (dv_stimulus_pre3 %in% 'fn4') + 1 * (dv_stimulus_pre4 %in% 'fn4') +
              1 * (dv_stimulus_pre1 %in% 'fk4') + 1 * (dv_stimulus_pre2 %in% 'fk4') +
              1 * (dv_stimulus_pre3 %in% 'fk4') + 1 * (dv_stimulus_pre4 %in% 'fk4'))
  
  stimf5 = (1 * (dv_stimulus_pre1 %in% 'fn5') + 1 * (dv_stimulus_pre2 %in% 'fn5') +
              1 * (dv_stimulus_pre3 %in% 'fn5') + 1 * (dv_stimulus_pre4 %in% 'fn5') +
              1 * (dv_stimulus_pre1 %in% 'fk5') + 1 * (dv_stimulus_pre2 %in% 'fk5') +
              1 * (dv_stimulus_pre3 %in% 'fk5') + 1 * (dv_stimulus_pre4 %in% 'fk5'))
  
  stimf6 = (1 * (dv_stimulus_pre1 %in% 'fn6') + 1 * (dv_stimulus_pre2 %in% 'fn6') +
              1 * (dv_stimulus_pre3 %in% 'fn6') + 1 * (dv_stimulus_pre4 %in% 'fn6') +
              1 * (dv_stimulus_pre1 %in% 'fk6') + 1 * (dv_stimulus_pre2 %in% 'fk6') +
              1 * (dv_stimulus_pre3 %in% 'fk6') + 1 * (dv_stimulus_pre4 %in% 'fk6'))
  
  stimf7 = (1 * (dv_stimulus_pre1 %in% 'fn7') + 1 * (dv_stimulus_pre2 %in% 'fn7') +
              1 * (dv_stimulus_pre3 %in% 'fn7') + 1 * (dv_stimulus_pre4 %in% 'fn7') +
              1 * (dv_stimulus_pre1 %in% 'fk7') + 1 * (dv_stimulus_pre2 %in% 'fk7') +
              1 * (dv_stimulus_pre3 %in% 'fk7') + 1 * (dv_stimulus_pre4 %in% 'fk7'))
  
  stimf8 = (1 * (dv_stimulus_pre1 %in% 'fn8') + 1 * (dv_stimulus_pre2 %in% 'fn8') +
              1 * (dv_stimulus_pre3 %in% 'fn8') + 1 * (dv_stimulus_pre4 %in% 'fn8') +
              1 * (dv_stimulus_pre1 %in% 'fk8') + 1 * (dv_stimulus_pre2 %in% 'fk8') +
              1 * (dv_stimulus_pre3 %in% 'fk8') + 1 * (dv_stimulus_pre4 %in% 'fk8'))
  
  stimf9 = (1 * (dv_stimulus_pre1 %in% 'fn9') + 1 * (dv_stimulus_pre2 %in% 'fn9') +
              1 * (dv_stimulus_pre3 %in% 'fn9') + 1 * (dv_stimulus_pre4 %in% 'fn9') +
              1 * (dv_stimulus_pre1 %in% 'fk9') + 1 * (dv_stimulus_pre2 %in% 'fk9') +
              1 * (dv_stimulus_pre3 %in% 'fk9') + 1 * (dv_stimulus_pre4 %in% 'fk9'))
  
  stimf10 = (1 * (dv_stimulus_pre1 %in% 'fn10') + 1 * (dv_stimulus_pre2 %in% 'fn10') +
               1 * (dv_stimulus_pre3 %in% 'fn10') + 1 * (dv_stimulus_pre4 %in% 'fn10') +
               1 * (dv_stimulus_pre1 %in% 'fk10') + 1 * (dv_stimulus_pre2 %in% 'fk10') +
               1 * (dv_stimulus_pre3 %in% 'fk10') + 1 * (dv_stimulus_pre4 %in% 'fk10'))
  
  stimf11 = (1 * (dv_stimulus_pre1 %in% 'fn11') + 1 * (dv_stimulus_pre2 %in% 'fn11') +
               1 * (dv_stimulus_pre3 %in% 'fn11') + 1 * (dv_stimulus_pre4 %in% 'fn11') +
               1 * (dv_stimulus_pre1 %in% 'fk11') + 1 * (dv_stimulus_pre2 %in% 'fk11') +
               1 * (dv_stimulus_pre3 %in% 'fk11') + 1 * (dv_stimulus_pre4 %in% 'fk11'))
  
  stimf12 = (1 * (dv_stimulus_pre1 %in% 'fn12') + 1 * (dv_stimulus_pre2 %in% 'fn12') +
               1 * (dv_stimulus_pre3 %in% 'fn12') + 1 * (dv_stimulus_pre4 %in% 'fn12') +
               1 * (dv_stimulus_pre1 %in% 'fk12') + 1 * (dv_stimulus_pre2 %in% 'fk12') +
               1 * (dv_stimulus_pre3 %in% 'fk12') + 1 * (dv_stimulus_pre4 %in% 'fk12'))
  
  stimf13 = (1 * (dv_stimulus_pre1 %in% 'fn13') + 1 * (dv_stimulus_pre2 %in% 'fn13') +
               1 * (dv_stimulus_pre3 %in% 'fn13') + 1 * (dv_stimulus_pre4 %in% 'fn13') +
               1 * (dv_stimulus_pre1 %in% 'fk13') + 1 * (dv_stimulus_pre2 %in% 'fk13') +
               1 * (dv_stimulus_pre3 %in% 'fk13') + 1 * (dv_stimulus_pre4 %in% 'fk13'))
  
  stimf14 = (1 * (dv_stimulus_pre1 %in% 'fn14') + 1 * (dv_stimulus_pre2 %in% 'fn14') +
               1 * (dv_stimulus_pre3 %in% 'fn14') + 1 * (dv_stimulus_pre4 %in% 'fn14'))
  
  stimf15 = (1 * (dv_stimulus_pre1 %in% 'fn15') + 1 * (dv_stimulus_pre2 %in% 'fn15') +
               1 * (dv_stimulus_pre3 %in% 'fn15') + 1 * (dv_stimulus_pre4 %in% 'fn15'))
  
  stimf16 = (1 * (dv_stimulus_pre1 %in% 'fn16') + 1 * (dv_stimulus_pre2 %in% 'fn16') +
               1 * (dv_stimulus_pre3 %in% 'fn16') + 1 * (dv_stimulus_pre4 %in% 'fn16'))
  
  # True stimuli from respective countries
  stimt1 = (1 * (dv_stimulus_pre1 %in% 'tn1') + 1 * (dv_stimulus_pre2 %in% 'tn1') +
              1 * (dv_stimulus_pre3 %in% 'tn1') + 1 * (dv_stimulus_pre4 %in% 'tn1') +
              1 * (dv_stimulus_pre1 %in% 'tk1') + 1 * (dv_stimulus_pre2 %in% 'tk1') +
              1 * (dv_stimulus_pre3 %in% 'tk1') + 1 * (dv_stimulus_pre4 %in% 'tk1'))
  
  stimt2 = (1 * (dv_stimulus_pre1 %in% 'tn2') + 1 * (dv_stimulus_pre2 %in% 'tn2') +
              1 * (dv_stimulus_pre3 %in% 'tn2') + 1 * (dv_stimulus_pre4 %in% 'tn2') +
              1 * (dv_stimulus_pre1 %in% 'tk2') + 1 * (dv_stimulus_pre2 %in% 'tk2') +
              1 * (dv_stimulus_pre3 %in% 'tk2') + 1 * (dv_stimulus_pre4 %in% 'tk2'))
  
  stimt3 = (1 * (dv_stimulus_pre1 %in% 'tn3') + 1 * (dv_stimulus_pre2 %in% 'tn3') +
              1 * (dv_stimulus_pre3 %in% 'tn3') + 1 * (dv_stimulus_pre4 %in% 'tn3') +
              1 * (dv_stimulus_pre1 %in% 'tk3') + 1 * (dv_stimulus_pre2 %in% 'tk3') +
              1 * (dv_stimulus_pre3 %in% 'tk3') + 1 * (dv_stimulus_pre4 %in% 'tk3'))
  
  stimt4 = (1 * (dv_stimulus_pre1 %in% 'tn4') + 1 * (dv_stimulus_pre2 %in% 'tn4') +
              1 * (dv_stimulus_pre3 %in% 'tn4') + 1 * (dv_stimulus_pre4 %in% 'tn4') +
              1 * (dv_stimulus_pre1 %in% 'tk4') + 1 * (dv_stimulus_pre2 %in% 'tk4') +
              1 * (dv_stimulus_pre3 %in% 'tk4') + 1 * (dv_stimulus_pre4 %in% 'tk4'))
  
  stimt5 = (1 * (dv_stimulus_pre1 %in% 'tn5') + 1 * (dv_stimulus_pre2 %in% 'tn5') +
              1 * (dv_stimulus_pre3 %in% 'tn5') + 1 * (dv_stimulus_pre4 %in% 'tn5') +
              1 * (dv_stimulus_pre1 %in% 'tk5') + 1 * (dv_stimulus_pre2 %in% 'tk5') +
              1 * (dv_stimulus_pre3 %in% 'tk5') + 1 * (dv_stimulus_pre4 %in% 'tk5'))
  
  stimt6 = (1 * (dv_stimulus_pre1 %in% 'tn6') + 1 * (dv_stimulus_pre2 %in% 'tn6') +
              1 * (dv_stimulus_pre3 %in% 'tn6') + 1 * (dv_stimulus_pre4 %in% 'tn6') +
              1 * (dv_stimulus_pre1 %in% 'tk6') + 1 * (dv_stimulus_pre2 %in% 'tk6') +
              1 * (dv_stimulus_pre3 %in% 'tk6') + 1 * (dv_stimulus_pre4 %in% 'tk6'))
  
  # True stimuli from both countries
  stimb1 = (1 * (dv_stimulus_pre1 %in% 'tb1') + 1 * (dv_stimulus_pre2 %in% 'tb1') +
              1 * (dv_stimulus_pre3 %in% 'tb1') + 1 * (dv_stimulus_pre4 %in% 'tb1'))
  stimb2 = (1 * (dv_stimulus_pre1 %in% 'tb2') + 1 * (dv_stimulus_pre2 %in% 'tb2') +
              1 * (dv_stimulus_pre3 %in% 'tb2') + 1 * (dv_stimulus_pre4 %in% 'tb2'))
  stimb3 = (1 * (dv_stimulus_pre1 %in% 'tb3') + 1 * (dv_stimulus_pre2 %in% 'tb3') +
              1 * (dv_stimulus_pre3 %in% 'tb3') + 1 * (dv_stimulus_pre4 %in% 'tb3'))
  stimb4 = (1 * (dv_stimulus_pre1 %in% 'tb4') + 1 * (dv_stimulus_pre2 %in% 'tb4') +
              1 * (dv_stimulus_pre3 %in% 'tb4') + 1 * (dv_stimulus_pre4 %in% 'tb4'))
  stimb5 = (1 * (dv_stimulus_pre1 %in% 'tb5') + 1 * (dv_stimulus_pre2 %in% 'tb5') +
              1 * (dv_stimulus_pre3 %in% 'tb5') + 1 * (dv_stimulus_pre4 %in% 'tb5'))
  
  # Country
  nigeria = 1*(df$country1 %in% 'nigeria')
  
  # Post-treatment variables and response
  post_false = 0
  post_true = 0
  
  # Response to stimuli
  dv_1 = (dv_timeline_pre1 %in% 'yes') + (dv_send_pre1 %in% 'yes')
  dv_2 = (dv_timeline_pre2 %in% 'yes') + (dv_send_pre2 %in% 'yes')
  dv_3 = (dv_timeline_pre3 %in% 'yes') + (dv_send_pre3 %in% 'yes')
  dv_4 = (dv_timeline_pre4 %in% 'yes') + (dv_send_pre4 %in% 'yes')
  
  dv_5 = (dv_timeline_post5 %in% 'yes') + (dv_send_post5 %in% 'yes')
  dv_6 = (dv_timeline_post6 %in% 'yes') + (dv_send_post6 %in% 'yes')
  dv_7 = (dv_timeline_post7 %in% 'yes') + (dv_send_post7 %in% 'yes')
  dv_8 = (dv_timeline_post8 %in% 'yes') + (dv_send_post8 %in% 'yes')
  
  
  pre_true = ((substr(dv_stimulus_pre1, 0, 1)%in%'t')*dv_1 +
                (substr(dv_stimulus_pre2, 0, 1)%in%'t')*dv_2 +
                (substr(dv_stimulus_pre3, 0, 1)%in%'t')*dv_3 +
                (substr(dv_stimulus_pre4, 0, 1)%in%'t')*dv_4)
  
  pre_false = ((substr(dv_stimulus_pre1, 0, 1)%in%'f')*dv_1 +
                 (substr(dv_stimulus_pre2, 0, 1)%in%'f')*dv_2 +
                 (substr(dv_stimulus_pre3, 0, 1)%in%'f')*dv_3 +
                 (substr(dv_stimulus_pre4, 0, 1)%in%'f')*dv_4)
  
  pre_timeline_true = ((substr(dv_stimulus_pre1, 0, 1)%in%'t')*(dv_timeline_pre1 %in% 'yes') +
                         (substr(dv_stimulus_pre2, 0, 1)%in%'t')*(dv_timeline_pre2 %in% 'yes') +
                         (substr(dv_stimulus_pre3, 0, 1)%in%'t')*(dv_timeline_pre3 %in% 'yes') +
                         (substr(dv_stimulus_pre4, 0, 1)%in%'t')*(dv_timeline_pre4 %in% 'yes'))
  
  pre_timeline_false = ((substr(dv_stimulus_pre1, 0, 1)%in%'f')*(dv_timeline_pre1 %in% 'yes') +
                          (substr(dv_stimulus_pre2, 0, 1)%in%'f')*(dv_timeline_pre2 %in% 'yes') +
                          (substr(dv_stimulus_pre3, 0, 1)%in%'f')*(dv_timeline_pre3 %in% 'yes') +
                          (substr(dv_stimulus_pre4, 0, 1)%in%'f')*(dv_timeline_pre4 %in% 'yes'))
  
  pre_send_true = ((substr(dv_stimulus_pre1, 0, 1)%in%'t')*(dv_send_pre1 %in% 'yes') +
                     (substr(dv_stimulus_pre2, 0, 1)%in%'t')*(dv_send_pre2 %in% 'yes') +
                     (substr(dv_stimulus_pre3, 0, 1)%in%'t')*(dv_send_pre3 %in% 'yes') +
                     (substr(dv_stimulus_pre4, 0, 1)%in%'t')*(dv_send_pre4 %in% 'yes'))
  
  pre_send_false = ((substr(dv_stimulus_pre1, 0, 1)%in%'f')*(dv_send_pre1 %in% 'yes') +
                      (substr(dv_stimulus_pre2, 0, 1)%in%'f')*(dv_send_pre2 %in% 'yes') +
                      (substr(dv_stimulus_pre3, 0, 1)%in%'f')*(dv_send_pre3 %in% 'yes') +
                      (substr(dv_stimulus_pre4, 0, 1)%in%'f')*(dv_send_pre4 %in% 'yes'))
  
  
  
  
  post_timeline_true = ((substr(dv_stimulus_post5, 0, 1)%in%'t')*(dv_timeline_post5 %in% 'yes') +
                          (substr(dv_stimulus_post6, 0, 1)%in%'t')*(dv_timeline_post6 %in% 'yes') +
                          (substr(dv_stimulus_post7, 0, 1)%in%'t')*(dv_timeline_post7 %in% 'yes') +
                          (substr(dv_stimulus_post8, 0, 1)%in%'t')*(dv_timeline_post8 %in% 'yes'))
  
  post_timeline_false = ((substr(dv_stimulus_post5, 0, 1)%in%'f')*(dv_timeline_post5 %in% 'yes') +
                           (substr(dv_stimulus_post6, 0, 1)%in%'f')*(dv_timeline_post6 %in% 'yes') +
                           (substr(dv_stimulus_post7, 0, 1)%in%'f')*(dv_timeline_post7 %in% 'yes') +
                           (substr(dv_stimulus_post8, 0, 1)%in%'f')*(dv_timeline_post8 %in% 'yes'))
  
  post_send_true = ((substr(dv_stimulus_post5, 0, 1)%in%'t')*(dv_send_post5 %in% 'yes') +
                      (substr(dv_stimulus_post6, 0, 1)%in%'t')*(dv_send_post6 %in% 'yes') +
                      (substr(dv_stimulus_post7, 0, 1)%in%'t')*(dv_send_post7 %in% 'yes') +
                      (substr(dv_stimulus_post8, 0, 1)%in%'t')*(dv_send_post8 %in% 'yes'))
  
  post_send_false = ((substr(dv_stimulus_post5, 0, 1)%in%'f')*(dv_send_post5 %in% 'yes') +
                       (substr(dv_stimulus_post6, 0, 1)%in%'f')*(dv_send_post6 %in% 'yes') +
                       (substr(dv_stimulus_post7, 0, 1)%in%'f')*(dv_send_post7 %in% 'yes') +
                       (substr(dv_stimulus_post8, 0, 1)%in%'f')*(dv_send_post8 %in% 'yes'))
  
  
  
  # pre-treatment strata
  strat_send_false0 = 1*(pre_send_false == 0)
  strat_send_false1 = 1*(pre_send_false == 1)
  strat_send_false2 = 1*(pre_send_false == 2)
  strat_send_true0 = 1*(pre_send_true == 0)
  strat_send_true1 = 1*(pre_send_true == 1)
  strat_send_true2 = 1*(pre_send_true == 2)
  strat_timeline_false0 = 1*(pre_timeline_false == 0)
  strat_timeline_false1 = 1*(pre_timeline_false == 1)
  strat_timeline_false2 = 1*(pre_timeline_false == 2)
  strat_timeline_true0 = 1*(pre_timeline_true == 0)
  strat_timeline_true1 = 1*(pre_timeline_true == 1)
  strat_timeline_true2 = 1*(pre_timeline_true == 2)
  
  # Response to post-treatment stimuli
  attrited = 1*(!(!is.na(dv_send_post8) & !is.na(dv_timeline_post8)))
  
  post_true = case_when((incomplete %in% 1) & !(!is.na(dv_send_post8) & !is.na(dv_timeline_post8)) ~ pre_true,
                        TRUE ~ ((substr(dv_stimulus_post5, 0, 1)%in%'t')*dv_5 +
                                  (substr(dv_stimulus_post6, 0, 1)%in%'t')*dv_6 +
                                  (substr(dv_stimulus_post7, 0, 1)%in%'t')*dv_7 +
                                  (substr(dv_stimulus_post8, 0, 1)%in%'t')*dv_8))
  
  post_false = case_when((incomplete %in% 1) & !(!is.na(dv_send_post8) & !is.na(dv_timeline_post8)) ~ pre_false,
                         TRUE ~ ((substr(dv_stimulus_post5, 0, 1)%in%'f')*dv_5 +
                                   (substr(dv_stimulus_post6, 0, 1)%in%'f')*dv_6 +
                                   (substr(dv_stimulus_post7, 0, 1)%in%'f')*dv_7 +
                                   (substr(dv_stimulus_post8, 0, 1)%in%'f')*dv_8))
  
  post_true_any = ((substr(dv_stimulus_post5, 0, 1)%in%'t')*(dv_5>0) +
                     (substr(dv_stimulus_post6, 0, 1)%in%'t')*(dv_6>0) +
                     (substr(dv_stimulus_post7, 0, 1)%in%'t')*(dv_7>0) +
                     (substr(dv_stimulus_post8, 0, 1)%in%'t')*(dv_8>0))
  
  post_false_any = ((substr(dv_stimulus_post5, 0, 1)%in%'f')*(dv_5>0) +
                      (substr(dv_stimulus_post6, 0, 1)%in%'f')*(dv_6>0) +
                      (substr(dv_stimulus_post7, 0, 1)%in%'f')*(dv_7>0) +
                      (substr(dv_stimulus_post8, 0, 1)%in%'f')*(dv_8>0))
  # Response variable; use pre-stimuli response if missing
  Y = - post_false + 0.5 * post_true
  
  # Additional response measures
  
  pre_true_any = ((substr(dv_stimulus_pre1, 0, 1)%in%'t')*(dv_1>0) +
                    (substr(dv_stimulus_pre2, 0, 1)%in%'t')*(dv_2>0) +
                    (substr(dv_stimulus_pre3, 0, 1)%in%'t')*(dv_3>0) +
                    (substr(dv_stimulus_pre4, 0, 1)%in%'t')*(dv_4>0))
  
  pre_false_any = ((substr(dv_stimulus_pre1, 0, 1)%in%'f')*(dv_1>0) +
                     (substr(dv_stimulus_pre2, 0, 1)%in%'f')*(dv_2>0) +
                     (substr(dv_stimulus_pre3, 0, 1)%in%'f')*(dv_3>0) +
                     (substr(dv_stimulus_pre4, 0, 1)%in%'f')*(dv_4>0))
  
  post_true_any = ((substr(dv_stimulus_post5, 0, 1)%in%'t')*(dv_5>0) +
                     (substr(dv_stimulus_post6, 0, 1)%in%'t')*(dv_6>0) +
                     (substr(dv_stimulus_post7, 0, 1)%in%'t')*(dv_7>0) +
                     (substr(dv_stimulus_post8, 0, 1)%in%'t')*(dv_8>0))
  
  post_true_timeline = ((substr(dv_stimulus_post5, 0, 1)%in%'t')*(dv_timeline_post5%in% 'yes') +
                          (substr(dv_stimulus_post6, 0, 1)%in%'t')*(dv_timeline_post6%in% 'yes') +
                          (substr(dv_stimulus_post7, 0, 1)%in%'t')*(dv_timeline_post7%in% 'yes') +
                          (substr(dv_stimulus_post8, 0, 1)%in%'t')*(dv_timeline_post8%in% 'yes'))
  
  post_true_send = ((substr(dv_stimulus_post5, 0, 1)%in%'t')*(dv_send_post5%in% 'yes') +
                      (substr(dv_stimulus_post6, 0, 1)%in%'t')*(dv_send_post6%in% 'yes') +
                      (substr(dv_stimulus_post7, 0, 1)%in%'t')*(dv_send_post7%in% 'yes') +
                      (substr(dv_stimulus_post8, 0, 1)%in%'t')*(dv_send_post8%in% 'yes'))
  
  post_true_seen = ((substr(dv_stimulus_post5, 0, 1)%in%'t') +
                      (substr(dv_stimulus_post6, 0, 1)%in%'t') +
                      (substr(dv_stimulus_post7, 0, 1)%in%'t') +
                      (substr(dv_stimulus_post8, 0, 1)%in%'t'))
  
  post_false_any = ((substr(dv_stimulus_post5, 0, 1)%in%'f')*(dv_5>0) +
                      (substr(dv_stimulus_post6, 0, 1)%in%'f')*(dv_6>0) +
                      (substr(dv_stimulus_post7, 0, 1)%in%'f')*(dv_7>0) +
                      (substr(dv_stimulus_post8, 0, 1)%in%'f')*(dv_8>0))
  
  post_false_timeline = ((substr(dv_stimulus_post5, 0, 1)%in%'f')*(dv_timeline_post5%in% 'yes') +
                           (substr(dv_stimulus_post6, 0, 1)%in%'f')*(dv_timeline_post6%in% 'yes') +
                           (substr(dv_stimulus_post7, 0, 1)%in%'f')*(dv_timeline_post7%in% 'yes') +
                           (substr(dv_stimulus_post8, 0, 1)%in%'f')*(dv_timeline_post8%in% 'yes'))
  
  post_false_send = ((substr(dv_stimulus_post5, 0, 1)%in%'f')*(dv_send_post5%in% 'yes') +
                       (substr(dv_stimulus_post6, 0, 1)%in%'f')*(dv_send_post6%in% 'yes') +
                       (substr(dv_stimulus_post7, 0, 1)%in%'f')*(dv_send_post7%in% 'yes') +
                       (substr(dv_stimulus_post8, 0, 1)%in%'f')*(dv_send_post8%in% 'yes'))
  
  post_false_seen = ((substr(dv_stimulus_post5, 0, 1)%in%'f') +
                       (substr(dv_stimulus_post6, 0, 1)%in%'f') +
                       (substr(dv_stimulus_post7, 0, 1)%in%'f') +
                       (substr(dv_stimulus_post8, 0, 1)%in%'f'))
  
  post_true_prop = case_when(post_true_seen > 0 ~ post_true_any/ post_true_seen,
                             TRUE ~ 0)
  post_true_timeline_prop = case_when(post_true_seen > 0 ~ post_true_timeline/ post_true_seen,
                                      TRUE ~ 0)
  post_true_send_prop = case_when(post_true_seen > 0 ~ post_true_send/ post_true_seen,
                                  TRUE ~ 0)
  post_false_prop = case_when(post_false_seen > 0 ~ post_false_any/ post_false_seen,
                              TRUE ~ 0)
  post_false_timeline_prop = case_when(post_false_seen > 0 ~ post_false_timeline/ post_false_seen,
                                       TRUE ~ 0)
  post_false_send_prop = case_when(post_false_seen > 0 ~ post_false_send/ post_false_seen,
                                   TRUE ~ 0)
}
)


## ----click_through_rates----------------------------------------------------------------------------------
df <- 
  df %>% 
  # true click through rate
  mutate(true_any = post_true_any + pre_true_any) %>% 
  
  mutate(share_TB1 = coalesce(share_TB1, 0)) %>% 
  mutate(share_TB2 = coalesce(share_TB2, 0)) %>% 
  mutate(share_TB3 = coalesce(share_TB3, 0)) %>% 
  mutate(share_TB4 = coalesce(share_TB4, 0)) %>% 
  mutate(share_TB5 = coalesce(share_TB5, 0)) %>% 
  
  mutate(share_TK1 = coalesce(share_TK1, 0)) %>% 
  mutate(share_TK2 = coalesce(share_TK2, 0)) %>% 
  mutate(share_TK3 = coalesce(share_TK3, 0)) %>% 
  mutate(share_TK4 = coalesce(share_TK4, 0)) %>% 
  mutate(share_TK5 = coalesce(share_TK5, 0)) %>% 
  mutate(share_TK6 = coalesce(share_TK6, 0)) %>% 
  
  mutate(share_TN1 = coalesce(share_TN1, 0)) %>% 
  mutate(share_TN2 = coalesce(share_TN2, 0)) %>% 
  mutate(share_TN3 = coalesce(share_TN3, 0)) %>% 
  mutate(share_TN4 = coalesce(share_TN4, 0)) %>% 
  mutate(share_TN6 = coalesce(share_TN6, 0)) %>% 

  mutate(true_shared = share_TB1 + share_TB2 + share_TB3 + share_TB4 + share_TB5 +
                       share_TK1 + share_TK2 + share_TK3 + share_TK4 + share_TK5 + share_TK6 +
                       share_TN1 + share_TN2 + share_TN3 + share_TN4 + share_TN6) %>% 
  
  mutate(true_click_through_rate = coalesce(true_shared / true_any, 0)) %>% 
  
  # false click through rate
  mutate(link_FK2 = coalesce(link_FK2, 0)) %>% 
  mutate(link_FK3 = coalesce(link_FK3, 0)) %>% 
  mutate(link_FK4 = coalesce(link_FK4, 0)) %>% 
  mutate(link_FK5 = coalesce(link_FK5, 0)) %>% 
  mutate(link_FK6 = coalesce(link_FK6, 0)) %>% 
  mutate(link_FK7 = coalesce(link_FK7, 0)) %>% 
  mutate(link_FK8 = coalesce(link_FK8, 0)) %>% 
  mutate(link_FK9 = coalesce(link_FK9, 0)) %>% 
  mutate(link_FK10 = coalesce(link_FK10, 0)) %>% 
  mutate(link_FK11 = coalesce(link_FK11, 0)) %>% 
  mutate(link_FK12 = coalesce(link_FK12, 0)) %>% 
  mutate(link_FK13 = coalesce(link_FK13, 0)) %>% 
  
  mutate(link_FN1 = coalesce(link_FN1, 0)) %>% 
  mutate(link_FN2 = coalesce(link_FN2, 0)) %>% 
  mutate(link_FN3 = coalesce(link_FN3, 0)) %>% 
  mutate(link_FN4 = coalesce(link_FN4, 0)) %>% 
  mutate(link_FN5 = coalesce(link_FN5, 0)) %>% 
  mutate(link_FN6 = coalesce(link_FN6, 0)) %>% 
  mutate(link_FN7 = coalesce(link_FN7, 0)) %>% 
  mutate(link_FN8 = coalesce(link_FN8, 0)) %>% 
  mutate(link_FN9 = coalesce(link_FN9, 0)) %>% 
  mutate(link_FN10 = coalesce(link_FN10, 0)) %>% 
  mutate(link_FN11 = coalesce(link_FN11, 0)) %>% 
  mutate(link_FN12 = coalesce(link_FN12, 0)) %>% 
  mutate(link_FN13 = coalesce(link_FN13, 0)) %>% 
  mutate(link_FN14 = coalesce(link_FN14, 0)) %>% 
  mutate(link_FN15 = coalesce(link_FN15, 0)) %>% 
  mutate(link_FN16 = coalesce(link_FN16, 0)) %>% 
  
  mutate(false_clicked = link_FK2 + link_FK3 + link_FK4 + link_FK5 + link_FK6 + link_FK7 +
                       link_FK8 + link_FK9 + link_FK10 + link_FK11 + link_FK12 + link_FK13 +
                       link_FN1 + link_FN2 + link_FN3 + link_FN4 + link_FN5 + link_FN6 +
                       link_FN7 + link_FN8 + link_FN9 + link_FN10 + link_FN11 + link_FN12 +
                       link_FN13 + link_FN14 + link_FN15 + link_FN16) %>% 
  
  mutate(false_click_through_rate = coalesce(false_clicked / 4, 0))



## ----treatment_data---------------------------------------------------------------------------------------

cols <- c('Y', 'W', 'user_id', 'time_stamp', #not included in context variables
          
          # covariates
          'male', 
          'age', 'age_flag', 'age_check_flag',
          'ed', 'ed_flag', 
          'urban', 
          'rel_none', 'rel_christian', 'rel_muslim', 'rel_traditionalist', 'rel_other', 
          'denom_pentecostal', 
          'religiosity', 'religiosity_flag',
          'god',
          'locus', 'locus_flag',
          'science', 'science_flag',
          'dli',
          'fb_post', 'fb_post_flag',
          'fb_msg', 'fb_msg_flag',
          'crt',
          'hhi', 'hhi_flag',
          'cash',
          'hh', 'hh_flag',
          'pol',
          'cov_concern', 'cov_concern_flag',
          'cov_info',
          'cov_efficacy', 'cov_efficacy_flag',
          'strat_send_false0', 'strat_send_false1', 'strat_send_false2', 
          'strat_send_true0', 'strat_send_true1', 'strat_send_true2', 
          'strat_timeline_false0', 'strat_timeline_false1', 'strat_timeline_false2', 
          'strat_timeline_true0', 'strat_timeline_true1', 'strat_timeline_true2',
          'nigeria', 
          
          ### Response to pre-treatment stimuli
          'pre_false',
          'pre_true',
          
          ### Response to post-treatment stimuli
          'attrited',
          'post_true',
          'post_false',
          'post_true_any',
          'post_false_any',
          
          # Additional response measures
          'pre_true_any',
          'pre_false_any',
          'post_true_timeline',
          'post_true_send',
          'post_true_seen',
          'post_false_timeline',
          'post_false_send',
          'post_false_seen',
          'post_true_prop',
          'post_true_timeline_prop',
          'post_true_send_prop',
          'post_false_prop',
          'post_false_timeline_prop',
          'post_false_send_prop',
          'true_click_through_rate',
          'false_click_through_rate',
          'batch', 'optimal'
          )

context_cols <- cols[!cols %in% c('Y', 'W', 'user_id', 'time_stamp')]
transformed_cols <- c('post_false', 'post_true',
                      'dv_1', 'dv_2', 'dv_3', 'dv_4',
                      'dv_5', 'dv_6', 'dv_7', 'dv_8',
                      'post_true_any', 'post_false_any', 
                      'post_timeline_true',
                      'post_timeline_false', 'post_send_true', 'post_send_false',
                      'pre_true_any', 'pre_false_any', 
                      'post_true_timeline', 'post_true_send',
                      'post_false_timeline', 'post_false_send',
                      'post_true_seen', 'post_false_seen',
                      'post_true_prop', 'post_false_prop',
                      'post_true_timeline_prop', 'post_true_send_prop',
                      'post_false_timeline_prop', 'post_false_send_prop',
                      'dv_stimulus_pre1', 'dv_stimulus_pre2', 'dv_stimulus_pre3', 'dv_stimulus_pre4',
                      'dv_stimulus_post5', 'dv_stimulus_post6', 'dv_stimulus_post7', 'dv_stimulus_post8')

df_treat <- df %>% filter(!is.na(treatment_r)) %>% filter(!is.na(Y))
df_treat['W'] <- sapply(1:nrow(df_treat), 
                        function(i) paste0('H_', df_treat['treatment_h'][i, ], '_R_',
                                           df_treat['treatment_r'][i, ]))

W_levels <- c('H_control_R_control',
              'H_factcheck_R_control', 'H_more_info_R_control', 'H_real_info_R_control', 'H_related_R_control',
              'H_control_R_accuracy', 'H_control_R_deliberation', 'H_control_R_emotion', 'H_control_R_pledge', 'H_control_R_tips_africacheck', 'H_control_R_tips_facebook', 'H_control_R_video',
              'H_factcheck_R_accuracy', 'H_factcheck_R_deliberation', 'H_factcheck_R_emotion', 'H_factcheck_R_pledge', 'H_factcheck_R_tips_africacheck', 'H_factcheck_R_tips_facebook', 'H_factcheck_R_video',
              'H_more_info_R_accuracy', 'H_more_info_R_deliberation', 'H_more_info_R_emotion', 'H_more_info_R_pledge', 'H_more_info_R_tips_africacheck', 'H_more_info_R_tips_facebook', 'H_more_info_R_video',
              'H_real_info_R_accuracy', 'H_real_info_R_deliberation', 'H_real_info_R_emotion', 'H_real_info_R_pledge', 'H_real_info_R_tips_africacheck', 'H_real_info_R_tips_facebook', 'H_real_info_R_video',
              'H_related_R_accuracy', 'H_related_R_deliberation', 'H_related_R_emotion', 'H_related_R_pledge', 'H_related_R_tips_africacheck', 'H_related_R_tips_facebook', 'H_related_R_video')
df_treat$W <- factor(df_treat$W, levels = W_levels)
df_treat$W <- relevel(df_treat$W, ref = 'H_control_R_control')



## ----factor_levels----------------------------------------------------------------------------------------
W_levels <- levels(df_treat$W)
# Decompose treatment levels into separate factors
WH_levels <- unique(sub('_R_.*', '', W_levels))
WR_levels <- unique(sub('H_[a-z]*_*[a-z]*_*', '', W_levels))

# Identify treatment locations where each respective factor level is represented
WH_idx <- lapply(WH_levels, function(x) grep(x, W_levels))

df_treat['treatment_r'] <- relevel(as.factor(df_treat[['treatment_r']]), 
                                   ref = 'control')
df_treat['treatment_h'] <- relevel(as.factor(df_treat[['treatment_h']]), 
                                   ref = 'control')


## ----factor_scores----------------------------------------------------------------------------------------
predict_cols <- c('male', 
                  'age', 
                  'age_flag',
                  'age_check_flag',
                  'ed', 'ed_flag', 
                  'urban',
                  'rel_christian', 'rel_muslim',
                  'denom_pentecostal', 
                  'religiosity', 'religiosity_flag',
                  'locus', 'locus_flag',
                  'science', 'science_flag',
                  'dli',
                  'fb_post', 'fb_post_flag',
                  'fb_msg', 'fb_msg_flag',
                  'crt',
                  'hhi', 'hhi_flag',
                  'cash',
                  'hh', 'hh_flag',
                  'pol',
                  'cov_concern', 'cov_concern_flag',
                  'cov_efficacy', 'cov_efficacy_flag',
                  'nigeria', 
                  'strat_send_false0', 'strat_send_false1', 'strat_send_false2', 
                  'strat_send_true0', 'strat_send_true1', 'strat_send_true2', 
                  'strat_timeline_false0', 'strat_timeline_false1', 'strat_timeline_false2', 
                  'strat_timeline_true0', 'strat_timeline_true1', 'strat_timeline_true2')

xs <- as.matrix(df_treat[, predict_cols])


# Include headline as context
xs_h <- cbind(xs, 
              model.matrix(lm(Y~ treatment_h-1, data = df_treat)))
# Predict when headline == control
xs_h_new <- xs_h
treat_cols <- grepl(pattern = 'treatment', colnames(xs_h))
xs_h_new[, treat_cols] <- matrix(c(1, rep(0, sum(treat_cols) -1 )), 
                                 nrow = nrow(df_treat), 
                                 ncol = sum(treat_cols), byrow = TRUE)

h1 <- which(df_treat$batch <5)
h2 <- which(df_treat$batch == 5)



## ----predictions------------------------------------------------------------------------------------------

W_forest <- readRDS('objects/policy_objects/W_forest.rds')
multi_forest <- readRDS('objects/policy_objects/multi_forest.rds')
Y_forest <- readRDS('objects/policy_objects/Y_forest.rds')

# 2a. Predict m(x) = E[Y|X]
Y_hat_test <- predict(Y_forest, xs_h_new[h2,])$predictions[,1]
# 2b. Predict W.hat = E[W|X] 
W_hat_test <- predict(W_forest, xs_h_new[h2,])$predictions
# 2c. Predict baseline, E[Y(k) | X = x], and tau_k(X) = E[Y(k') - Y(k) | X = x]; 
# here accuracy is baseline, 
tau_hat_test <- predict(multi_forest, xs_h_new[h2,])$predictions[,,]
Y_hat_baseline_test <- Y_hat_test - rowSums(W_hat_test[, -1, drop = FALSE] * tau_hat_test)

# 3. combine to get mu_k(X) on test data (and re-order)
muk_test <- cbind(Y_hat_baseline_test, Y_hat_baseline_test + tau_hat_test)[,c(2, 1, 3:8)]
names(muk_test) <- WR_levels


levels_implement <- c(2,4,7,8)
wopt_multi_forest <- WH_idx[[1]][levels_implement[apply(
  muk_test[,levels_implement], 1, which.max
)]]



prev_nrow <- nrow(df_treat)

df_treat <- df_treat %>% 
  mutate(optimal = c(rep(1, length(h1)), wopt_multi_forest),
         opt_col = paste0('probs_', optimal-1)) %>% 
  rowwise() %>% 
  mutate(optimal_probs = get(opt_col)) %>% 
  ungroup() %>% 
  mutate(optimal = case_when(batch !=5 ~ NA_real_,
                             TRUE ~ optimal)) %>% 
  filter(optimal_probs!=0) %>% # probability of optimal assignment is 0
  filter( (batch != 5) | ((probs_0 %in% c(0.025, 0.1666666667))) ) %>% # probabilities out of range
  filter(Y >= -4 & Y <= 2) %>% 
  filter(post_true >= 0 & post_true <= 4) %>%
  filter(post_false >= 0 & post_false <= 4) %>% 
  filter(pre_send_true >= 0 & pre_send_true <= 2) %>% 
  filter(pre_send_false >= 0 & pre_send_false <= 2) %>% 
  filter(post_timeline_true >= 0 & post_timeline_true <= 2) %>% 
  filter(post_timeline_false >= 0 & post_timeline_false <= 2) %>% 
  filter(seen_false < 6 & seen_true < 6) %>% 
  mutate(ID = as.numeric(factor(ID)))

after_nrow <- nrow(df_treat)

paste0('There are ', prev_nrow - after_nrow, ' infeasible observations.')
paste0('Percentage of infeasible observations is ', round((prev_nrow - after_nrow)/prev_nrow, 4), '%.')


## ----desc-------------------------------------------------------------------------------------------------
attr(df_treat$male, 'shortDescription') <- "Pre-treatment covariate: 'What is your gender?'; binary coded as 1 if male, 0 for any other response."
attr(df_treat$age, 'shortDescription') <- "Pre-treatment covariate: 'What is your age?'; continuous variable."
attr(df_treat$age_check, 'shortDescription') <- "Pre-treatment covariate: binary coded as 1 if age >= 120 (i.e. abnormal age), 0 for any other age between 18 and 120."
attr(df_treat$age_check_flag, 'shortDescription') <- "Pre-treatment covariate: binary coded as 1 if the age check question later in the survey is different from the age the respondent entered initially, 0 otherwise."
attr(df_treat$ed, "shortDescription") <- "Pre-treatment covariate: 'What is your highest level of education?';  integer coded between 1 and 10, where 1 represents no formal schooling and 10 represents post-graduate; levels: No formal schooling, Informal schooling only, Some primary school, Primary school completed, Some secondary school, Secondary school completed, Post-secondary qualifications, Some university, University completed, Post-graduate"
attr(df_treat$ed_flag, 'shortDescription') <- "Pre-treatment covariate: binary coded as 1 if respondent's education is missing, 0 otherwise."
attr(df_treat$urban, 'shortDescription') <- "Pre-treatment covariate: 'Is your home area mostly urban or mostly rural?'; binary coded as 1 if mostly urban, 0 for mostly rural."

attr(df_treat$rel_none, "shortDescription") <- "Pre-treatment covariate: 'What is your religion, if any?'; binary coded as 1 if religion is none, 0 for any other response."
attr(df_treat$rel_christian, "shortDescription") <- "Pre-treatment covariate: 'What is your religion, if any?'; binary coded as 1 if religion is Christian, 0 for any other response."
attr(df_treat$rel_muslim, "shortDescription") <- "Pre-treatment covariate: 'What is your religion, if any?'; binary coded as 1 if religion is Muslim, 0 for any other response."
attr(df_treat$rel_traditionalist, "shortDescription") <- "Pre-treatment covariate: 'What is your religion, if any?'; binary coded as 1 if religion is Traditionalist, 0 for any other response."
attr(df_treat$rel_other, "shortDescription") <- "Pre-treatment covariate: 'What is your religion, if any?'; binary coded as 1 if religion is not none/christian/muslim/traditionalist, 0 for any other response."
attr(df_treat$denom_pentecostal, "shortDescription") <- "Pre-treatment covariate: 'What church or denomination do you identify with?' if respondent's religion is Christian; binary coded as 1 if denomination is Pentecostal, 0 for any other response."
attr(df_treat$religiosity, "shortDescription") <- "Pre-treatment covariate: 'Before Coronavirus came to your country, how often did you usually attend religious services?';  integer coded between 1 and 6 (with missing value coded as 0), where 1 represents never and 6 represents daily: never, < once a month, 1-3 times a month, once a week, > once a week, daily." 
attr(df_treat$religiosity_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent's religiosity is missing, 0 otherwise." 
attr(df_treat$god, "shortDescription") <- "Pre-treatment covariate: 'Which of these two statements comes closer to your own views:
1. God will grant wealth and good health to all believers who have enough faith 2. God doesn't always give wealth and good health even to believers who have deep faith'; binary coded as 1 if respondent chooses option 1, 0 for option 2 and non-religious respondent."
attr(df_treat$locus, "shortDescription") <- "Pre-treatment covariate:  integer coded between 1 and 10,  where 1 means 'no choice at all' and 10 means 'a great deal of choice' to indicate how much freedom of choice and control you feel you have over the way your life turns out."
attr(df_treat$locus_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent's locus of control is missing, 0 otherwise."
attr(df_treat$science, "shortDescription") <- "Pre-treatment covariate: asking respondent's view on whether human has evolved over time and whether global warming exists; integer coded between 0 and 2, respondent gain 1 point for each correct answer."
attr(df_treat$science_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent's science variable is missing, 0 otherwise."
attr(df_treat$dli, "shortDescription") <- "Pre-treatment covariate: measuring respondent's digital literacy based on method proposed by Guess et al. (2020);  integer coded between 0 and 24, where 0 represents lowest digital literacy and 24 represents highest digital literacy."

attr(df_treat$fb_post, "shortDescription") <- "Pre-treatment covariate: 'How many times in the past 7 days did you post on your Facebook timeline?'; integer coded between 0 and 3, where 1 represents '1-2 times', 2 represents '3-4 times', 3 represents '5+ times', and 0 otherwise."
attr(df_treat$fb_post_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent's facebook post variable (fb_post) is missing, 0 otherwise."
attr(df_treat$fb_msg, "shortDescription") <- "Pre-treatment covariate: 'How many times in the past 7 days did you message a friend on Facebook?'; integer coded between 0 and 3, where 1 represents '1-2 times', 2 represents '3-4 times', 3 represents '5+ times', and 0 otherwise."
attr(df_treat$fb_msg_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent's facebook message variable (fb_msg) is missing, 0 otherwise."
attr(df_treat$crt, "shortDescription") <- "Pre-treatment covariate: cognitive reflective test, respondent gets three math questions proposed by Frederick (2005) to measure cognitive ability (or 'IQ') and gets one point for each correct answer; this variables integer coded between 0 and 3."
attr(df_treat$hhi, "shortDescription") <- "Pre-treatment covariate: 'Which of the following things do you or anyone in your household own?' Radio/TV/motorcycle/computer/bank account/mobile phone/bicycle; records the sum of all owned items, integer coded between 0 and 6."
attr(df_treat$hhi_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if all of the responses to the household index questions (hhi) are missing, 0 otherwise."
attr(df_treat$cash, "shortDescription") <- "Pre-treatment covariate: 'Do you have a job that pays a cash income?'; binary coded as 1 if yes, 0 otherwise."
attr(df_treat$hh, "shortDescription") <- "Pre-treatment covariate: 'How many people live in your household, including yourself?'; continuous variable (integer), coded as 0 if missing value."
attr(df_treat$hh_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as if number of people in household (hh) is missing, 0 otherwise."
attr(df_treat$pol, "shortDescription") <- "Pre-treatmennt covariate: 'Do you feel close to any political party? If so, which party is that?'; binary coded as 1 if associate with or voted for candidate from governing party, 0 otherwise."
attr(df_treat$cov_concern, "shortDescription") <- "Pre-treatment covariate: 'How concerned are you that you or someone close to you will get sick from the Coronavirus?'; integer coded between 0 and 3, where 1 represents 'not at all worried', 2 represents 'somewhat worried', 3 represents 'very worried', and 0 if value is missing."
attr(df_treat$cov_concern_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if covid concern (cov_concern) variable is misisng, 0 otherwise."
attr(df_treat$cov_info, "shortDescription") <- "Pre-treatment covariate: three statements on COVID-19 information, 'Coronavirus spreads through droplets in the air', 'Catching Coronavirus and having COVID-19 does not mean you will have it for life', and 'COVID-19 is real'; integer coded between 0 and 3, respondent gets 1 point for each correct answer."
attr(df_treat$cov_efficacy, "shortDescription") <- "Pre-treatment covariate: 'How well or poorly do you think your national government is managing the Coronavirus pandemic?'; integer coded between 0 and 4, where 1 presents 'very poorly', 2 represents 'somewhat poorly', 3 represents 'somewhat well', 4 represents 'very well', and 0 if value is missing."
attr(df_treat$cov_efficacy_flag, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if covid efficacy variable (cov_efficacy) is missing, 0 otherwise."
attr(df_treat$nigeria, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent comes form Nigeria, 0 if respondent comes from Kenya."
attr(df_treat$attrited, "shortDescription") <- "Post-treatment covariate: binary coded as 1 if respondent attrites before reaching the final post-treatment stimulus, 0 otherwise."

attr(df_treat$strat_send_false0, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 0 out of 2 pre-treatment false stimuli on messenger, 0 otherwise."
attr(df_treat$strat_send_false1, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 1 out of 2 pre-treatment false stimuli on messenger, 0 otherwise."
attr(df_treat$strat_send_false2, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 2 out of 2 pre-treatment false stimuli on messenger, 0 otherwise."
attr(df_treat$strat_send_true0, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 0 out of 2 pre-treatment true stimuli on messenger, 0 otherwise."
attr(df_treat$strat_send_true1, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 1 out of 2 pre-treatment true stimuli on messenger, 0 otherwise."
attr(df_treat$strat_send_true2, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 2 out of 2 pre-treatment true stimuli on messenger, 0 otherwise."

attr(df_treat$strat_timeline_false0, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 0 out of 2 pre-treatment false stimuli on Facebook timeline, 0 otherwise."
attr(df_treat$strat_timeline_false1, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 1 out of 2 pre-treatment false stimuli on Facebook timeline, 0 otherwise."
attr(df_treat$strat_timeline_false2, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 2 out of 2 pre-treatment false stimuli on Facebook timeline, 0 otherwise."
attr(df_treat$strat_timeline_true0, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 0 out of 2 pre-treatment true stimuli on Facebook timeline, 0 otherwise."
attr(df_treat$strat_timeline_true1, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 1 out of 2 pre-treatment true stimuli on Facebook timeline, 0 otherwise."
attr(df_treat$strat_timeline_true2, "shortDescription") <- "Pre-treatment covariate: binary coded as 1 if respondent intents to share 2 out of 2 pre-treatment true stimuli on Facebook timeline, 0 otherwise."


attr(df_treat$pre_false, "shortDescription") <- "Pre-treatment false stimuli response count: record how many times the respondent wants to share any of the pre-treatment false stimuli; integer coded between 0 and 4; note that for each stimulus, the respondent can share at most twice (once on timeline, once on messenger); there are two true stimuli and two false stimuli pre-treatment."
attr(df_treat$pre_true, "shortDescription") <- "Pre-treatment true stimuli response count: record how many times the respondent wants to share any of the pre-treatment true stimuli; integer coded between 0 and 4; note that for each stimulus, the respondent can share at most twice (once on timeline, once on messenger); there are two true stimuli and two false stimuli pre-treatment."


attr(df_treat$stimf1, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 1 country specific (nigeria); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf2, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 2 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf3, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 3 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf4, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 4 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf5, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 5 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf6, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 6 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf7, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 7 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf8, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 8 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf9, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 9 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf10, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 10 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf11, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 11 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf12, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 12 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf13, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 13 country specific (nigeria or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf14, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 14 country specific (nigeria); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf15, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 15 country specific (nigeria); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimf16, "shortDescription") <- "Pre-treatment indicator for individual stimulus: false stimulus 16 country specific (nigeria); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."

attr(df_treat$stimt1, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 1 country specific (nigera or kenya); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimt2, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 2 country specific (nigeria or kenya); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimt3, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 3 country specific (nigeria or kenya); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimt4, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 4 country specific (nigeria or kenya); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimt5, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 5 country specific (nigeria or kenya); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimt6, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 6 country specific (nigeria or kenya); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."

attr(df_treat$stimb1, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 1 both country (source: WHO); binary coded as 1 if respondent received this stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimb2, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 2 both country (source: WHO); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimb3, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 3 both country (source: WHO); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimb4, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 4 both country (source: WHO); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."
attr(df_treat$stimb5, "shortDescription") <- "Pre-treatment indicator for individual stimulus: true stimulus 5 both country (source: WHO); binary coded as 1 if respondent receivedthis stimuli in one of the four pre-treatment stimuli, 0 otherwise."

# newly added
attr(df_treat$Y, 'shortDescription') <- "Response; response function: Y = - post_false + 0.5 * post_true."

attr(df_treat$W, 'shortDescription') <- "Treatment each subject receives. There are 40 unqiue levels."

attr(df_treat$age_flag, 'shortDescription') <- "Pre-treatment covariate: binary coded as 1 if age >= 120 (i.e. abnormal age) or empty, 0 otherwise."

attr(df_treat$batch, 'shortDescription') <- "The batch in the linear Thompson sampling algorithm; integer ranging from 1 to 5."

attr(df_treat$optimal, 'shortDescription') <- ""

attr(df_treat$post_true, "shortDescription") <- "Post-treatment true stimuli response count: record how many times the respondent wants to share any of the post-treatment true stimuli; integer coded between 0 and 4; note that for each stimulus, the respondent can share at most twice (once on timeline, once on messenger); there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_false, "shortDescription") <- "Post-treatment false stimuli response count: record how many times the respondent wants to share any of the post-treatment false stimuli; integer coded between 0 and 4; note that for each stimulus, the respondent can share at most twice (once on timeline, once on messenger); there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_true_any, "shortDescription") <- "Post-treatment true stimuli response count (# stimuli): record how many post-treatment true stimuli the respondent wants to share; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_false_any, "shortDescription") <- "Post-treatment false stimuli response count (# stimuli): record how many post-treatment false stimuli the respondent wants to share; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$pre_true_any, "shortDescription") <- "Pre-treatment true stimuli response count (# stimuli): record how many pre-treatment true stimuli the respondent wants to share; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli pre-treatment."

attr(df_treat$pre_false_any, "shortDescription") <- "Pre-treatment false stimuli response count (# stimuli): record how many pre-treatment false stimuli the respondent wants to share; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli pre-treatment."

attr(df_treat$post_true_timeline, "shortDescription") <- "Post-treatment true stimuli response count (# timeline): record how many post-treatment true stimuli the respondent wants to share in facebook timeline; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_true_send, "shortDescription") <- "Post-treatment true stimuli response count (# send): record how many post-treatment true stimuli the respondent wants to share via facebook message; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_true_seen, "shortDescription") <- "Post-treatment true stimuli seen (count): record how many post-treatment true stimuli the respondent sees; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_false_timeline, "shortDescription") <- "Post-treatment false stimuli response count (# timeline): record how many post-treatment false stimuli the respondent wants to share in facebook timeline; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_false_send, "shortDescription") <- "Post-treatment false stimuli response count (# send): record how many post-treatment false stimuli the respondent wants to share via facebook message; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_false_seen, "shortDescription") <- "Post-treatment false stimuli seen (count): record how many post-treatment false stimuli the respondent sees; integer coded between 0 and 2; note that there are two true stimuli and two false stimuli post-treatment."

attr(df_treat$post_true_prop, "shortDescription") <- "Post-treatment true stimuli response proportion: record the proportion of post-treatment true stimuli the respondent wants to share among all the post-treatment true stimuli they've seen (post_true_any/post_true_seen); float between 0 and 1."

attr(df_treat$post_true_timeline_prop, "shortDescription") <- "Post-treatment true stimuli response proportion (timeline): record the proportion of post-treatment true stimuli the respondent wants to share in facebook timeline among all the post-treatment true stimuli they've seen (post_true_timeline/post_true_seen); float between 0 and 1."

attr(df_treat$post_true_send_prop, "shortDescription") <- "Post-treatment true stimuli response proportion (message): record the proportion of post-treatment true stimuli the respondent wants to share via facebook message among all the post-treatment true stimuli they've seen (post_true_timeline/post_true_seen); float between 0 and 1."

attr(df_treat$post_false_prop, "shortDescription") <- "Post-treatment false stimuli response proportion: record the proportion of post-treatment false stimuli the respondent wants to share among all the post-treatment false stimuli they've seen (post_false_any/post_false_seen); float between 0 and 1."

attr(df_treat$post_false_timeline_prop, "shortDescription") <- "Post-treatment false stimuli response proportion (timeline): record the proportion of post-treatment false stimuli the respondent wants to share in facebook timeline among all the post-treatment false stimuli they've seen (post_false_timeline/post_false_seen); float between 0 and 1."

attr(df_treat$post_false_send_prop, "shortDescription") <- "Post-treatment false stimuli response proportion (message): record the proportion of post-treatment false stimuli the respondent wants to share via facebook message among all the post-treatment false stimuli they've seen (post_true_timeline/post_true_seen); float between 0 and 1."

attr(df_treat$true_click_through_rate, "shortDescription") <- "Click through rate of the sharing button for true stimuli: record the proportion of the true stimuli that the subject actually shares among the true stimuli that the subject wanted to share; float between 0 and 1."

attr(df_treat$false_click_through_rate, "shortDescription") <- "Click through rate of the sharing button for false stimuli: record the proportion of the links that correct the misinformation in the false stimuli that the subject shares among all the false stimuli that the subject saw (i.e. the denominator is 4); float between 0 and 1."



## ----attrition--------------------------------------------------------------------------------------------
df_treat <- df_treat %>% 
  mutate(last_response = case_when(!(is.na(dv_send_post8) & is.na(dv_timeline_post8)) ~ 8,
                                   !(is.na(dv_send_post7) & is.na(dv_timeline_post7)) ~ 7,
                                   !(is.na(dv_send_post6) & is.na(dv_timeline_post6)) ~ 6,
                                   !(is.na(dv_send_post5) & is.na(dv_timeline_post5)) ~ 5,
                                   !(is.na(dv_send_pre4) & is.na(dv_timeline_pre4)) ~ 4,
                                   !(is.na(dv_send_pre3) & is.na(dv_timeline_pre3)) ~ 3,
                                   !(is.na(dv_send_pre2) & is.na(dv_timeline_pre2)) ~ 2,
                                   !(is.na(dv_send_pre1) & is.na(dv_timeline_pre1)) ~ 1,
                                   TRUE ~ 0))

prop.table(table(df_treat$last_response))




## ----secondary, warning=FALSE, results='hide',message=FALSE-----------------------------------------------

## NEWS COV FALSE
df$news_cov_false[df$news_cov_false == '1'] <-
  'somewhat agree w/1'
df_treat$news_cov_false[df_treat$news_cov_false == '2'] <-
  'somewhat agree w/2'
df_treat$news_cov_false[!df_treat$news_cov_false %in% c(
  'somewhat agree w/1',
  'somewhat agree w/2',
  'strongly agree w/1',
  'strongly agree w/2',
  'skip',
  NA
)] <- 'other'

# mask_est ##
df_treat$mask_est[df_treat$mask_est > 11] <- NA

# covid vaccine ##
df_treat$cov_vaccine[!df_treat$cov_vaccine %in% c(
  'somewhat unwilling',
  'somewhat willing',
  'very unwilling',
  'very willing',
  'neither unwilling nor willing'
)] <- 'skip'

# age check ##
df_treat$age_check <- as.integer(df_treat$age_check)

## source of information

# 1: Newspapers
# 2: Radio
# 3: TV
# 4: Internet news sites
# 5: Government website
# 6: Facebook
# 7: Twitter
# 8: WhatsApp
# 9: Friends and family in person
# 10: Religious leaders
# 11: Government leaders
# 12: Other
# 0: Skip / Prefer not to answer

df_treat$main_source[!df_treat$main_source %in% c(0:12, NA)] <- 'other'
df_treat$main_source[df_treat$main_source == 0] <- 'skip'
df_treat$main_source[df_treat$main_source == 1] <- 'news'
df_treat$main_source[df_treat$main_source == 2] <- 'radio'
df_treat$main_source[df_treat$main_source == 3] <- 'tv'
df_treat$main_source[df_treat$main_source == 4] <- 'internet'
df_treat$main_source[df_treat$main_source == 5] <- 'gov'
df_treat$main_source[df_treat$main_source == 6] <- 'fb'
df_treat$main_source[df_treat$main_source == 7] <- 'twitter'
df_treat$main_source[df_treat$main_source == 8] <- 'whatsapp'
df_treat$main_source[df_treat$main_source == 9] <- 'friends&fam'
df_treat$main_source[df_treat$main_source == 10] <- 'rel_lead'
df_treat$main_source[df_treat$main_source == 11] <- 'gov_lead'
df_treat$main_source[df_treat$main_source == 12] <- 'other'


df_treat$source_2[!df_treat$source_2 %in% c(0:12, NA)] <- 'other'
df_treat$source_2[df_treat$source_2 == 0] <- 'skip'
df_treat$source_2[df_treat$source_2 == 1] <- 'news'
df_treat$source_2[df_treat$source_2 == 2] <- 'radio'
df_treat$source_2[df_treat$source_2 == 3] <- 'tv'
df_treat$source_2[df_treat$source_2 == 4] <- 'internet'
df_treat$source_2[df_treat$source_2 == 5] <- 'gov'
df_treat$source_2[df_treat$source_2 == 6] <- 'fb'
df_treat$source_2[df_treat$source_2 == 7] <- 'twitter'
df_treat$source_2[df_treat$source_2 == 8] <- 'whatsapp'
df_treat$source_2[df_treat$source_2 == 9] <- 'friends&fam'
df_treat$source_2[df_treat$source_2 == 10] <- 'rel_lead'
df_treat$source_2[df_treat$source_2 == 11] <- 'gov_lead'
df_treat$source_2[df_treat$source_2 == 12] <- 'other'



df_treat$source_3[!df_treat$source_3 %in% c(0:12, NA)] <- 'other'
df_treat$source_3[df_treat$source_3 == 0] <- 'skip'
df_treat$source_3[df_treat$source_3 == 1] <- 'news'
df_treat$source_3[df_treat$source_3 == 2] <- 'radio'
df_treat$source_3[df_treat$source_3 == 3] <- 'tv'
df_treat$source_3[df_treat$source_3 == 4] <- 'internet'
df_treat$source_3[df_treat$source_3 == 5] <- 'gov'
df_treat$source_3[df_treat$source_3 == 6] <- 'fb'
df_treat$source_3[df_treat$source_3 == 7] <- 'twitter'
df_treat$source_3[df_treat$source_3 == 8] <- 'whatsapp'
df_treat$source_3[df_treat$source_3 == 9] <- 'friends&fam'
df_treat$source_3[df_treat$source_3 == 10] <- 'rel_lead'
df_treat$source_3[df_treat$source_3 == 11] <- 'gov_lead'
df_treat$source_3[df_treat$source_3 == 12] <- 'other'


# news_fb_freq ##
freq_lst <- c('never', '< once a month', 'a few times a month', 'a few times a week', 'every day', 'skip', NA)
df_treat$news_fb_freq[!df_treat$news_fb_freq %in% freq_lst] <- 'other'

# news_whatsapp_freq ##
df_treat$news_whatsapp_freq[!df_treat$news_whatsapp_freq %in% freq_lst] <- 'other'

# news_fb_gen ##
df_treat$news_fb_gen[!df_treat$news_fb_gen %in% freq_lst] <- 'other'

# news_fb_gen_trust ##
trust_lst <- c('not at all trust', 'trust a little', 'somewhat trust', 'very much trust', 'skip', NA)
df_treat$news_fb_gen_trust[!df_treat$news_fb_gen_trust %in% trust_lst] <- 'other'

# dv_post_instruction ##
df_treat$dv_post_instructions[! df_treat$dv_post_instructions %in% c('yes', 'no', 'skip')] <- 'other'

# news_whatsapp_gen ##
df_treat$news_whatsapp_gen[!df_treat$news_whatsapp_gen %in% freq_lst] <- 'other'

# news_fb_gen_trust ##
df_treat$news_whatsapp_gen_trust[!df_treat$news_whatsapp_gen_trust %in% trust_lst] <- 'other'

# dv_send_post5 ##
send_lst <- c('yes', 'no', 'skip', NA)
df_treat$dv_send_post5[df_treat$dv_send_post5 == 0] <- 'skip'
df_treat$dv_send_post5[!df_treat$dv_send_post5 %in% send_lst] <- 'other'

# continue_dummy ##
df_treat$continue_dummy[df_treat$continue_dummy == 'yes'] <- 'continue'
df_treat$continue_dummy[!df_treat$continue_dummy %in% c('continue', NA)] <- 'other'

# dv_send_post6 ##
df_treat$dv_send_post6[df_treat$dv_send_post6 == 0] <- 'skip'
df_treat$dv_send_post6[!df_treat$dv_send_post6 %in% send_lst] <- 'other'

# dv_send_post7 ##
df_treat$dv_send_post7[df_treat$dv_send_post7 == 0] <- 'skip'
df_treat$dv_send_post7[!df_treat$dv_send_post7 %in% send_lst] <- 'other'

# dv_send_post8 ##
df_treat$dv_send_post8[df_treat$dv_send_post8 == 0] <- 'skip'
df_treat$dv_send_post8[!df_treat$dv_send_post8 %in% send_lst] <- 'other'

# cv_food ##
supply_lst <- c('never', 'just once or twice', 'several times', 'many times', 'always', 'skip', NA)
df_treat$cv_food[!df_treat$cv_food %in% supply_lst] <- 'other'

# cv_food ##
df_treat$cv_water[!df_treat$cv_water %in% supply_lst] <- 'other'

# stimulus_seen ##
seen_lst <- c('yes', 'no', 'skip', NA)
df_treat$stimulus1_seen[!df_treat$stimulus1_seen %in% seen_lst] <- 'other'
df_treat$stimulus2_seen[!df_treat$stimulus2_seen %in% seen_lst] <- 'other'
df_treat$stimulus3_seen[!df_treat$stimulus3_seen %in% seen_lst] <- 'other'
df_treat$stimulus4_seen[!df_treat$stimulus4_seen %in% seen_lst] <- 'other'
df_treat$stimulus5_seen[!df_treat$stimulus5_seen %in% seen_lst] <- 'other'
df_treat$stimulus6_seen[!df_treat$stimulus6_seen %in% seen_lst] <- 'other'
df_treat$stimulus7_seen[!df_treat$stimulus7_seen %in% seen_lst] <- 'other'
df_treat$stimulus8_seen[!df_treat$stimulus8_seen %in% seen_lst] <- 'other'

# news_tv_cov ##
highfreq_lst <- c('never', '< once a month', 'a few times a month', 'a few times a week', 'once a day', 'multiple times a day', 'skip', NA)
df_treat$news_tv_cov[!df_treat$news_tv_cov %in% highfreq_lst] <- 'other'

# news_tv_trust ##
df_treat$news_tv_trust[!df_treat$news_tv_trust %in% trust_lst] <- 'other'

# news_fb_cov ##
df_treat$news_fb_cov[!df_treat$news_fb_cov %in% highfreq_lst] <- 'other'

# news_fb_trust ##
df_treat$news_fb_trust[!df_treat$news_fb_trust %in% trust_lst] <- 'other'

# news_radio_cov ##
df_treat$news_radio_cov[!df_treat$news_radio_cov %in% highfreq_lst] <- 'other'

# news_radio_trust ##
df_treat$news_radio_trust[!df_treat$news_radio_trust %in% trust_lst] <- 'other'

# news_web_cov ##
df_treat$news_web_cov[!df_treat$news_web_cov %in% highfreq_lst] <- 'other'

# news_web_trust ##
df_treat$news_web_trust[!df_treat$news_web_trust %in% trust_lst] <- 'other'

# news_whatsapp_cov ##
df_treat$news_whatsapp_cov[!df_treat$news_whatsapp_cov %in% highfreq_lst] <- 'other'

# news_whatsapp_trust ##
df_treat$news_whatsapp_trust[!df_treat$news_whatsapp_trust %in% trust_lst] <- 'other'

# news_fam_cov ##
df_treat$news_fam_cov[!df_treat$news_fam_cov %in% highfreq_lst] <- 'other'

# news_fam_trust ##
df_treat$news_fam_trust[!df_treat$news_fam_trust %in% trust_lst] <- 'other'

# accuracy ##
df_treat$accuracy[!df_treat$accuracy %in% c('yes', 'no', 'skip', NA)] <- 'other'

# news_newspapers_cov ##
df_treat$news_newspapers_cov[!df_treat$news_newspapers_cov %in% highfreq_lst] <- 'other'

# news_newspapers_trust ##
df_treat$news_newspapers_trust[!df_treat$news_newspapers_trust %in% trust_lst] <- 'other'

# delibration_send ##
df_treat$deliberation_send[!df_treat$deliberation_send %in% c('yes', 'no', 'skip', NA)] <- 'other'

# delibration_timeline ##
df_treat$deliberation_timeline[!df_treat$deliberation_timeline %in% c('yes', 'no', 'skip', NA)] <- 'other'

# news_govweb_cov ##
df_treat$news_govweb_cov[!df_treat$news_govweb_cov %in% highfreq_lst] <- 'other'

# news_govweb_trust ##
df_treat$news_govweb_trust[!df_treat$news_govweb_trust %in% trust_lst] <- 'other'

# news_twitter_cov ##
df_treat$news_twitter_cov[!df_treat$news_twitter_cov %in% highfreq_lst] <- 'other'

# news_twitter_trust ##
df_treat$news_twitter_trust[!df_treat$news_twitter_trust %in% trust_lst] <- 'other'

# news_govt_leader_cov ##
df_treat$news_govt_leader_cov[!df_treat$news_govt_leader_cov %in% highfreq_lst] <- 'other'

# news_govt_leader_trust ##
df_treat$news_govt_leader_trust[!df_treat$news_govt_leader_trust %in% trust_lst] <- 'other'

# news_religious_cov ##
df_treat$news_religious_cov[!df_treat$news_religious_cov %in% highfreq_lst] <- 'other'

# news_religious_trust ##
df_treat$news_religious_trust[!df_treat$news_religious_trust %in% trust_lst] <- 'other'

# news_other_cov ##
df_treat$news_other_cov[!df_treat$news_other_cov %in% highfreq_lst] <- 'other'

# news_other_trust ##
df_treat$news_other_trust[!df_treat$news_other_trust %in% trust_lst] <- 'other'


## ----save-------------------------------------------------------------------------------------------------
text_vars <- c('region', 'cv_denom', 'cv_ethnic', 
               'questions_comments', 'cov_vaccine_no', 'open_input1', 'deliberation', 'open_input2', 
               'reason_true', 'reason_false', 'open_input3','cov_masks_no_other', 'input4', 
               'pledge_3_no', 'pledge_1_no', 'open_input', 'cv_religion_other')

NA_cols <- colnames(df_treat)[sapply(df_treat, function(x)all(is.na(x)))]
user_cols <- colnames(df_treat)[which(colnames(df_treat) == 'last.seen'):ncol(df_treat)]

secondary_vars_interest <- colnames(df_treat)[!colnames(df_treat) %in% c(text_vars, NA_cols, user_cols, context_cols, transformed_cols)]


## Cleaned data
CLEANED_FILENAME <- paste0('cleaned-data_', 
                           Sys.Date())

write_csv(cbind(df_treat[, c('ID', 'Y', 'W', context_cols, paste0('probs_', 0:39), transformed_cols)], df_treat[,secondary_vars_interest]), 
          paste0('../data/', CLEANED_FILENAME,'.csv'))

write_rds(cbind(df_treat[, c('ID', 'Y', 'W', context_cols, paste0('probs_', 0:39), transformed_cols)], df_treat[,secondary_vars_interest]), 
          paste0('../data/', CLEANED_FILENAME,'.rds'))



## ----codebook, cache = TRUE-------------------------------------------------------------------------------
# Codebook: only need to do this once per cleaned dataset
makeCodebook(df_treat[, c('Y', 'W', context_cols)], replace = TRUE, 
             reportTitle = 'Cleaned Data Codebook',
             file = '../data/codebook_df_treat_cleaned_main_evaluation.Rmd')


## ----check_packages, eval = FALSE-------------------------------------------------------------------------
## library(NCmisc)
## knitr::purl('dataCleaning_evaluation.Rmd')
## list.functions.in.file('dataCleaning_evaluation.R')

