### Banker data manipulation
library(readr)
library(readxl)
library(dplyr)
library(devtools)
library(tidyverse)

#setwd("~/Desktop/MA Thesis/Treatment data")
d <- read_csv("data-raw/TimeSpent (accessed 2017-02-14) (1).csv")
df <- read_csv("data-raw/ultimatum_banker (accessed 2017-02-14) (1).csv")
surv <- read_csv("data-raw/Post-experiment+questionnaire+-+Treatment_February+14%2C+2017_05.05 (1).csv")

#setwd("~/Desktop/MA Thesis/Banker data 2/banker 3")
e <- read_csv("data-raw/TimeSpent (accessed 2017-02-23) (1).csv")
ef <- read_csv("data-raw/ultimatum_banker (accessed 2017-02-23) (5).csv")
surv2 <- read_csv("data-raw/latest banker survey.csv")

df <- df %>%
  rename(userid = participant.code)
#d <- d %>%
# rename(userid = participant__code)

b <- left_join(df, surv, by="userid")
b <- b[,-c(49,80)]
b <- b %>%
  dplyr::filter(participant.mturk_worker_id != "") %>%
  rename(
    participant_id = participant.id_in_session,
    #code = participant.code,
    label = participant.label,
    isbot = participant._is_bot,
    pagenumber = participant._index_in_pages,
    maxpage = participant._max_page_index,
    app = participant._current_app_name,
    round = participant._round_number,
    pagename = participant._current_page_name,
    ip = participant.ip_address,
    time = participant.time_started,
    exclude = participant.exclude_from_data_analysis,
    visited = participant.visited,
    #mturk.id = participant.mturk_worker_id,
    mturk.assignid = participant.mturk_assignment_id,
    playerid = player.id_in_group,
    validcode = player.valid_code,
    payoff = player.payoff,
    subsessionid = group.id_in_subsession,
    offer = group.amount_offered,
    accept = group.offer_accepted,
    subsessionround = subsession.round_number
  ) %>%
  dplyr::filter(validcode != "")

reps <- c('A19F75QCU199K8', 'A1D9FOXZ0UT0L2', 'A2AAY4VT9L71SY','A2AUDQQQALH4Q8',
          'A2RBNMRC6942T0','A2SBISQJE2ZPJQ','A2UMB419R9QW8L','A317RA15E0SZ4P',
          'A33KTU8KJ5CP2T','A3SOJWB6AZWZV9','A3VA5UDZ327VT0','A3VANXT8EEKXQA',
          'A8P66AMRDFLLT','AAGZ40ERLMIPB', 'AELFDOPFPTHT0', 'AHDW0M4017Z8K',
          'AK467M5VTG7N4', 'AKHVXXXX53S5G')
b.reps <- subset(b, participant.mturk_worker_id %in% reps )
b <- subset(b, !(participant.mturk_worker_id %in% reps))
b.reps <- rbind(b.reps,
                subset(b, subsessionid %in% b.reps$subsessionid))
b <- subset(b, !(subsessionid %in% b.reps$subsessionid))
b.reps

## "anomalous data"
#anom <- subset(a, a$offer > 20)

ef <- ef %>%
  rename(userid = participant.code)
ef$group.amount_offered <- 100 - ef$group.amount_offered

bb <- left_join(ef, surv2, by="userid")
bb <- bb[,-c(49,82)]
bb <- bb %>%
  dplyr::filter(participant.mturk_worker_id != "") %>%
  rename(
    participant_id = participant.id_in_session,
    #code = participant.code,
    label = participant.label,
    isbot = participant._is_bot,
    pagenumber = participant._index_in_pages,
    maxpage = participant._max_page_index,
    app = participant._current_app_name,
    round = participant._round_number,
    pagename = participant._current_page_name,
    ip = participant.ip_address,
    time = participant.time_started,
    exclude = participant.exclude_from_data_analysis,
    visited = participant.visited,
    #mturk.id = participant.mturk_worker_id,
    mturk.assignid = participant.mturk_assignment_id,
    playerid = player.id_in_group,
    validcode = player.valid_code,
    payoff = player.payoff,
    subsessionid = group.id_in_subsession,
    offer = group.amount_offered,
    accept = group.offer_accepted,
    subsessionround = subsession.round_number
  ) %>%
  dplyr::filter(validcode != "")

b <- b %>%
  rename(
    attcheck1 = Q39,
    bankfee = Q42,
    brokerfee = Q44,
    servicecharge = Q46,
    ATMfee = Q48,
    age = Q1_1,
    occupation = Q1_2,
    nationality = Q1_3,
    gender = Q30,
    role = Q13,
    p1_min_otherexp = Q24_1,
    p1_max_otherexp = Q24_2,
    p1_MAO_other = Q24_3,
    p1_HAO = Q25_1,
    p2_min_otherexp = Q23_1,
    p2_max_otherexp = Q23_2,
    p2_MAO = Q26_1,
    fairoffer = Q27_1,
    howfelt = Q6_1,
    fairoutcome = Q7_1,
    forex_exp = Q8_1,
    travel_exp = Q8_2,
    SVO1 = Q50,
    SVO2 = Q52,
    SVO3 = Q54,
    SVO4 = Q56,
    SVO5 = Q58,
    SVO6 = Q60,
    SVO7 = Q62,
    SVO8 = Q64,
    SVO9 = Q66,
    attcheck2 = Q31,
    duration = `Duration (in seconds)`
  )

nacols <- c("UG_exp","readins")
b[,nacols] <- NA

bb <- bb %>%
  rename(
    attcheck1 = Q39,
    bankfee = Q42,
    brokerfee = Q44,
    servicecharge = Q46,
    ATMfee = Q48,
    age = Q1_1,
    occupation = Q1_2,
    nationality = Q1_3,
    gender = Q30,
    role = Q13,
    p1_min_otherexp = Q24_1,
    p1_max_otherexp = Q24_2,
    p1_MAO_other = Q24_3,
    p1_HAO = Q25_1,
    p2_min_otherexp = Q23_1,
    p2_max_otherexp = Q23_2,
    p2_MAO = Q26_1,
    fairoffer = Q27_1,
    howfelt = Q6_1,
    fairoutcome = Q7_1,
    forex_exp = Q8_1,
    travel_exp = Q8_2,
    SVO1 = Q50,
    SVO2 = Q52,
    SVO3 = Q54,
    SVO4 = Q56,
    SVO5 = Q58,
    SVO6 = Q60,
    SVO7 = Q62,
    SVO8 = Q64,
    SVO9 = Q66,
    UG_exp = Q80,
    readins = Q55,
    attcheck2 = Q31,
    duration = `Duration (in seconds)`
  )

bank <- rbind(b,bb)
bank <- subset(bank, select = -c(
  label, isbot, pagenumber, maxpage,
  round, pagename, time, visited, subsessionround,
  session.label, session.experimenter_name, session.time_started,
  session.comment, StartDate, EndDate, Status, IPAddress, Progress,
  Finished, RecordedDate, RecipientLastName, RecipientFirstName,
  RecipientEmail, ExternalReference, DistributionChannel))

c <- bank
rm(b); rm(b.reps); rm(bb); rm(d); rm(df); rm(e); rm(ef); rm(surv); rm(surv2)
rm(reps); rm(bank)
c$duration <- as.numeric(c$duration)
c$LocationLatitude <- as.numeric(c$LocationLatitude)
c$LocationLongitude <- as.numeric(c$LocationLongitude)
c$bankfee <- as.numeric(c$bankfee)
c$brokerfee <- as.numeric(c$brokerfee)
c$servicecharge <- as.numeric(c$servicecharge)
c$ATMfee <- as.numeric(c$ATMfee)
c$age <- as.numeric(c$age)
c$p1_min_otherexp <- as.numeric(c$p1_min_otherexp)
c$p1_max_otherexp <- as.numeric(c$p1_max_otherexp)
c$p1_MAO_other <- as.numeric(c$p1_MAO_other)
c$p1_HAO <- as.numeric(c$p1_HAO)
c$p2_min_otherexp <- as.numeric(c$p2_min_otherexp)
c$p2_max_otherexp <- as.numeric(c$p2_max_otherexp)
c$p2_MAO <- as.numeric(c$p2_MAO)
c$fairoffer <- as.numeric(c$fairoffer)

c$forex_exp[c$forex_exp=='&nbsp;'] <- NA
c$travel_exp[c$travel_exp=='&nbsp;'] <- NA
c$app[c$app=='ultimatum_banker'] <- 'banker'
ca <- c

## data manipulation #####

ca$howfelt <- factor(ca$howfelt, levels = c('Very negatively',
                                            'Somewhat negatively',
                                            'Neutral',
                                            'Somewhat positively',
                                            'Very positively'))
ca$howfelt <- as.integer(ca$howfelt)

ca$fairoutcome <- factor(ca$fairoutcome, levels = c('Strongly disagree',
                                                    'Somewhat disagree',
                                                    'Neither agree nor disagree',
                                                    'Somewhat agree',
                                                    'Strongly agree'))
ca$fairoutcome <- as.integer(ca$fairoutcome)

ca$forex_exp <- factor(ca$forex_exp, levels = c('No experience at all',
                                                'Lower than average',
                                                'Some experience',
                                                'Higher than average',
                                                'Extensive experience'))
ca$forex_exp <- as.integer(ca$forex_exp)

ca$travel_exp <- factor(ca$travel_exp, levels = c('No experience at all',
                                                  'Lower than average',
                                                  'Some experience',
                                                  'Higher than average',
                                                  'Extensive experience'))
ca$travel_exp <- as.integer(ca$travel_exp)
UG.banker <- ca
rm(ca); rm(c)
#setwd("~/Desktop/MA Thesis/framingdata2017")
use_data(UG.banker, overwrite = TRUE)




