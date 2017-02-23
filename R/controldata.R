## Control data manipulation
library(readr)
library(readxl)
library(dplyr)
library(devtools)
library(tidyverse)

setwd("~/Desktop/MA Thesis/framingdata2017")
data1 <- read_csv("data-raw/ultimatum_t1 (accessed 2017-02-09) copy.csv")
timespent <- read_csv("data-raw/TimeSpent (accessed 2017-02-09).csv")
survey <- read_excel("data-raw/Post-experiment+questionnaire_February+9%2C+2017_10.41.xls")

data1 <- data1 %>%
  rename(userid = player.valid_code)
survey$mTurkCode <- as.numeric(survey$mTurkCode)
survey <- survey[,-c(17,49)]
survey <- survey %>%
  rename(userid = mTurkCode)

#yt <- left_join(data1, survey, by="userid")


data1 <-
  data1 %>%
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
    #validcode = player.valid_code,
    payoff = player.payoff,
    subsessionid = group.id_in_subsession,
    offer = group.amount_offered,
    accept = group.offer_accepted,
    subsessionround = subsession.round_number
  ) %>%
  dplyr::filter(userid != "")

survey <- survey %>%
  rename(
    attcheck1 = Q39,
    bankfee = Q28,
    brokerfee = Q42,
    servicecharge = Q43,
    ATMfee = Q44,
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
    SVO1 = Q16,
    SVO2 = Q17,
    SVO3 = Q32,
    SVO4 = Q33,
    SVO5 = Q34,
    SVO6 = Q35,
    SVO7 = Q36,
    SVO8 = Q37,
    SVO9 = Q38,
    attcheck2 = Q31,
    duration = `Duration (in seconds)`
  )

nacols <- c("UG_exp","readins")
survey[,nacols] <- NA

yt <- left_join(data1, survey, by="userid")

yt <- subset(yt, select = -c(
  label, isbot, pagenumber, maxpage,
  round, pagename, ip, time, visited, subsessionround,
  session.label, session.experimenter_name, session.time_started,
  session.comment, StartDate, EndDate, Status, IPAddress, Progress,
  Finished, RecordedDate, RecipientLastName, RecipientFirstName,
  RecipientEmail, ExternalReference, DistributionChannel))
ca <- yt
#tb <- tbl_df(c)
#cols = c(7:ncol(cf))
#cf[,cols] = apply(cf[,cols], 2, function(x) as.numeric(as.character(x)))

# to be reformatted
ca$duration <- as.numeric(ca$duration)
ca$LocationLatitude <- as.numeric(ca$LocationLatitude)
ca$LocationLongitude <- as.numeric(ca$LocationLongitude)
ca$bankfee <- as.numeric(ca$bankfee)
ca$brokerfee <- as.numeric(ca$brokerfee)
ca$servicecharge <- as.numeric(ca$servicecharge)
ca$ATMfee <- as.numeric(ca$ATMfee)
ca$age <- as.numeric(ca$age)
ca$p1_min_otherexp <- as.numeric(ca$p1_min_otherexp)
ca$p1_max_otherexp <- as.numeric(ca$p1_max_otherexp)
ca$p1_MAO_other <- as.numeric(ca$p1_MAO_other)
ca$p1_HAO <- as.numeric(ca$p1_HAO)
ca$p2_min_otherexp <- as.numeric(ca$p2_min_otherexp)
ca$p2_max_otherexp <- as.numeric(ca$p2_max_otherexp)
ca$p2_MAO <- as.numeric(ca$p2_MAO)
ca$fairoffer <- as.numeric(ca$fairoffer)

ca$forex_exp[ca$forex_exp=='&nbsp;'] <- NA
ca$travel_exp[ca$travel_exp=='&nbsp;'] <- NA

UG.control <- ca

use_data(UG.control, overwrite = TRUE)


