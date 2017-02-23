library(readr)
library(readxl)
library(dplyr)
library(devtools)
setwd("~/Desktop/MA Thesis/framingdata2017")
control.data <- read_csv("data-raw/ultimatum_t1 (accessed 2017-02-09) copy.csv")
control.timespent <- read_csv("data-raw/TimeSpent (accessed 2017-02-09).csv")
control.survey <- read_excel("data-raw/Post-experiment+questionnaire_February+9%2C+2017_10.41.xls")
banker.data <- read_csv("data-raw/ultimatum_banker (accessed 2017-02-14) (1).csv")
customer.data <- read_csv("data-raw/ultimatum_customer (accessed 2017-02-14) (1).csv")
treatment.survey <- read_csv("data-raw/Post-experiment+questionnaire+-+Treatment_February+14%2C+2017_05.05.csv")

# filtering data
control.data <-
  control.data %>%
  dplyr::filter(participant.mturk_worker_id != "") %>%
  rename(
    participant_id = participant.id_in_session,
    code = participant.code,
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
    mturk.id = participant.mturk_worker_id,
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


### saves data to package with git commit ####

control.survey <-
  control.survey[-c(1,2),]
use_data(control.data, control.timespent, control.survey, overwrite = TRUE)

