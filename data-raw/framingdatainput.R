library(readr)
library(readxl)
library(dplyr)
control <- read_csv("data-raw/ultimatum_t1 (accessed 2017-02-09) copy.csv")
timespent <- read_csv("data-raw/TimeSpent (accessed 2017-02-09).csv")
survey <- read_excel("data-raw/Post-experiment+questionnaire_February+9%2C+2017_10.41.xls")

control <-
  control %>%
  dplyr::filter(participant.mturk_worker_id != "") %>%
  rename(
    participant_id = participant.id_in_session,
    mturk_id = participant.mturk_worker_id
  )

use_data(control, timespent, survey, overwrite = TRUE)

