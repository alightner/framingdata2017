library(readr)
library(readxl)
library(dplyr)
library(devtools)
library(tidyverse)

setwd("~/Desktop/MA Thesis/Treatment data")
d <- read_csv("TimeSpent (accessed 2017-02-14) (1).csv")
dt <- read_csv("ultimatum_banker (accessed 2017-02-14) (1).csv")
df <- read_csv("ultimatum_customer (accessed 2017-02-14) (1).csv")
dtt <- rbind(dt,df)

surv <- read_csv("Post-experiment+questionnaire+-+Treatment_February+14%2C+2017_05.05 (1).csv")
dtt <- dtt %>%
  rename(userid = participant.code)
d <- d %>%
  rename(userid = participant__code)
a <- left_join(dtt, surv, by="userid")
a <- left_join(a, d, by="userid")

#### histograms #######
ggplot(d, aes(x=seconds_on_page)) + geom_histogram() +
  facet_wrap(~page_name, scales = "free_x")

ggplot(dt, aes(group.amount_offered)) + geom_histogram()
ggplot(df, aes(group.amount_offered)) + geom_histogram()
ggplot(df, aes(group.amount_offered)) + geom_histogram() +
  facet_wrap(~group.offer_accepted)

ggplot(dt, aes(group.amount_offered)) + geom_histogram() +
  facet_wrap(~group.offer_accepted)


######## plots ##########
x <- na.omit(a$seconds_on_page[a$page_name=="Introduction"])
y <- a$group.amount_offered[a$page_name=="Introduction"]
ggplot(a, aes(x=seconds_on_page,
              y=group.amount_offered)) +
  facet_wrap(~page_name) +
  geom_point() + geom_smooth(se=FALSE) + ylim(0,100) + xlim(0,300)


m <- glm(a$group.offer_accepted[a$page_name=='Introduction']~a$seconds_on_page[a$page_name=='Introduction'], family=binomial)
n <- predict(m, list(seconds_on_page=seq(0,300,1)), type='response')
temp <- as.matrix(n); temp <- cbind(n, seq(0,300,1))
temp <- as.data.frame(temp)
ggplot(temp, aes(x=V2,y=n)) + geom_point() +
  labs(x='seconds on introduction', y='prob accept') +
  geom_smooth(se=FALSE)

gr <- ggplot() + geom_line(data = dt, aes(x=V4, y=V1, color='Control')) +
  geom_line(data = dt, aes(x=V4, y=n2, color='Customer')) +
  geom_line(data = dt, aes(x=V4, y=n3, color='Banker')) +
  labs(x="Percent Offered", y="Probability of acceptance") +
  scale_color_manual(name="Responder Type", values = c(Control = 'black', Customer = 'red', Banker = 'blue'))
gr


### repeats' plots ###############

rep_id <- c('A19F75QCU199K8', 'A1D9FOXZ0UT0L2', 'A2AAY4VT9L71SY','A2AUDQQQALH4Q8',
            'A2RBNMRC6942T0','A2SBISQJE2ZPJQ','A2UMB419R9QW8L','A317RA15E0SZ4P',
            'A33KTU8KJ5CP2T','A3SOJWB6AZWZV9','A3VA5UDZ327VT0','A3VANXT8EEKXQA',
            'A8P66AMRDFLLT','AAGZ40ERLMIPB', 'AELFDOPFPTHT0', 'AHDW0M4017Z8K',
            'AK467M5VTG7N4', 'AKHVXXXX53S5G')
#repeats <- a[a$participant.mturk_worker_id==rep_id]
repeats <- subset(a, participant.mturk_worker_id %in% rep_id)
noreps <- subset(a, !(participant.mturk_worker_id %in% rep_id))
ggplot(repeats, aes(x=seconds_on_page,
              y=group.amount_offered)) +
  facet_wrap(~page_name) +
  geom_point() + geom_smooth(se=FALSE) + ylim(0,100) + xlim(0,1000)

ggplot(noreps, aes(x=seconds_on_page)) + geom_histogram() +
  facet_wrap(~page_name)
ggplot(noreps, aes(x=seconds_on_page,
              y=group.amount_offered)) +
  facet_wrap(~page_name) +
  geom_point() + geom_smooth(se=FALSE) + ylim(0,100) + xlim(0,300)

ggplot(noreps, aes(x=seconds_on_page,
                    y=group.amount_offered)) +
  facet_wrap(~page_name) +
  geom_point() + geom_smooth(se=FALSE) + ylim(0,100) + xlim(0,300)


## customer plots #######

v2 <- a$Q8_1; v3 <- a$Q8_2
u <- subset(a, app_name=="ultimatum_customer")
ggplot(data=u, aes(x=group.amount_offered)) + geom_histogram() +
  facet_wrap(~Q8_1)

ggplot(data=u, aes(x=group.amount_offered)) + geom_histogram() +
  facet_wrap(~Q8_2)

ggplot(data=u, aes(x=group.amount_offered)) + geom_histogram()
ggplot(data=noreps, aes(x=group.amount_offered)) + geom_histogram()
ggplot(data=repeats, aes(x=group.amount_offered)) + geom_histogram()

ggplot(data=u, aes(x=group.amount_offered)) + geom_histogram() +
  facet_wrap(~Q1_3)

proposers <- subset(u, player.id_in_group==1)
proposers$Q42 <- as.numeric(proposers$Q42)
ggplot(proposers, aes(x=Q42, y=group.amount_offered)) + geom_point()





###########

reps <- subset(df, participant.mturk_worker_id %in% rep_id)
View(reps)

