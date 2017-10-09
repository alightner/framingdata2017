library(readr)
library(readxl)
library(dplyr)
library(devtools)
library(tidyverse)

load("~/Desktop/Docs/MA Thesis/framingdata2017/data/UG.banker.rda")
load("~/Desktop/Docs/MA Thesis/framingdata2017/data/UG.control.rda")
load("~/Desktop/Docs/MA Thesis/framingdata2017/data/UG.customer.rda")

du <- rbind(UG.control, UG.banker, UG.customer)

## SVO transform ########################
## code: 1=competitive, 2=individualistic, 3=prosocial
du$SVO1 <- factor(du$SVO1, levels = c('480\n80','540\n280','480\n480'))
du$SVO2 <- factor(du$SVO2, levels = c('500\n100','560\n300','500\n500'))
du$SVO3 <- factor(du$SVO3, levels = c('520\n120','580\n320','520\n520'))
du$SVO4 <- factor(du$SVO4, levels = c('500\n100','560\n300','490\n490'))
du$SVO5 <- factor(du$SVO5, levels = c('490\n90','560\n300','500\n500'))
du$SVO6 <- factor(du$SVO6, levels = c('500\n100','570\n300','500\n500'))
du$SVO7 <- factor(du$SVO7, levels = c('510\n110','560\n300','510\n510'))
du$SVO8 <- factor(du$SVO8, levels = c('500\n100','550\n300','500\n500'))
du$SVO9 <- factor(du$SVO9, levels = c('480\n100','540\n300','490\n490'))
#cols <- select(du, SVO1:SVO9)
#du[,cols] <- apply(du[,cols], 2, function(x) as.integer(as.character(x)))
du$SVO1 <- as.integer(du$SVO1)
du$SVO2 <- as.integer(du$SVO2)
du$SVO3 <- as.integer(du$SVO3)
du$SVO4 <- as.integer(du$SVO4)
du$SVO5 <- as.integer(du$SVO5)
du$SVO6 <- as.integer(du$SVO6)
du$SVO7 <- as.integer(du$SVO7)
du$SVO8 <- as.integer(du$SVO8)
du$SVO9 <- as.integer(du$SVO9)

#select(du, SVO1:SVO9,prosocial)
#du <- du %>%
# mutate(prosocial = sum(na.omit(SVO1==3, SVO2==3, SVO3==3,
#                       SVO4==3, SVO5==3, SVO6==3,
#                      SVO7==3, SVO8==3, SVO9==3)))

du$prosocial <- 0
du$individual <- 0
du$competitive <- 0

for(i in 1:dim(du)[1]){
  g <- c(du$SVO1[i]==3,du$SVO2[i]==3,du$SVO3[i]==3,
         du$SVO4[i]==3,du$SVO5[i]==3,du$SVO6[i]==3,
         du$SVO7[i]==3,du$SVO8[i]==3,du$SVO9[i]==3)
  g <- as.integer(as.logical(g))
  du$prosocial[i] <- sum(g, na.rm = TRUE)
}
for(i in 1:dim(du)[1]){
  g <- c(du$SVO1[i]==2,du$SVO2[i]==2,du$SVO3[i]==2,
         du$SVO4[i]==2,du$SVO5[i]==2,du$SVO6[i]==2,
         du$SVO7[i]==2,du$SVO8[i]==2,du$SVO9[i]==2)
  g <- as.integer(as.logical(g))
  du$individual[i] <- sum(g, na.rm = TRUE)
}
for(i in 1:dim(du)[1]){
  g <- c(du$SVO1[i]==1,du$SVO2[i]==1,du$SVO3[i]==1,
         du$SVO4[i]==1,du$SVO5[i]==1,du$SVO6[i]==1,
         du$SVO7[i]==1,du$SVO8[i]==1,du$SVO9[i]==1)
  g <- as.integer(as.logical(g))
  du$competitive[i] <- sum(g, na.rm = TRUE)
}


dplyr::select(du, SVO1:SVO9,prosocial:competitive)
#####################################################################
#### subsetting to relevant vars ################

du <- du %>%
  dplyr::select(
    app, playerid, offer, accept, duration, attcheck1,
    bankfee:competitive
  )

#wd <- getwd()
#setwd('/Users/aaronlightner/Desktop/Docs')
ac1 <- read_csv('data-raw/supplement_banker_surv.csv')
ac2 <- read_csv('data-raw/supplement customer surv.csv')
ac1 <- ac1[-c(1:2),]
ac2 <- ac2[-c(1:2),]
ac <- rbind(ac1, ac2)
ac <- ac[,c(18,52:54)]
ac3 <- read_csv('data-raw/supplemental surv both.csv')
#setwd(wd)
ac3 <- ac3[-c(1:2),]
ac3 <- ac3[,c(18,50:52)]
ac <- rbind(ac, ac3)
ac <- left_join(ac, du, by="mTurkCode")
ac <- ac %>% dplyr::rename(a1=Q39_1, a2=Q31_1)
ac$a1 <- factor(ac$a1); ac$a1[ac$a1==""] <- NA
ac$a2 <- factor(ac$a2); ac$a2[ac$a2==""] <- NA

y <- na.omit(ac$mTurkCode[ac$a1=='Other'])
w <- subset(du, !(mTurkCode %in% y))
# att check 2
#yy <- na.omit(ac$mTurkCode[ac$a2=='Clock'])
#w <- subset(w, mTurkCode %in% yy)
# "att check 3"
w <- subset(w, p1_max_otherexp > p1_min_otherexp | p2_max_otherexp > p2_min_otherexp)
#w <- subset(w, UG_exp == "No" | is.na(UG_exp))
# exclude fast duration
w <- subset(w, duration >= 90)
du <- w
du$offer2 <- du$offer/100
du$app <- factor(du$app, levels=c('control', 'banker', 'customer'))
dataAMT_pilot <- du
rm(ac); rm(ac1); rm(ac2); rm(ac3); rm(du); rm(w); rm(g); rm(i); rm(y)
#setwd("~/Desktop/MA Thesis/framingdata2017")
use_data(dataAMT_pilot, overwrite = TRUE)


