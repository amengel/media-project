#----------------------------------------------------------------------------------------------------#
# Created by: Drew Engelhardt 6-20-2017
# Last modified by: Drew Engelhardt 6-20-2017
# Filename: MIP_cleaning_v2.R
# File purpose: Data set cleaning and plotting for Gallup MIP
#----------------------------------------------------------------------------------------------------#
library(lmtest)

source("~/Dropbox/Vandy/Elite Race Rhetoric Project/Code/MIP_cleaning_v2.R")

setwd("~/Dropbox/Vandy/Elite Race Rhetoric Project/")

# coded.dat <- read.csv("~/Dropbox/Vandy/Prospectus Prelims/race rhetoric/case studies/Race stories/coded_data_061617_noObama.csv", stringsAsFactors = F)
coded.dat <- read.csv("~/Dropbox/Vandy/Prospectus Prelims/race rhetoric/case studies/Race stories/coded_data_062317_noObama.csv", stringsAsFactors = F)
coded.dat$date <- as.Date(coded.dat$date)
coded.dat$month <- format(coded.dat$date, "%m")
coded.dat$year <- format(coded.dat$date, "%Y")
# coded.dat$show <- as.factor(coded.dat$show)


## Appending show proportions for aggregate analyses
dates <- mip.sum.race$int.max

coded.maddow <- subset(coded.dat, show == "Maddow")
coded.oreilly <- subset(coded.dat, show == "O'Reilly")

mip.sum.race$maddow.prop14 <- NA
mip.sum.race$oreilly.prop14 <- NA
mip.sum.race$tot.prop14 <- NA
mip.sum.race$maddow.prop7 <- NA
mip.sum.race$oreilly.prop7 <- NA
mip.sum.race$tot.prop7 <- NA

for(i in 1:length(dates)){
  total14 <- sum(dates[i] - coded.dat$date <= 14 & dates[i] - coded.dat$date > 0, na.rm = T)
  race14 <- sum(dates[i] - coded.dat$date[which(coded.dat$race == 1)] <= 14 
              & dates[i] - coded.dat$date[which(coded.dat$race == 1)] > 0)
  mip.sum.race$tot.prop14[i] <- (race14/total14)
  maddow14 <- sum(dates[i] - coded.maddow$date <= 14 
                     & dates[i] - coded.maddow$date > 0)
  maddow.race14 <- sum(dates[i] - coded.maddow$date[which(coded.maddow$race == 1)] <= 14 
                & dates[i] - coded.maddow$date[which(coded.maddow$race == 1)] > 0)
  mip.sum.race$maddow.prop14[i] <- (maddow.race14/maddow14)
  oreilly14 <- sum(dates[i] - coded.oreilly$date <= 14 
                      & dates[i] - coded.oreilly$date > 0)
  oreilly.race14 <- sum(dates[i] - coded.oreilly$date[which(coded.oreilly$race == 1)] <= 14 
                 & dates[i] - coded.oreilly$date[which(coded.oreilly$race == 1)] > 0)
  mip.sum.race$oreilly.prop14[i] <- (oreilly.race14/oreilly14)
  
  total7 <- sum(dates[i] - coded.dat$date <= 7 & dates[i] - coded.dat$date > 0)
  race7 <- sum(dates[i] - coded.dat$date[which(coded.dat$race == 1)] <= 7 
                & dates[i] - coded.dat$date[which(coded.dat$race == 1)] > 0)
  mip.sum.race$tot.prop7[i] <- (race7/total7)
  maddow7 <- sum(dates[i] - coded.maddow$date <= 7 
                  & dates[i] - coded.maddow$date > 0)
  maddow.race7 <- sum(dates[i] - coded.maddow$date[which(coded.maddow$race == 1)] <= 7 
                       & dates[i] - coded.maddow$date[which(coded.maddow$race == 1)] > 0)
  mip.sum.race$maddow.prop7[i] <- (maddow.race7/maddow7)
  oreilly7 <- sum(dates[i] - coded.oreilly$date <= 7 
                   & dates[i] - coded.oreilly$date > 0)
  oreilly.race7 <- sum(dates[i] - coded.oreilly$date[which(coded.oreilly$race == 1)] <= 7 
                        & dates[i] - coded.oreilly$date[which(coded.oreilly$race == 1)] > 0)
  mip.sum.race$oreilly.prop7[i] <- (oreilly.race7/oreilly7)
}

## Appending discussion data to individual-level data
stacked$tot.prop14 <- NA
stacked$maddow.prop14 <- NA
stacked$oreilly.prop14 <- NA
stacked$tot.prop7 <- NA
stacked$maddow.prop7 <- NA
stacked$oreilly.prop7 <- NA

dates <- unique(stacked$date)
for(i in 1:length(dates)){
  total14 <- sum(dates[i] - coded.dat$date <= 14 & dates[i] - coded.dat$date > 0)
  race14 <- sum(dates[i] - coded.dat$date[which(coded.dat$race == 1)] <= 14 
                & dates[i] - coded.dat$date[which(coded.dat$race == 1)] > 0)
  stacked$tot.prop14[which(stacked$date == dates[i])] <- (race14/total14)
  
  maddow14 <- sum(dates[i] - coded.maddow$date <= 14 
                  & dates[i] - coded.maddow$date > 0)
  maddow.race14 <- sum(dates[i] - coded.maddow$date[which(coded.maddow$race == 1)] <= 14 
                       & dates[i] - coded.maddow$date[which(coded.maddow$race == 1)] > 0)
  stacked$maddow.prop14[which(stacked$date == dates[i])] <- (maddow.race14/maddow14)
  
  oreilly14 <- sum(dates[i] - coded.oreilly$date <= 14 
                   & dates[i] - coded.oreilly$date > 0)
  oreilly.race14 <- sum(dates[i] - coded.oreilly$date[which(coded.oreilly$race == 1)] <= 14 
                        & dates[i] - coded.oreilly$date[which(coded.oreilly$race == 1)] > 0)
  stacked$oreilly.prop14[which(stacked$date == dates[i])] <- (oreilly.race14/oreilly14)
  
  
  total7 <- sum(dates[i] - coded.dat$date <= 7 & dates[i] - coded.dat$date > 0)
  race7 <- sum(dates[i] - coded.dat$date[which(coded.dat$race == 1)] <= 7 
               & dates[i] - coded.dat$date[which(coded.dat$race == 1)] > 0)
  stacked$tot.prop7[which(stacked$date == dates[i])] <- (race7/total7)
  
  maddow7 <- sum(dates[i] - coded.maddow$date <= 7 
                 & dates[i] - coded.maddow$date > 0)
  maddow.race7 <- sum(dates[i] - coded.maddow$date[which(coded.maddow$race == 1)] <= 7 
                      & dates[i] - coded.maddow$date[which(coded.maddow$race == 1)] > 0)
  stacked$maddow.prop7[which(stacked$date == dates[i])] <- (maddow.race7/maddow7)
  
  oreilly7 <- sum(dates[i] - coded.oreilly$date <= 7 
                  & dates[i] - coded.oreilly$date > 0)
  oreilly.race7 <- sum(dates[i] - coded.oreilly$date[which(coded.oreilly$race == 1)] <= 7 
                       & dates[i] - coded.oreilly$date[which(coded.oreilly$race == 1)] > 0)
  stacked$oreilly.prop7[which(stacked$date == dates[i])] <- (oreilly.race7/oreilly7)
}

#----------------------------------------------------------------------------------------------------#
# Aggregate analyses
#----------------------------------------------------------------------------------------------------#
# removing first row because of limited show observations
dat.w <- subset(mip.sum.race[-1,], Race == "White")
dat.nw <- subset(mip.sum.race[-1,], Race == "Non-white")

m1 <- lm(race.prop.weighted ~ tot.prop14, 
         data = dat.w)
summary(m1)

m1.m <- lm(race.prop.weighted ~ maddow.prop14, 
         data = dat.w)
summary(m1.m)

m1.o <- lm(race.prop.weighted ~ oreilly.prop14, 
           data = dat.w)
summary(m1.o)

# lagging variables
m1.1 <- lm(race.prop.weighted[-1] ~ tot.prop14[-1] + race.prop.weighted[-94], 
         data = dat.w)
summary(m1.1)

m1.1.m <- lm(race.prop.weighted[-1] ~ maddow.prop14[-1] + race.prop.weighted[-94], 
           data = dat.w)
summary(m1.1.m)

m1.1.o <- lm(race.prop.weighted[-1] ~ oreilly.prop14[-1] + race.prop.weighted[-94], 
           data = dat.w)
summary(m1.1.o)

m1.1.mo <- lm(race.prop.weighted[-1] ~ maddow.prop14[-1] + oreilly.prop14[-1] + race.prop.weighted[-94], 
             data = dat.w)
summary(m1.1.mo)

# bptest(m1.1.o)

# 1 week prior
m1.7d <- lm(race.prop.weighted ~ tot.prop7, 
         data = dat.w)
summary(m1.7d)

m1.7d.m <- lm(race.prop.weighted ~ maddow.prop7, 
           data = dat.w)
summary(m1.7d.m)

m1.7d.o <- lm(race.prop.weighted ~ oreilly.prop7, 
           data = dat.w)
summary(m1.7d.o)

m1.7d.1 <- lm(race.prop.weighted[-1] ~ tot.prop7[-1] + race.prop.weighted[-94], 
           data = dat.w)
summary(m1.7d.1)

m1.7d.1.m <- lm(race.prop.weighted[-1] ~ maddow.prop7[-1] + race.prop.weighted[-94], 
             data = dat.w)
summary(m1.7d.1.m)

m1.7d.1.o <- lm(race.prop.weighted[-1] ~ oreilly.prop7[-1] + race.prop.weighted[-94], 
             data = dat.w)
summary(m1.7d.1.o)

m1.7d.1.mo <- lm(race.prop.weighted[-1] ~ maddow.prop7[-1] + oreilly.prop7[-1] + race.prop.weighted[-94], 
                data = dat.w)
summary(m1.7d.1.mo)

## Non-whites
m10 <- lm(race.prop.weighted ~ tot.prop14, 
         data = dat.nw)
summary(m10)

m10.m <- lm(race.prop.weighted ~ maddow.prop14, 
           data = dat.nw)
summary(m10.m)

m10.o <- lm(race.prop.weighted ~ oreilly.prop14, 
           data = dat.nw)
summary(m10.o)

m10.1 <- lm(race.prop.weighted[-1] ~ tot.prop14[-1] + race.prop.weighted[-94], 
           data = dat.nw)
summary(m10.1)

m10.1.m <- lm(race.prop.weighted[-1] ~ maddow.prop14[-1] + race.prop.weighted[-94], 
           data = dat.nw)
summary(m10.1.m)

m10.1.o <- lm(race.prop.weighted[-1] ~ oreilly.prop14[-1] + race.prop.weighted[-94], 
           data = dat.nw)
summary(m10.1.o)

m10.1.mo <- lm(race.prop.weighted[-1] ~ maddow.prop14[-1] + oreilly.prop14[-1] + race.prop.weighted[-94], 
              data = dat.nw)
summary(m10.1.mo)

# 1 week prior
m10.7d <- lm(race.prop.weighted ~ tot.prop7, 
          data = dat.nw)
summary(m10.7d)

m10.7d.m <- lm(race.prop.weighted ~ maddow.prop7, 
            data = dat.nw)
summary(m10.7d.m)

m10.7d.o <- lm(race.prop.weighted ~ oreilly.prop7, 
            data = dat.nw)
summary(m10.7d.o)

m10.7d.1 <- lm(race.prop.weighted[-1] ~ tot.prop7[-1] + race.prop.weighted[-94], 
            data = dat.nw)
summary(m10.7d.1)

m10.7d.1.m <- lm(race.prop.weighted[-1] ~ maddow.prop7[-1] + race.prop.weighted[-94], 
              data = dat.nw)
summary(m10.7d.1.m)

m10.7d.1.o <- lm(race.prop.weighted[-1] ~ oreilly.prop7[-1] + race.prop.weighted[-94], 
              data = dat.nw)
summary(m10.7d.1.o)

m10.7d.1.mo <- lm(race.prop.weighted[-1] ~ maddow.prop7[-1] + oreilly.prop7[-1] + race.prop.weighted[-94], 
                 data = dat.nw)
summary(m10.7d.1.mo)


#### Race Plus MIP
m2 <- lm(race.plus.prop.weighted ~ tot.prop14, 
         data = dat.w)
summary(m2)

m2.m <- lm(race.plus.prop.weighted ~ maddow.prop14, 
           data = dat.w)
summary(m2.m)

m2.o <- lm(race.plus.prop.weighted ~ oreilly.prop14, 
           data = dat.w)
summary(m2.o)

m2.1 <- lm(race.plus.prop.weighted[-1] ~ tot.prop14[-1] + race.plus.prop.weighted[-94], 
           data = dat.w)
summary(m2.1)

m2.1.m <- lm(race.plus.prop.weighted[-1] ~ maddow.prop14[-1] + race.plus.prop.weighted[-94], 
           data = dat.w)
summary(m2.1.m)

m2.1.o <- lm(race.plus.prop.weighted[-1] ~ oreilly.prop14[-1] + race.plus.prop.weighted[-94], 
           data = dat.w)
summary(m2.1.o)

m2.1.mo <- lm(race.plus.prop.weighted[-1] ~  maddow.prop14[-1] + oreilly.prop14[-1] + race.plus.prop.weighted[-94], 
             data = dat.w)
summary(m2.1.mo)


# 1 week prior
m2.7d <- lm(race.plus.prop.weighted ~ tot.prop7, 
         data = dat.w)
summary(m2.7d)

m2.7d.m <- lm(race.plus.prop.weighted ~ maddow.prop7, 
           data = dat.w)
summary(m2.7d.m)

m2.7d.o <- lm(race.plus.prop.weighted ~ oreilly.prop7, 
           data = dat.w)
summary(m2.7d.o)

m2.7d.1 <- lm(race.plus.prop.weighted[-1] ~ tot.prop7[-1] + race.plus.prop.weighted[-94], 
           data = dat.w)
summary(m2.7d.1)

m2.7d.1.m <- lm(race.plus.prop.weighted[-1] ~ maddow.prop7[-1] + race.plus.prop.weighted[-94], 
             data = dat.w)
summary(m2.7d.1.m)

m2.7d.1.o <- lm(race.plus.prop.weighted[-1] ~ oreilly.prop7[-1] + race.plus.prop.weighted[-94], 
             data = dat.w)
summary(m2.7d.1.o)

m2.7d.1.mo <- lm(race.plus.prop.weighted[-1] ~ maddow.prop7[-1] + oreilly.prop7[-1] + race.plus.prop.weighted[-94], 
                data = dat.w)
summary(m2.7d.1.mo)

## Non-whites
m20 <- lm(race.plus.prop.weighted ~ tot.prop14, 
          data = dat.nw)
summary(m20)

m20.m <- lm(race.plus.prop.weighted ~ maddow.prop14, 
            data = dat.nw)
summary(m20.m)

m20.o <- lm(race.plus.prop.weighted ~ oreilly.prop14, 
            data = dat.nw)
summary(m20.o)

m20.1 <- lm(race.plus.prop.weighted[-1] ~ tot.prop14[-1] + race.plus.prop.weighted[-94], 
            data = dat.nw)
summary(m20.1)

m20.1.m <- lm(race.plus.prop.weighted[-1] ~ maddow.prop14[-1] + race.plus.prop.weighted[-94], 
            data = dat.nw)
summary(m20.1.m)

m20.1.o <- lm(race.plus.prop.weighted[-1] ~ oreilly.prop14[-1] + race.plus.prop.weighted[-94], 
            data = dat.nw)
summary(m20.1.o)

m20.1.mo <- lm(race.plus.prop.weighted[-1] ~  maddow.prop14[-1] +  oreilly.prop14[-1] + race.plus.prop.weighted[-94], 
              data = dat.nw)
summary(m20.1.mo)

# 1 week prior
m20.7d <- lm(race.plus.prop.weighted ~ tot.prop7, 
          data = dat.nw)
summary(m20.7d)

m20.7d.m <- lm(race.plus.prop.weighted ~ maddow.prop7, 
            data = dat.nw)
summary(m20.7d.m)

m20.7d.o <- lm(race.plus.prop.weighted ~ oreilly.prop7, 
            data = dat.nw)
summary(m20.7d.o)

m20.7d.1 <- lm(race.plus.prop.weighted[-1] ~ tot.prop7[-1] + race.plus.prop.weighted[-94], 
            data = dat.nw)
summary(m20.7d.1)

m20.7d.1.m <- lm(race.plus.prop.weighted[-1] ~ maddow.prop7[-1] + race.plus.prop.weighted[-94], 
              data = dat.nw)
summary(m20.7d.1.m)

m20.7d.1.o <- lm(race.plus.prop.weighted[-1] ~ oreilly.prop7[-1] + race.plus.prop.weighted[-94], 
              data = dat.nw)
summary(m20.7d.1.o)

m20.7d.1.mo <- lm(race.plus.prop.weighted[-1] ~ maddow.prop7[-1] + oreilly.prop7[-1] + race.plus.prop.weighted[-94], 
                 data = dat.nw)
summary(m20.7d.1.mo)

#----------------------------------------------------------------------------------------------------#
# Individual analyses
#----------------------------------------------------------------------------------------------------#
m1 <- glm(as.factor(race) ~ pid3 + age + fem + as.factor(educ) + south,
             weights = wtfctr,
             data = stacked,
             subset = raceban1 == "Non-Hispanic White",
             family = binomial(link = "probit"))
summary(m1)

m1.fe <- glm(as.factor(race) ~ pid3 + age + fem + as.factor(educ) + south 
             + as.factor(df),
             weights = wtfctr,
             data = stacked,
             subset = raceban1 == "Non-Hispanic White",
             family = binomial(link = "probit"))
summary(m1.fe)


m10.tot <- glm(as.factor(race) ~ pid3*tot.prop14 + age 
              + fem + as.factor(educ) + south,
              weights = wtfctr,
              data = stacked,
              subset = raceban1 == "Non-Hispanic White",
              family = binomial(link = "probit"))
summary(m10.tot)

m10.bo <- glm(as.factor(race) ~ pid3*oreilly.prop14 + age 
           + fem + as.factor(educ) + south,
          weights = wtfctr,
          data = stacked,
          subset = raceban1 == "Non-Hispanic White",
          family = binomial(link = "probit"))
summary(m10.bo)

m10.rm <- glm(as.factor(race) ~ pid3*maddow.prop14 + age 
              + fem + as.factor(educ) + south,
              weights = wtfctr,
              data = stacked,
              subset = raceban1 == "Non-Hispanic White",
              family = binomial(link = "probit"))
summary(m10.rm)

m10.fe.tot <- glm(as.factor(race) ~ pid3*tot.prop14 + age 
               + fem + as.factor(educ) + south
               + as.factor(df),
               weights = wtfctr,
               data = stacked,
               subset = raceban1 == "Non-Hispanic White",
               family = binomial(link = "probit"))
summary(m10.fe.tot)

m10.fe.bo <- glm(as.factor(race) ~ pid3*oreilly.prop14 + age 
              + fem + as.factor(educ) + south
              + as.factor(df),
              weights = wtfctr,
              data = stacked,
              subset = raceban1 == "Non-Hispanic White",
              family = binomial(link = "probit"))
summary(m10.fe.bo)

m10.fe.rm <- glm(as.factor(race) ~ pid3*maddow.prop14 + age 
              + fem + as.factor(educ) + south 
              + as.factor(df),
              weights = wtfctr,
              data = stacked,
              subset = raceban1 == "Non-Hispanic White",
              family = binomial(link = "probit"))
summary(m10.fe.rm)

# mean(mip.sum.race$maddow.prop14, na.rm = T)
# sd(mip.sum.race$maddow.prop14, na.rm = T)

## Does partisan coverage have independent effect
m11 <- glm(as.factor(race) ~ pid3 + tot.prop14
            + age + fem + as.factor(educ) + south,
                weights = wtfctr,
                data = stacked,
                subset = raceban1 == "Non-Hispanic White",
                family = binomial(link = "probit"))
summary(m11)

m11.fe <- glm(as.factor(race) ~ pid3 + tot.prop14
           + age + fem + as.factor(educ) + south
           + as.factor(df),
           weights = wtfctr,
           data = stacked,
           subset = raceban1 == "Non-Hispanic White",
           family = binomial(link = "probit"))
summary(m11.fe)

m11.rm <- glm(as.factor(race) ~ pid3 + maddow.prop14
                + age + fem + as.factor(educ) + south,
                weights = wtfctr,
                data = stacked,
                subset = raceban1 == "Non-Hispanic White",
                family = binomial(link = "probit"))
summary(m11.rm)

m11.fe.rm <- glm(as.factor(race) ~ pid3 + maddow.prop14
                   + age + fem + as.factor(educ) + south 
                   + as.factor(df),
                   weights = wtfctr,
                   data = stacked,
                   subset = raceban1 == "Non-Hispanic White",
                   family = binomial(link = "probit"))
summary(m11.fe.rm)

# econ, unemp, enviro
m11.fe.rm.plac <- glm(as.factor(enviro) ~ pid3 + maddow.prop14
                        + age + fem + as.factor(educ) + south 
                        + as.factor(df),
                        weights = wtfctr,
                        data = stacked,
                        subset = raceban1 == "Non-Hispanic White",
                        family = binomial(link = "probit"))
summary(m11.fe.rm.plac)


m11.bo <- glm(as.factor(race) ~ pid3 + oreilly.prop14 
                + age + fem + as.factor(educ) + south,
                weights = wtfctr,
                data = stacked,
                subset = raceban1 == "Non-Hispanic White",
                family = binomial(link = "probit"))
summary(m11.bo)

m11.fe.bo <- glm(as.factor(race) ~ pid3  + oreilly.prop14 
                   + age + fem + as.factor(educ) + south 
                   + as.factor(df),
                   weights = wtfctr,
                   data = stacked,
                   subset = raceban1 == "Non-Hispanic White",
                   family = binomial(link = "probit"))
summary(m11.fe.bo)

# econ, unemp, enviro
m11.fe.bo.plac <- glm(as.factor(enviro) ~ pid3 + oreilly.prop14 
                        + age + fem + as.factor(educ) + south 
                        + as.factor(df),
                        weights = wtfctr,
                        data = stacked,
                        subset = raceban1 == "Non-Hispanic White",
                        family = binomial(link = "probit"))
summary(m11.fe.bo.plac)


m11.rmbo <- glm(as.factor(race) ~ pid3 + maddow.prop14 + oreilly.prop14 
                   + age + fem + as.factor(educ) + south,
                   weights = wtfctr,
                   data = stacked,
                   subset = raceban1 == "Non-Hispanic White",
                   family = binomial(link = "probit"))
summary(m11.rmbo)

m11.fe.rmbo <- glm(as.factor(race) ~ pid3 + maddow.prop14 + oreilly.prop14 
                   + age + fem + as.factor(educ) + south 
                 + as.factor(df),
                 weights = wtfctr,
                 data = stacked,
                 subset = raceban1 == "Non-Hispanic White",
                 family = binomial(link = "probit"))
summary(m11.fe.rmbo)
m11.fe.rmbo.se <- sqrt(diag(vcovHC(m11.fe.rmbo, "HC0")))
coeftest(m11.fe.rmbo, vcov = vcovHC(m11.fe.rmbo, "HC0"))

# econ, unemp, enviro
m11.fe.rmbo.plac <- glm(as.factor(econ) ~ pid3 + maddow.prop14 + oreilly.prop14 
                   + age + fem + as.factor(educ) + south 
                   + as.factor(df),
                   weights = wtfctr,
                   data = stacked,
                   subset = raceban1 == "Non-Hispanic White",
                   family = binomial(link = "probit"))
summary(m11.fe.rmbo.plac)

#bptest

#----------------------------------------------------------------------------------------------------#
# predicted probabilities

mode.fx <- function(var){
  m <- names(which.max(table(var)))
  return(m)
}

p <- seq(min(stacked$maddow.prop14, na.rm = T), 
         max(stacked$maddow.prop14, na.rm = T), 
         by = .001)
testdat <- stacked[1:length(p), c("south", "educ", "pid3", "fem", "age",
                                  "df", "race", "maddow.prop14", "oreilly.prop14")]

for(i in 1:length(p)){
  testdat$south[i] <- mode.fx(stacked$south[which(stacked$raceban1 == "Non-Hispanic White")])
  testdat$educ[i] <- mode.fx(stacked$educ[which(stacked$raceban1 == "Non-Hispanic White")])
  testdat$pid3[i] <- "Dem"
  testdat$fem[i] <- mode.fx(stacked$fem[which(stacked$raceban1 == "Non-Hispanic White")])
  testdat$age[i] <- mean(stacked$age[which(stacked$raceban1 == "Non-Hispanic White")], na.rm = T)
  testdat$df[i] <- mode.fx(stacked$df[which(stacked$raceban1 == "Non-Hispanic White")])
  testdat$race[i] <- NA
  testdat$maddow.prop14[i] <- p[i] #mean(stacked$maddow.prop14[which(stacked$raceban1 == "Non-Hispanic White")], na.rm = T)
  testdat$oreilly.prop14[i] <- mean(stacked$oreilly.prop14[which(stacked$raceban1 == "Non-Hispanic White")], na.rm = T)
}


head(testdat)
testdat <- cbind(testdat[,c(3,6)], apply(testdat[,-c(3,6)], 2, as.numeric))
testdat$pid3 <- as.factor(testdat$pid3)

predicted.rms <- predict(m11.fe.rmbo, testdat, se.fit=TRUE, type = "response")

p.rms <- p
p.bo <- p

# pdf("./figures/indiv_show_raceMIP.pdf")
plot(seq(0,.2, length.out = 10), seq(0,.2,length.out = 10), type = "n",
     xlab = "Proportion of Show Coverage Mentioning Race",
     ylab = "Predicted Probability",
     main = "Relationship between Show Coverage\nand Seeing Race as the Most Important Problem",
     axes = F)
lines(p.rms, predicted.rms$fit, col = "navy", lwd = 2)
lines(p.bo, predicted.bo$fit, col = "darkorange1", lwd = 2)
rug(unique(stacked$maddow.prop14), col = "navy")
rug(unique(stacked$oreilly.prop14), col = "darkorange1", side = 3)
legend("topright", c("Maddow", "O'Reilly"), fill = c("navy", "darkorange1"), bty = "n")
axis(1); axis(2, las = 1)
# dev.off()
#----------------------------------------------------------------------------------------------------#
## Plotting Trends
#----------------------------------------------------------------------------------------------------#
ggplot(mip.sum.race, aes(int.max, race.prop.weighted)) +
  geom_line(aes(color = Race), lwd = 1) +
  scale_color_manual(values = c("navy", "darkorange1")) +
  theme_bw() +
  labs(title = "Race as Country's Most Important Problem",
       subtitle = "By race. Gallup Data",
       x = "Date",
       y = "Percentage")

ggplot(subset(mip.sum.white.pid, Party != "Ind"), 
       aes(int.max, race.prop.weighted)) +
  geom_line(aes(color = Party), lwd = 1) +
  scale_color_manual(values = c("navy", "firebrick")) +
  theme_bw() +
  labs(title = "Race as Country's Most Important Problem",
       subtitle = "Non-Hispanic Whites, by party. Gallup Data",
       x = "Date",
       y = "Percentage")


