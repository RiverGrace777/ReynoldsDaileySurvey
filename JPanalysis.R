# load in data
setwd("/Users/rivergreynolds/Desktop/Spring2023/SPI404C/Survey")
FakeWD <- read.csv('RealData.csv', header = TRUE)

# remove subjects who took less than 4 minutes to complete the survey
FakeWD$Duration..in.seconds. <- as.integer(FakeWD$Duration..in.seconds.)
FakeWD <- FakeWD[which(FakeWD$Duration..in.seconds.>120),]

# change "" to NA
FakeWD <- replace(FakeWD, FakeWD=='', NA)

library(tidyr)
library(dplyr)
library(tidyverse)
library(gplots)

##################################
## INDEPENDENT VARIABLES
##################################

## GENDER
FakeWD$GenderIdentity <- factor(FakeWD$GenderIdentity, levels = c("Female", "Male", "Non-binary", "Other"))
FakeWD$Gender <- c()
FakeWD$Gender <- as.numeric(FakeWD$GenderIdentity)

## RACE
FakeWD$RaceInt <- c()
for (x in 1:nrow(FakeWD)) {
  if (grepl("Black", FakeWD$RaceEthnicity[x]) && grepl("White", FakeWD$RaceEthnicity[x])) {
    FakeWD$RaceInt[x] <- 3
  }
  else if (grepl("Black", FakeWD$RaceEthnicity[x])) {
    FakeWD$RaceInt[x] <- 1
  }
  else if (grepl("White", FakeWD$RaceEthnicity[x])) {
    FakeWD$RaceInt[x] <- 2
  }
  else {
    FakeWD$RaceInt[x] <- 4
  }
}

## SELF STIGMA
FakeWD$Experience <- c()
FakeWD$Experience <- coalesce(FakeWD$SelfStigma.Con, FakeWD$SelfStigma.BW, FakeWD$SelfStigma.WW, 
                          FakeWD$SelfStigma.WM, FakeWD$SelfStigma.BM, FakeWD$PSelfStigma.UnP)
FakeWD$Experience <- factor(FakeWD$Experience, levels =  c("Strongly disagree", 
                                                   "Disagree", "Somewhat disagree", 
                                                   "Somewhat agree", "Agree", 
                                                   "Strongly agree"))
FakeWD$Experience <- as.numeric(FakeWD$Experience)

## INCOME
FakeWD$IncomeLev <- factor(FakeWD$Income, levels = c("Less than $10,000", "$10,000–30,000", 
                                                     "$30,000–50,000", "$50,000–75,000", 
                                                     "$75,000–100,000", "$100,000 –$200,000", 
                                                     "Over $200,000", "I do not know"))

## POLITICAL PARTY
FakeWD$PolParty <- NA
for (x in 1:length(FakeWD$Party)) {
  if (grepl("republican", FakeWD$Party[x], ignore.case = TRUE)) {
    FakeWD$PolParty[x] <- 1
  }
  if (grepl("democrat", FakeWD$Party[x], ignore.case = TRUE)) {
    FakeWD$PolParty[x] <- 2
  }
}

##################################
## OUTCOMES
##################################

## TREATMENT GROUP
FakeWD$treatment <- c()
FakeWD$treatment <- rep(NA, nrow(FakeWD))
for (x in 1:nrow(FakeWD)) {
  if (!is.na(FakeWD$SelfStigma.UnP[x])) {
    FakeWD$treatment[x] <- 1
  }
  else if (!is.na(FakeWD$SelfStigma.BW[x])) {
    FakeWD$treatment[x] <- 2
  }
  else if (!is.na(FakeWD$SelfStigma.WW[x])) {
    FakeWD$treatment[x] <- 3
  }
  else if (!is.na(FakeWD$SelfStigma.BM[x])) {
    FakeWD$treatment[x] <- 4
  }
  else if (!is.na(FakeWD$SelfStigma.WM[x])) {
    FakeWD$treatment[x] <- 5
  }
  else if (!is.na(FakeWD$SelfStigma.Con[x])) {
    FakeWD$treatment[x] <- 6
  }
}

#### FAIRNESS
FakeWD$Fair <- c()
FakeWD$Fair <- coalesce(FakeWD$Fairness.Con, FakeWD$Fairness.BW, FakeWD$Fairness.WW, 
                          FakeWD$Fairness.WM, FakeWD$Fairness.BM, FakeWD$Fairness.UnP)
FakeWD$Fair <- factor(FakeWD$Fair, levels =  c("Strongly disagree", 
                                               "Disagree", "Somewhat disagree", 
                                               "Somewhat agree", "Agree", 
                                               "Strongly agree"))
FakeWD$Fair <- as.numeric(FakeWD$Fair)

## SEE SELF
FakeWD$SeeSelf <- c()
FakeWD$SeeSelf <- coalesce(FakeWD$SeeSelf.Con, FakeWD$SeeSelf.BW, FakeWD$SeeSelf.WW, 
                        FakeWD$SeeSelf.WM, FakeWD$SeeSelf.BM, FakeWD$SeeSelf.UnP)
FakeWD$SeeSelf <- factor(FakeWD$SeeSelf, levels =  c("Strongly disagree", 
                                               "Disagree", "Somewhat disagree", 
                                               "Somewhat agree", "Agree", 
                                               "Strongly agree"))
FakeWD$SeeSelf <- as.numeric(FakeWD$SeeSelf)

## WARMTH
FakeWD$Warmth <- c()
FakeWD$Warmth <- coalesce(FakeWD$Warmth.Con, FakeWD$Warmth.BW, FakeWD$Warmth.WW, 
                           FakeWD$Warmth.WM, FakeWD$Warmth.BM, FakeWD$Warmth.UnP)
FakeWD$Warmth <- factor(FakeWD$Warmth, levels =  c("Very cold", 
                                                     "Cold", "Somewhat cold", 
                                                     "Somewhat warm", "Warm", 
                                                     "Very warm"))
FakeWD$Warmth <- as.numeric(FakeWD$Warmth)

###  EMPATHY SCALE
## Empathy measures
FakeWD$EmpScale <- c()
FakeWD$EmpScale <- (FakeWD$Warmth + FakeWD$SeeSelf) / 2

## Identity
# Shared race
FakeWD$Identity.R <- c()
for (x in 1:nrow(FakeWD)) {
  # if you're Black and assigned to treatments with Black people (2 and 4), racial identity shared = 1
  if (FakeWD$RaceInt[x] == 1) {
    if (FakeWD$treatment[x] == 2 | FakeWD$treatment[x] == 4) {
      FakeWD$Identity.R[x] <- 1
    }
    else {
      FakeWD$Identity.R[x] <- 0
    }
  }
  # if you're White and assigned to treatments with white people (3 and 5), racial identity shared = 1
  if (FakeWD$RaceInt[x] == 2) { 
    if (FakeWD$treatment[x] == 3 | FakeWD$treatment[x] == 5) {
      FakeWD$Identity.R[x] <- 1
    }
    else {
      FakeWD$Identity.R[x] <- 0
    }
  }
  # if you're mixed and assigned to any treatment, identity shared = 1
  if (FakeWD$RaceInt[x] == 3) { 
    if (FakeWD$treatment[x] == 2 | FakeWD$treatment[x] == 3 | FakeWD$treatment[x] == 4 | FakeWD$treatment[x] == 5) {
      FakeWD$Identity.R[x] <- 1
    }
    else {
      FakeWD$Identity.R[x] <- 0
    }
  }
  # if you're a person of color and assigned to treatments with Black people, identity shared = 2
  if (FakeWD$RaceInt[x] == 4) {
    if (FakeWD$RaceInt[x] == 1) {
      if (FakeWD$treatment[x] == 2 | FakeWD$treatment[x] == 4) {
        FakeWD$Identity.R[x] <- 2
      }
      else {
        FakeWD$Identity.R[x] <- 0
      }
    }
  }
}

# Shared gender
FakeWD$Identity.G <- c()
for (x in 1:length(FakeWD)) {
  # if you're a woman
  if (FakeWD$Gender[x] == 1) {
    # and you're assigned to treatments with women, then you have shared identity
    if (FakeWD$treatment[x] == 2 | FakeWD$treatment[x] == 3) {
      FakeWD$Identity.G[x] <- 1
    }
    # if you're in treatments with men, no shared identity
    else {
      FakeWD$Identity.G[x] <- 0
    }
  }
  # if you're a man
  if (FakeWD$Gender[x] == 2) {
    # and assigned to treatments with men, shared identity
    if (FakeWD$treatment[x] == 4 | FakeWD$treatment[x] == 5) {
      FakeWD$Identity.G[x] <- 1
    }
    # if you're in treatments with women, no shared identity
    else {
      FakeWD$Identity.G[x] <- 0
    }
  }
  else {
    FakeWD$Identity.G[x] <- 0
  }
}

# Shared identity scale
FakeWD$Identity <- FakeWD$Identity.G + FakeWD$Identity.R

# identity + measures
FakeWD$EmpAll <- c()
FakeWD$EmpAll <- FakeWD$Identity + FakeWD$EmpScale

## DISCRIMINATION
FakeWD$Dis <- c()
FakeWD$Dis <- coalesce(FakeWD$YesDiscrim.Con, FakeWD$YesDiscrim.BW, FakeWD$YesDiscrim.WW, 
                       FakeWD$YesDiscrim.BM, FakeWD$YesDiscrim.WM, FakeWD$YesDiscrim.UnP)
FakeWD$Dis <- factor(FakeWD$Dis, levels =  c("Strongly disagree", 
                                                     "Disagree", "Somewhat disagree", 
                                                     "Somewhat agree", "Agree", 
                                                     "Strongly agree"))
FakeWD$Dis <- as.numeric(FakeWD$Dis)

## WHY DISCRIMINATION
FakeWD$Why <- c()
FakeWD$Why <- coalesce(FakeWD$WhyDiscrim.Con, FakeWD$WhyDiscrim.BW, FakeWD$WhyDiscrim.WW, 
                       FakeWD$WhyDiscrim.WM, FakeWD$WhyDiscrim.BM, FakeWD$WhyDiscrim.UnP)

FakeWD$WhyDiscrim <- c()
for (x in 1:nrow(FakeWD)) {
  if (is.na(FakeWD$Why[x])) {
    FakeWD$WhyDiscrim[x] <- NA
  }
  else if (FakeWD$Why[x] == "Weight/body size") {
    FakeWD$WhyDiscrim[x] <- 1
  }
  else if (grepl("Weight/body size", FakeWD$Why[x])=="TRUE") {
    FakeWD$WhyDiscrim[x] <- 1
  }
  else {
    FakeWD$WhyDiscrim[x] <- 0
  }
}

## PERSONAL RESPONSIBILITY
FakeWD$Res <- c()
FakeWD$Res <- coalesce(FakeWD$Will.Con, FakeWD$Will.BW, FakeWD$Will.WW, 
                       FakeWD$Will.WM, FakeWD$Will.BM, FakeWD$Will.UnP)
FakeWD$Res <- factor(FakeWD$Res, levels =  c("Strongly disagree", 
                                             "Disagree", "Somewhat disagree", 
                                             "Somewhat agree", "Agree", 
                                             "Strongly agree"))
FakeWD$Res <- as.numeric(FakeWD$Res)

## EMPATHY LAW
FakeWD$EmpLaw <- c()
FakeWD$EmpLaw <- coalesce(FakeWD$EmpLaw.Con, FakeWD$Will.BW, FakeWD$EmpLaw.WW, 
                       FakeWD$EmpLaw.WM, FakeWD$EmpLaw.BM., FakeWD$EmpLaw.UnP)
FakeWD$EmpLaw <- factor(FakeWD$EmpLaw, levels =  c("Strongly disagree", 
                                             "Disagree", "Somewhat disagree", 
                                             "Somewhat agree", "Agree", 
                                             "Strongly agree"))
FakeWD$EmpLaw <- as.numeric(FakeWD$EmpLaw)

#### POLICY SUPPORT
FakeWD$Policy <- c()
FakeWD$Policy <- coalesce(FakeWD$Policy.Con, FakeWD$Policy.BW, FakeWD$Policy.WW, 
                          FakeWD$Policy.WM, FakeWD$Policy.BM, FakeWD$Policy.UnP)
FakeWD$Policy <- factor(FakeWD$Policy, levels =  c("Strongly oppose", 
                                                   "Oppose", "Somewhat oppose", 
                                                   "Somewhat support", "Support", 
                                                   "Strongly support"))
FakeWD$Policy <- as.numeric(FakeWD$Policy)


##################################
## NEW DATA FRAME
##################################

srDat <- c()
srDat <- data.frame(FakeWD$Gender, FakeWD$RaceInt, FakeWD$IncomeLev, FakeWD$PolParty,
                    FakeWD$Experience, FakeWD$treatment, 
                    FakeWD$Fair, FakeWD$SeeSelf, FakeWD$Warmth, FakeWD$EmpScale,
                    FakeWD$Identity, FakeWD$EmpAll, FakeWD$Dis, FakeWD$WhyDiscrim, 
                    FakeWD$Res, FakeWD$EmpLaw, FakeWD$Policy)
colnames(srDat) <- c("Gender", "Race", "Income", "Party", "Experience", "Treatment", 
                     "Fair", "SeeSelf", "Warmth", "EmpMes", "Identity", "EmpAll", "Dis", "WhyDis", 
                     "Will", "EmpLaw", "Policy")

# subset data by race
RaceB <- c()
RaceW <- c()
RaceOR <- c()
RaceB <- srDat[which(srDat$Race==1),]
RaceW <- srDat[which(srDat$Race==2),]
RaceOR <- srDat[which(srDat$Race == 4),]

# subset data by gender
GenderF <- c()
GenderM <- c()
GenderO <- c()
GenderF <- srDat[which(srDat$Gender==1),]
GenderM <- srDat[which(srDat$Gender==2),]
GenderO <- srDat[which(srDat$Gender==3 | srDat$Gender ==4),]

## CREATE POLICY SUPPORT TABLE OF MEANS

# create confidence interval function
conf_int <- function(meane, stDev, n) {
  interval <- c()
  hold <-qt(0.975, n) * stDev / sqrt(n)
  min <- round(meane - hold, 2)
  max <- round(meane + hold, 2)
  interval <- paste("[", min, ",", max, "]", sep = "")
  return(interval)
}

# create function for table making
# function takes 2 vectors: one of the categories, another of data
# function outputs 3 vectors: 
#   one vector of observation counts
#   one vector of sorted means
#   one vector of sorted conf int
mn_sd <- function(interest, category) {
  # initialize vectors
  obs_vec <- c()
  means_vec <- c()
  sd_vec <- c()
  conf_int_vec <- c()
  new_rows <- c()
  # fill vectors
  obs_vec<- as.data.frame(table(category))[,2]
  means_vec <- round(tapply(interest, category, mean, na.rm = TRUE), 2)
  sd_vec <- round(tapply(interest, category, sd, na.rm = TRUE), 2)
  conf_int_vec <- conf_int(means_vec,sd_vec,obs_vec)
  new_rows <- rbind(means_vec, conf_int_vec, obs_vec)
  return(new_rows)
}

# create a function to append the average for the whole category 
# regardless of treatment group
total_dem <- function(outcome_vector) {
  holdMean <- round(mean(outcome_vector, na.rm = TRUE), 2)
  holdCI <- conf_int(holdMean, sd(outcome_vector, na.rm = TRUE), length(outcome_vector))
  holdN <- length(outcome_vector)
  allT <- c(holdMean, holdCI, holdN)
  return(allT)
}

# all
polT <- c()
polT <- mn_sd(srDat$Policy, srDat$Treatment)
polT <- cbind(polT, total_dem(srDat$Policy))

# by race
polB <- c()
polW <- c()
polOR <- c()
polB <- mn_sd(RaceB$Policy, RaceB$Treatment)
polB <- cbind(polB, total_dem(RaceB$Policy))
polW <- mn_sd(RaceW$Policy, RaceW$Treatment)
polW <- cbind(polW, total_dem(RaceW$Policy))
polOR <- mn_sd(RaceOR$Policy, RaceOR$Treatment)
polOR <- cbind(polOR, total_dem(RaceOR$Policy))

# by gender
polF <- c()
polM <- c()
polGO <- c()
polF <- mn_sd(GenderF$Policy, GenderF$Treatment)
polF <- cbind(polF, total_dem(GenderF$Policy))
polM <- mn_sd(GenderM$Policy, GenderM$Treatment)
polM <- cbind(polM, total_dem(GenderM$Policy))

# by income level
polI1 <- c()
polI1 <- mn_sd(srDat$Policy[which(srDat$Income == "Less than $10,000")], 
               srDat$Treatment[which(srDat$Income == "Less than $10,000")])
polI1 <- cbind(polI1, total_dem(srDat$Policy[which(srDat$Income == "Less than $10,000")]))

polI2 <- c()
polI2 <- mn_sd(srDat$Policy[which(srDat$Income == "$10,000–30,000")], 
               srDat$Treatment[which(srDat$Income == "$10,000–30,000")])
polI2 <- cbind(polI2, total_dem(srDat$Policy[which(srDat$Income == "$10,000–30,000")]))

polI3 <- c()
polI3 <- mn_sd(srDat$Policy[which(srDat$Income == "$30,000–50,000")], 
               srDat$Treatment[which(srDat$Income == "$30,000–50,000")])
polI3 <- cbind(polI3, total_dem(srDat$Policy[which(srDat$Income == "$30,000–50,000")]))

polI4 <- c()
polI4 <- mn_sd(srDat$Policy[which(srDat$Income == "$50,000–75,000")], 
               srDat$Treatment[which(srDat$Income == "$50,000–75,000")])
polI4 <- cbind(polI4, total_dem(srDat$Policy[which(srDat$Income == "$50,000–75,000")]))

polI5 <- c()
polI5 <- mn_sd(srDat$Policy[which(srDat$Income == "$75,000–100,000")], 
               srDat$Treatment[which(srDat$Income == "$75,000–100,000")])
polI5 <- cbind(polI5, total_dem(srDat$Policy[which(srDat$Income == "$75,000–100,000")]))

polI6 <- c()
polI6 <- mn_sd(srDat$Policy[which(srDat$Income == "$100,000 –$200,000")], 
               srDat$Treatment[which(srDat$Income == "$100,000 –$200,000")])
polI6 <- cbind(polI6, total_dem(srDat$Policy[which(srDat$Income == "$100,000 –$200,000")]))

polI7 <- c()
polI7 <- mn_sd(srDat$Policy[which(srDat$Income == "Over $200,000")], 
               srDat$Treatment[which(srDat$Income == "Over $200,000")])
polI7 <- cbind(polI7, total_dem(srDat$Policy[which(srDat$Income == "Over $200,000")]))

# by political party
polDem <- c()
polRep <- c()
polDem <- mn_sd(srDat$Policy[which(srDat$Party == 1)], srDat$Treatment[which(srDat$Party == 1)])
polDem <- cbind(polDem, total_dem(srDat$Policy[which(srDat$Party == 1)]))
polRep <- mn_sd(srDat$Policy[which(srDat$Party == 2)], srDat$Treatment[which(srDat$Party == 2)])
polRep <- cbind(polRep, total_dem(srDat$Policy[which(srDat$Party == 2)]))

# create table of observations, means, confidence intervals
polMeans <- c()
polMeans <- rbind(polT, polB, polW, polOR, polF, polM, polDem, polRep, 
                  polI1, polI2, polI3, polI4, polI5, polI6, polI7)
rownames(polMeans) <- c("All", "", "", 
                        "Race", "", "", "", "", "", "", "", "", 
                        "Gender", "",  "",  "", "", "",
                        "Political Party", "", "",  "", "", "",
                        "Income Level", "", "", "", "", "", "", "", "", "", "","", "", "", "", "", "", "", "", "", "")
categories <- c("All", "", "", 
               "Black", "", "", "White", "", "", "Other race", "", "",
               "Female", "", "", "Male", "", "",
               "Democrat", "", "", "Republican", "", "",
               "Less than $10,000", "", "", "10,000 to 30,000", "", "",
               "$30,000 to 50,000", "", "", "$50,000 to 75,000", "", "", 
               "$75,000 to 100,000", "", "", "$100,000 to $200,000", "", "", 
               "Over $200,000", "", "")
polMeans <- cbind(categories, polMeans)
polMeans <- as.data.frame(polMeans)
write.csv(polMeans, "polMeans.csv")

##################################
## INTERSECTING IDENTITIES POLICY TABLE
##################################

# Black Woman
blackWoman <- c()
blackWoman <- mn_sd(RaceB$Policy[which(RaceB$Gender == 1)], RaceB$Treatment[which(RaceB$Gender == 1)])
col_order <- c("2", "4", "3", "5", "6", "1")
blackWoman <- blackWoman[, col_order]

# Black Man
blackMan <- c()
blackMan <- mn_sd(RaceB$Policy[which(RaceB$Gender == 2)], RaceB$Treatment[which(RaceB$Gender == 2)])
six <- c(NA, NA, NA)
blackMan <- cbind(blackMan, six)
col_order <- c("2", "4", "3", "5", "six", "1")
blackMan <- blackMan[, col_order]

# White Woman
whiteWoman <- c()
whiteWoman <- mn_sd(RaceW$Policy[which(RaceW$Gender == 1)], RaceW$Treatment[which(RaceW$Gender == 1)])
col_order <- c("2", "4", "3", "5", "6", "1")
whiteWoman <- whiteWoman[, col_order]

# White Man
whiteMan <- c()
whiteMan <- mn_sd(RaceW$Policy[which(RaceW$Gender == 2)], RaceW$Treatment[which(RaceW$Gender == 2)])
col_order <- c("2", "4", "3", "5", "6", "1")
whiteMan <- whiteMan[, col_order]

# Create dataframe
identityPolMeans <- c()
identityPolMeans <- rbind(blackWoman, blackMan, whiteWoman, whiteMan)
identityPolMeans <- as.data.frame(identityPolMeans)
write.csv(identityPolMeans, "identityPolMeans.csv")

##################################
## INTERSECTING IDENTITIES EMP LAW TABLE
##################################

# Black Woman
blackWoman <- c()
blackWoman <- mn_sd(RaceB$EmpLaw[which(RaceB$Gender == 1)], RaceB$Treatment[which(RaceB$Gender == 1)])
col_order <- c("2", "4", "3", "5", "6", "1")
blackWoman <- blackWoman[, col_order]

# Black Man
blackMan <- c()
blackMan <- mn_sd(RaceB$EmpLaw[which(RaceB$Gender == 2)], RaceB$Treatment[which(RaceB$Gender == 2)])
six <- c(NA, NA, NA)
blackMan <- cbind(blackMan, six)
col_order <- c("2", "4", "3", "5", "six", "1")
blackMan <- blackMan[, col_order]

# White Woman
whiteWoman <- c()
whiteWoman <- mn_sd(RaceW$EmpLaw[which(RaceW$Gender == 1)], RaceW$Treatment[which(RaceW$Gender == 1)])
col_order <- c("2", "4", "3", "5", "6", "1")
whiteWoman <- whiteWoman[, col_order]

# White Man
whiteMan <- c()
whiteMan <- mn_sd(RaceW$EmpLaw[which(RaceW$Gender == 2)], RaceW$Treatment[which(RaceW$Gender == 2)])
col_order <- c("2", "4", "3", "5", "6", "1")
whiteMan <- whiteMan[, col_order]

# Create dataframe
identityLawEmpMeans <- c()
identityLawEmpMeans <- rbind(blackWoman, blackMan, whiteWoman, whiteMan)
identityLawEmpMeans <- as.data.frame(identityLawEmpMeans)
write.csv(identityLawEmpMeans, "identityLawEmpMeans.csv")

##################################
## PLOTS
##################################

## POLICY SUPPORT
# set up one figure to contain all plots
par(mfrow=c(3,2))

#plot mean policy support by treatment group for white people
plotmeans(Policy ~ Treatment, data = RaceW, frame = FALSE, 
          connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
          ylab = "Policy Support", xlab = "Treatment Group", ylim=c(0, 6),
          main = "White Respondents", xaxt = "n")
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man", "Control"), at = 1:6, 
                               srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty = 2)
abline(h = 0, lty = 1)

# plot mean policy support by treatment group for black people
plotmeans(Policy ~ Treatment, data = RaceB, frame = FALSE, 
          connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
          ylab = "Policy Support", xlab = "Treatment Group", ylim=c(0, 6),
          main = "Black Respondents", xaxt = "n", maxbar = 6)
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man", "Control"), at = 1:6, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty = 2)
abline(h = 0, lty = 1)

# plot mean policy support by treatment group for women
plotmeans(Policy ~ Treatment, data = GenderF, frame = FALSE, 
          connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
          ylab = "Policy Support", xlab = "Treatment Group", ylim=c(0, 6),
          main = "Female Respondents", xaxt = "n", maxbar = 6)
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man", "Control"), at = 1:6, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty = 2)
abline(h = 0, lty = 1)

# plot mean policy support by treatment group for men
plotmeans(Policy ~ Treatment, data = GenderM, frame = FALSE, 
          connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
          ylab = "Policy Support", xlab = "Treatment Group", ylim=c(0, 6),
          main = "Male Respondents", xaxt = "n", maxbar = 6)
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man", "Control"), at = 1:6, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty = 2)
abline(h = 0, lty = 1)

# plot mean policy support by treatment group
plotmeans(Policy ~ Treatment, data = srDat, frame = FALSE, 
          connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
          ylab = "Policy Support", xlab = "Treatment Group", ylim=c(0, 6),
          main = "All Respondents", xaxt = "n")
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man", "Control"), at = 1:6, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty = 2)
abline(h = 0, lty = 1)

# PLOT ABILITY TO RECOGNIZE WEIGHT DISCRIMINATION

par(mfrow=c(3,2))

# plot black people's ability to recognize weight based discrimination
whyDisMeanPlotB <- plotmeans(WhyDis ~ Treatment, data = RaceB, frame = FALSE, 
                            connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                            ylab = "Policy Support", xlab = "Treatment Group", 
                            main = "Black Respondents", error.bars = "conf.int",
                            ylim=c(0, 1), xaxt="n", barcol = "black", minbar = 0, maxbar = 1)
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 0.5, lty=2)
abline(h = 0, lty = 1)
abline(h = 0, lty = 1)

# plot white people's ability to recognize weight based discrimination
whyDisMeanPlotW <- plotmeans(WhyDis ~ Treatment, data = RaceW, frame = FALSE, 
                            connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                            ylab = "Policy Support", xlab = "Treatment Group", 
                            main = "White Respondents", error.bars = "conf.int",
                            ylim=c(0, 1), xaxt="n", barcol = "black", minbar = 0, maxbar = 1)
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 0.5, lty=2)
abline(h = 0, lty = 1)
abline(h = 0, lty = 1)

# plot men's ability to recognize weight based discrimination
whyDisMeanPlotM <- plotmeans(WhyDis ~ Treatment, data = GenderM, frame = FALSE, 
                            connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                            ylab = "Policy Support", xlab = "Treatment Group", 
                            main = "Men", error.bars = "conf.int",
                            ylim=c(0, 1), xaxt="n", barcol = "black", minbar = 0, maxbar = 1)
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 0.5, lty=2)
abline(h = 0, lty = 1)
abline(h = 0, lty = 1)

# plot women's ability to recognize weight based discrimination
whyDisMeanPlotF <- plotmeans(WhyDis ~ Treatment, data = GenderF, frame = FALSE, 
                            connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                            ylab = "Policy Support", xlab = "Treatment Group", 
                            main = "Women", error.bars = "conf.int",
                            ylim=c(0, 1), xaxt="n", barcol = "black", minbar = 0, maxbar = 1)
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 0.5, lty=2)
abline(h = 0, lty = 1)
abline(h = 0, lty = 1)

# plot ability to recognize weight based discrimination by treatment group
whyDisMeanPlot <- plotmeans(WhyDis ~ Treatment, data = srDat, frame = FALSE, 
                            connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                            ylab = "No to Yes", xlab = "Treatment Group", 
                            main = "Was the person in the passage discriminated against 
                            on the basis of weight/body size?", error.bars = "conf.int",
                            ylim=c(0, 1), xaxt="n", barcol = "black")
angleAxis(side = 1, labels = c("Unidentified", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 0.5, lty=2)
abline(h = 0, lty = 1)

par(mfrow = c(1,1))


# PLOT RECOGNITION OF DISCRIMINATION
srDat$DisBinary <- c()
for (x in 1:nrow(srDat)) {
  if (is.na(srDat$Dis[x]) == TRUE) {
    srDat$DisBinary[x] <- NA
  }
  else if (srDat$Dis[x] >= 4) {
    srDat$DisBinary[x] <- 1
  }
  else {
    srDat$DisBinary[x] <- 0
  }
}

ifDisMeanPlot <- plotmeans(DisBinary ~ Treatment, data = srDat, frame = FALSE, 
                            connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                            ylab = "Was the person in the passage discriminated against?", 
                            xlab = "Treatment Group", error.bars = "conf.int",
                            main = "All Respondents",
                            ylim=c(0, 1), xaxt="n", barcol = "black")
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 0.5, lty=2)
abline(h = 0, lty = 1)

## PLOT FAIRNESS
fairMeanPlot <- plotmeans(Fair ~ Treatment, data = srDat, frame = FALSE, 
                            connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                            ylab = "Is What Happened Fair", xlab = "Treatment Group", 
                            main = "All Respondents", error.bars = "conf.int",
                            ylim=c(0, 6), xaxt="n", barcol = "black")
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty=2)
abline(h = 0, lty = 1)


## PLOT EMP LAW


## POLICY SUPPORT

# set up one figure to contain all plots
par(mfrow=c(3,2))

# should there be a law to protect __ by treatment group, black
empLawPlot <- plotmeans(EmpLaw ~ Treatment, data = RaceB, frame = FALSE, 
                        connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                        ylab = "Mean Support", xlab = "Treatment Group", 
                        main = "Black Respondents", error.bars = "conf.int",
                        ylim=c(0, 6), xaxt="n", barcol = "black")
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty=2)
abline(h = 0, lty = 1)

#plot mean policy support by treatment group for white people
empLawPlot <- plotmeans(EmpLaw ~ Treatment, data = RaceW, frame = FALSE, 
                        connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                        ylab = "Mean Support", xlab = "Treatment Group", 
                        main = "White Respondents", error.bars = "conf.int",
                        ylim=c(0, 6), xaxt="n", barcol = "black")
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty=2)
abline(h = 0, lty = 1)

# plot mean policy support by treatment group for women
# should there be a law to protect __ by treatment group
empLawPlot <- plotmeans(EmpLaw ~ Treatment, data = GenderF, frame = FALSE, 
                        connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                        ylab = "Mean Support", xlab = "Treatment Group", 
                        main = "Female Respondents", error.bars = "conf.int",
                        ylim=c(0, 6), xaxt="n", barcol = "black")
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty=2)
abline(h = 0, lty = 1)

# should there be a law to protect __ by men
empLawPlot <- plotmeans(EmpLaw ~ Treatment, data = GenderM, frame = FALSE, 
                        connect = FALSE, mean.labels = FALSE, text.n.label = "N=", 
                        ylab = "Mean Support", xlab = "Treatment Group", 
                        main = "Men", error.bars = "conf.int",
                        ylim=c(0, 6), xaxt="n", barcol = "black")
angleAxis(side = 1, labels = c("Unidentified Person", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty=2)
abline(h = 0, lty = 1)

par(mfrow = c(1,1))

# should there be a law to protect __ by treatment group
empLawPlot <- plotmeans(EmpLaw ~ Treatment, data = srDat, frame = FALSE, 
                          connect = FALSE, mean.labels = FALSE,
                          ylab = "Mean Support", xlab = "Treatment Group", 
                          main = "All Respondents", 
                          error.bars = "conf.int", barwidth = 1.2,
                          ylim=c(0, 6), xaxt="n", barcol = "black")
angleAxis(side = 1, labels = c("Unidentified", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty=2)
abline(h = 0, lty = 1)

# emp law white women respondents
empLawPlot <- plotmeans(EmpLaw ~ Treatment, data = RaceW[which(RaceW$Gender == 1),], frame = FALSE, 
                        connect = FALSE, mean.labels = FALSE,
                        ylab = "Mean Support", xlab = "Treatment Group", pch = 18,
                        error.bars = "conf.int", barcol = "blue", col="blue",
                        main = "White Woman Respondents", 
                        ylim=c(0, 6), xaxt="n")
angleAxis(side = 1, labels = c("Unidentified", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty=2)
abline(h = 0, lty = 1)

## PLOT WILL
willPlot <- plotmeans(Will ~ Treatment, data = srDat, Race,
                        connect = FALSE, mean.labels = FALSE, n.label = F,
                        ylab = "Is What Happened Fair", xlab = "Treatment Group", 
                        main = "Should There Be a Law to Protect?", error.bars = "conf.int",
                        ylim=c(0, 6), xaxt="n", barcol = "black", frame = F)
angleAxis(side = 1, labels = c("Unidentified", "Black Woman", 
                               "White Woman", "Black Man", "White Man"), at = 1:5, 
          srt = 0, adj =  0.5, xpd = TRUE)
abline(h = 3.5, lty=2)
abline(h = 0, lty = 1)

##################################
## RECOGNIZING DISCRIMINATION TABLE
##################################

# all
whyT <- c()
whyT <- mn_sd(srDat$WhyDis, srDat$Treatment)
whyT <- cbind(whyT, total_dem(srDat$WhyDis))

# by race
whyB <- c()
whyW <- c()
whyB <- mn_sd(RaceB$WhyDis, RaceB$Treatment)
whyB <- cbind(whyB, total_dem(RaceB$WhyDis))
whyW <- mn_sd(RaceW$WhyDis, RaceW$Treatment)
whyW <- cbind(whyW, total_dem(RaceW$WhyDis))

# by gender
whyF <- c()
whyM <- c()
whyF <- mn_sd(GenderF$WhyDis, GenderF$Treatment)
whyF <- cbind(whyF, total_dem(GenderF$WhyDis))
whyM <- mn_sd(GenderM$WhyDis, GenderM$Treatment)
whyM <- cbind(whyM, total_dem(GenderM$WhyDis))

whyMeans <- c()
whyMeans <- rbind(whyT, whyB, whyW, whyF, whyM)
rownames(whyMeans) <- c("All", "", "", 
                        "Race", "", "", "", "", "",
                        "Gender", "",  "",  "", "", "")
categories <- c("All", "", "", 
                "Black", "", "", "White", "", "",
                "Female", "", "", "Male", "", "")
whyMeans <- cbind(categories, whyMeans)
whyMeans <- as.data.frame(whyMeans)
whyMeans <- whyMeans[,-7]
write.csv(whyMeans, "whyMeans.csv")

##################################
## EMP LAW TABLE
##################################

# all
elT <- c()
elT <- mn_sd(srDat$EmpLaw, srDat$Treatment)
elT <- cbind(elT, total_dem(srDat$EmpLaw))

# by race
elB <- c()
elW <- c()
elB <- mn_sd(RaceB$EmpLaw, RaceB$Treatment)
elB <- cbind(elB, total_dem(RaceB$EmpLaw))
elW <- mn_sd(RaceW$EmpLaw, RaceW$Treatment)
elW <- cbind(elW, total_dem(RaceW$EmpLaw))

# by gender
elF <- c()
elM <- c()
elF <- mn_sd(GenderF$EmpLaw, GenderF$Treatment)
elF <- cbind(elF, total_dem(GenderF$EmpLaw))
elM <- mn_sd(GenderM$EmpLaw, GenderM$Treatment)
elM <- cbind(elM, total_dem(GenderM$EmpLaw))

elMeans <- c()
elMeans <- rbind(elT, elB, elW, elF, elM)
rownames(elMeans) <- c("All", "", "", 
                        "Race", "", "", "", "", "",
                        "Gender", "",  "",  "", "", "")
categories <- c("All", "", "", 
                "Black", "", "", "White", "", "",
                "Female", "", "", "Male", "", "")
elMeans <- cbind(categories, elMeans)
elMeans <- as.data.frame(elMeans)
elMeans <- elMeans[,-7]
write.csv(elMeans, "elMeans.csv")

# IDENTITY EMP MEANS TABLE

# Black Woman
blackWoman <- c()
blackWoman <- mn_sd(RaceB$EmpLaw[which(RaceB$Gender == 1)], RaceB$Treatment[which(RaceB$Gender == 1)])
col_order <- c("2", "4", "3", "5", "6", "1")
blackWoman <- blackWoman[, col_order]

# Black Man
blackMan <- c()
blackMan <- mn_sd(RaceB$EmpLaw[which(RaceB$Gender == 2)], RaceB$Treatment[which(RaceB$Gender == 2)])
six <- c(NA, NA, NA)
blackMan <- cbind(blackMan, six)
col_order <- c("2", "4", "3", "5", "six", "1")
blackMan <- blackMan[, col_order]

# White Woman
whiteWoman <- c()
whiteWoman <- mn_sd(RaceW$EmpLaw[which(RaceW$Gender == 1)], RaceW$Treatment[which(RaceW$Gender == 1)])
col_order <- c("2", "4", "3", "5", "6", "1")
whiteWoman <- whiteWoman[, col_order]

# White Man
whiteMan <- c()
whiteMan <- mn_sd(RaceW$EmpLaw[which(RaceW$Gender == 2)], RaceW$Treatment[which(RaceW$Gender == 2)])
col_order <- c("2", "4", "3", "5", "6", "1")
whiteMan <- whiteMan[, col_order]

# Create dataframe
identityELMeans <- c()
identityELMeans <- rbind(blackWoman, blackMan, whiteWoman, whiteMan)
identityELMeans <- as.data.frame(identityELMeans)
identityELMeans <- identityELMeans[,-5]
write.csv(identityELMeans, "identityELMeans.csv")


### CHECK EMPATHY CORRELATIONS
lm(Policy ~ EmpMes, data = srDat)
lm(Policy ~ Identity, data = srDat)
lm(Policy ~ EmpAll, data = FakeWD)

lm(EmpLaw ~ EmpMes, data = srDat)
lm(EmpLaw ~ Identity, data = srDat)
lm(EmpLaw ~ EmpAll, data = FakeWD)












