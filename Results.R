library(dplyr)
library(tidyverse)
library(stargazer)
library(rdrobust)
library(rdd)
library(lfe)
data<-read.csv("PaperFullDataFinal.csv")
dataWithPitlane<-read.csv("dataWithPitlane.csv")
data$Team <- as.factor(data$Team)
data$GP <- as.factor(data$GP)
dataWithPitlane$Team <- as.factor(dataWithPitlane$Team)
dataWithPitlane$GP <- as.factor(dataWithPitlane$GP)

#Outcomes 
#Grid Position (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$GridPosition, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(GridPosition ~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2  | Team:GP, data = data_filtered)
summary(model)
nobs(model)
rdplot(data_filtered$GridPosition, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(5, 20), 
       x.lim = c(-.5,.5), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "GridPosition")






#Grid Position (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$GridPosition, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(GridPosition ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
nobs(model)
rdplot(data_filtered$GridPosition, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(4, 16), 
       x.lim = c(-.4,.4), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "GridPosition")


stargazer::stargazer(model1,model2,header=F)

general_mean <- mean(data$GridPosition, na.rm = TRUE)

#Race Position (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$RacePosition, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(RacePosition ~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$RacePosition, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(4, 16), 
       x.lim = c(-1,1), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "Race Position")

#Race Position (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$RacePosition, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(RacePosition ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$RacePosition, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(4, 16), 
       x.lim = c(-.5,.5), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Race Position")

stargazer::stargazer(model1,model2,header=F)


#Points (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$Points, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(Points ~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$Points, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, 10), 
       x.lim = c(-1,1), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "Points")

#Points (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$Points, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(Points ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$Points, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, 10), 
       x.lim = c(-.4,.4), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Points")

stargazer::stargazer(model1,model2,header=F)




#DNF (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$DNF, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(DNF ~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$DNF, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, .5), 
       x.lim = c(-1,1), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "Probability of DNF")

#DNF (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$DNF, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(DNF ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$DNF, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, .5), 
       x.lim = c(-.6,.6), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Probability of DNF")

stargazer::stargazer(model1,model2,header=F)

#Alternative Strategy (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$AlternativeStrategy, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(AlternativeStrategy ~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$AlternativeStrategy, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(.5, 1), 
       x.lim = c(-1,1), 
       title = "",
       x.label = "GapToKnockoutQ1", 
       y.label = "Probability of Alternative Strategy")

#Alternative Strategy  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$AlternativeStrategy, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(AlternativeStrategy ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$AlternativeStrategy, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(.4, 1), 
       x.lim = c(-.6,.6), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Probability of AlternativeStrategy")

stargazer::stargazer(model1,model2,header=F)



#SOFT (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$TireSOFT, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(TireSOFT ~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TireSOFT, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, .5), 
       x.lim = c(-1,1), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "Probability of starting with Soft")

#SOFT  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$TireSOFT, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(TireSOFT ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TireSOFT, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, .6), 
       x.lim = c(-.6,.6), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Probability of starting with Soft")

stargazer::stargazer(model1,model2,header=F)

#MEDIUM (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$TireMEDIUM, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(TireMEDIUM ~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TireMEDIUM, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, 1), 
       x.lim = c(-1,1), 
       title = "",
       x.label = "GapToKnockoutQ1", 
       y.label = "Probability of starting with Medium")

#MEDIUM  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$TireMEDIUM, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(TireMEDIUM ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TireMEDIUM, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(.25, .9), 
       x.lim = c(-.6,.6), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Probability of starting with Medium")


stargazer::stargazer(model1,model2,header=F)



#HARD (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$TireHARD, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(TireHARD~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model1)
rdplot(data_filtered$TireHARD, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, .5), 
       x.lim = c(-1,1), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "Probability of starting with Hard")

#Hard  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$TireHARD, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(TireHARD ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TireHARD, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, .25), 
       x.lim = c(-.6,.6), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Probability of starting with Hard")

stargazer::stargazer(model1,model2,header=F)




#Number of pitstops (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$NumberOfPitStops, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(NumberOfPitStops~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$NumberOfPitStops, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(.5, 2), 
       x.lim = c(-1,1), 
       title = "",
       x.label = "GapToKnockoutQ1", 
       y.label = "Number of Pitstops")



#NumberOfPitStops  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$NumberOfPitStops, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(NumberOfPitStops ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$NumberOfPitStops, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(.5, 2), 
       x.lim = c(-.6,.6), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "NumberOfPitStops")

stargazer::stargazer(model1,model2,header=F)

#PITLANE (Q1)
cutoff <- 0 
bw <- rdbwselect(y = dataWithPitlane$Pitlane, x = dataWithPitlane$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- dataWithPitlane[dataWithPitlane$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                                   dataWithPitlane$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(Pitlane~ MadeItToQ2 + GapToKnockoutQ1 + 
                 GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model1)
rdplot(data_filtered$Pitlane, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0,.2), 
       x.lim = c(), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "Probability of Pitlane Start")



#Pitlane  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = dataWithPitlane$NumberOfPitStops, x = dataWithPitlane$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- dataWithPitlane[dataWithPitlane$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                                   dataWithPitlane$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(Pitlane ~ MadeItToQ3 + GapToKnockoutQ2 + 
                 GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model2)
rdplot(data_filtered$Pitlane, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0,.5), 
       x.lim = c(), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Probability of Pitlane Start")

stargazer::stargazer(model1,model2,header=F)

#DNFDRIVING (Q1)
cutoff <- 0 
bw <- rdbwselect(y = dataWithPitlane$DNFDriving, x = dataWithPitlane$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- dataWithPitlane[dataWithPitlane$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                                   dataWithPitlane$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(DNFDriving~ MadeItToQ2 + GapToKnockoutQ1 + 
                 GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model1)
rdplot(data_filtered$DNFDriving, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(), 
       x.lim = c(), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "Probabily of DNF (Driving)")



#DNFDriving  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = dataWithPitlane$DNFDriving, x = dataWithPitlane$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- dataWithPitlane[dataWithPitlane$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                                   dataWithPitlane$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(DNFDriving ~ MadeItToQ3 + GapToKnockoutQ2 + 
                 GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model2)
rdplot(data_filtered$DNFDriving, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(), 
       x.lim = c(), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Probabily of DNF (Driving)")

stargazer::stargazer(model1,model2,header=F)

#DNFMechanical (Q1)
cutoff <- 0 
bw <- rdbwselect(y = dataWithPitlane$DNFMechanical, x = dataWithPitlane$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- dataWithPitlane[dataWithPitlane$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                                   dataWithPitlane$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model1 <- felm(DNFMechanical~ MadeItToQ2 + GapToKnockoutQ1 + 
                 GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model1)
rdplot(data_filtered$DNFMechanical, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0,.2), 
       x.lim = c(), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "Probabily of DNF (Mechanical)")



#DNFMechanical  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = dataWithPitlane$DNFMechanical, x = dataWithPitlane$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- dataWithPitlane[dataWithPitlane$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                                   dataWithPitlane$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model2 <- felm(DNFMechanical ~ MadeItToQ3 + GapToKnockoutQ2 + 
                 GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model2)
rdplot(data_filtered$DNFMechanical, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0,.15), 
       x.lim = c(), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "Probabily of DNF (Mechanical)")

stargazer::stargazer(model1,model2,header=F)








#COVARIATES 
#GAPTOFASTESTFP1 (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$GapToFastestFP1, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model <- felm(GapToFastestFP1~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)



options(repr.plot.width=10, repr.plot.height=6) # Ajusta según tus necesidades

rdplot(data_filtered$GapToFastestFP1, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, 10), 
       x.lim = c(-1.25,1.25), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "GapToFastestFP1")

#GAPTOFASTESTFP1  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$GapToFastestFP1, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model <- felm(GapToFastestFP1 ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$GapToFastestFP1, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, 5), 
       x.lim = c(-.5,.5), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "GapToFastestFP1")

#GAPTOFASTESTFP2 (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$GapToFastestFP2, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model <- felm(GapToFastestFP2~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$GapToFastestFP2, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, 5), 
       x.lim = c(-1.25,1.25), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "GapToFastestFP2")

#GAPTOFASTESTFP2  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$GapToFastestFP2, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model <- felm(GapToFastestFP2 ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$GapToFastestFP2, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, 4), 
       x.lim = c(-.3,.3), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "GapToFastestFP2")




#GAPTOFASTESTFP3 (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$GapToFastestFP3, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model <- felm(GapToFastestFP3~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$GapToFastestFP3, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, 4), 
       x.lim = c(-.4,.4), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "GapToFastestFP3")

#GAPTOFASTESTFP3  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$GapToFastestFP3, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model <- felm(GapToFastestFP3 ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$GapToFastestFP3, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, 3), 
       x.lim = c(-.5,.5), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "GapToFastestFP3")



#ChampionshipPoints (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$ChampionshipPoints, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model <- felm(ChampionshipPoints~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$ChampionshipPoints, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, 100), 
       x.lim = c(-.6,.6), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "ChampionshipPoints")

#ChampionshipPoints  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$ChampionshipPoints, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model <- felm(ChampionshipPoints ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$ChampionshipPoints, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, 150), 
       x.lim = c(-.5,.5), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "ChampionshipPoints")


#TeamChampionshipPoints (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$TeamChampionshipPoints, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model <- felm(TeamChampionshipPoints~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TeamChampionshipPoints, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(0, 200), 
       x.lim = c(-.75,.75), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "TeamChampionshipPoints")

#TeamChampionshipPoints  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$TeamChampionshipPoints, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model <- felm(TeamChampionshipPoints ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TeamChampionshipPoints, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(0, 250), 
       x.lim = c(-.5,.5), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "TeamChampionshipPoints")


#LastRacePosition (Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$LastRacePosition, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model <- felm(LastRacePosition~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$LastRacePosition, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(5, 20), 
       x.lim = c(-1,1), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "LastRacePosition")

#LastRacePosition  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$LastRacePosition, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model <- felm(LastRacePosition ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$LastRacePosition, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(5, 20), 
       x.lim = c(-.5,.5), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "LastRacePosition")

#TeammatesPosition(Q1)
cutoff <- 0 
bw <- rdbwselect(y = data$TeammateLastRacePosition, x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
model <- felm(TeammateLastRacePosition~ MadeItToQ2 + GapToKnockoutQ1 + 
              GapToKnockoutQ1:MadeItToQ2 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TeammateLastRacePosition, data_filtered$GapToKnockoutQ1, 
       c = 0, p = 1, 
       y.lim = c(5, 20), 
       x.lim = c(-1,1), 
       title = " ",
       x.label = "GapToKnockoutQ1", 
       y.label = "TeammateLastRacePosition")

#TeammateLastRacePosition  (Q2)
cutoff <- 0 
bw <- rdbwselect(y = data$TeammateLastRacePosition, x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
bandwidth <- bw$bws[1]  
data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                        data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
model <- felm(TeammateLastRacePosition ~ MadeItToQ3 + GapToKnockoutQ2 + 
              GapToKnockoutQ2:MadeItToQ3 | Team:GP, data = data_filtered)
summary(model)
rdplot(data_filtered$TeammateLastRacePosition, data_filtered$GapToKnockoutQ2, 
       c = 0, p = 1, 
       y.lim = c(5, 20), 
       x.lim = c(-.5,.5), 
       title = " ",
       x.label = "GapToKnockoutQ2", 
       y.label = "TeammateLastRacePosition")


model$coefficients[1]
results<-summary(model)


p_values <- results$coefficients[, "Pr(>|t|)"][1]
print(p_values)




library(lfe)  # para felm
library(rdrobust)  # para rdbwselect

run_models <- function(data) {
  # Crear listas para almacenar resultados
  results_q1 <- list()
  results_q2 <- list()
  
  # Definir las covariables
  covariates <- c("GapToFastestFP1", "GapToFastestFP2", "GapToFastestFP3", 
                  "ChampionshipPoints", "TeamChampionshipPoints", 
                  "LastRacePosition", "TeammateLastRacePosition")
  
  for (var in covariates) {
    # Modelo para MadeItToQ2 (Q1)
    cutoff <- 0
    bw <- rdbwselect(y = data[[var]], x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
    bandwidth <- bw$bws[1]
    data_filtered_q1 <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
    
    # Ajustar el modelo con covariables
    model_q1 <- felm(as.formula(paste(var, "~ MadeItToQ2 + GapToKnockoutQ1 | Team:GP")), data = data_filtered_q1)
    
    # Extraer resultados
    coefs_q1 <- summary(model_q1)$coefficients
    results_q1[[var]] <- data.frame(
      Covariate = var,
      Treatment = "MadeItToQ2",
      Estimate = coefs_q1["MadeItToQ2", "Estimate"],
      p_value = coefs_q1["MadeItToQ2", "Pr(>|t|)"],
      CI_lower = coefs_q1["MadeItToQ2", "Estimate"] - 1.96 * coefs_q1["MadeItToQ2", "Std. Error"],
      CI_upper = coefs_q1["MadeItToQ2", "Estimate"] + 1.96 * coefs_q1["MadeItToQ2", "Estimate"] + 1.96 * coefs_q1["MadeItToQ2", "Std. Error"],
      Observations = nrow(data_filtered_q1),
      Bandwidth = bandwidth
    )
    
    # Modelo para MadeItToQ3 (Q2)
    bw <- rdbwselect(y = data[[var]], x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
    bandwidth <- bw$bws[1]
    data_filtered_q2 <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
    
    # Ajustar el modelo con covariables
    model_q2 <- felm(as.formula(paste(var, "~ MadeItToQ3 + GapToKnockoutQ2 | Team:GP")), data = data_filtered_q2)
    
    # Extraer resultados
    coefs_q2 <- summary(model_q2)$coefficients
    results_q2[[var]] <- data.frame(
      Covariate = var,
      Treatment = "MadeItToQ3",
      Estimate = coefs_q2["MadeItToQ3", "Estimate"],
      p_value = coefs_q2["MadeItToQ3", "Pr(>|t|)"],
      CI_lower = coefs_q2["MadeItToQ3", "Estimate"] - 1.96 * coefs_q2["MadeItToQ3", "Std. Error"],
      CI_upper = coefs_q2["MadeItToQ3", "Estimate"] + 1.96 * coefs_q2["MadeItToQ3", "Std. Error"],
      Observations = nrow(data_filtered_q2),
      Bandwidth = bandwidth
    )
  }
  
  # Combinar resultados en dataframes
  df_results_q1 <- do.call(rbind, results_q1)
  df_results_q2 <- do.call(rbind, results_q2)
  
  return(list(MadeItToQ2 = df_results_q1, MadeItToQ3 = df_results_q2))
}

# Ejemplo de uso
resultados <- run_models(data)


ggplot(resultados$MadeItToQ2, aes(x = RD_Estimator, y = reorder(Variable, RD_Estimator))) +
  geom_point(color = "#1f78b4", size = 3) +  # Puntos para los estimadores
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2, color = "#1f78b4") +  # Barras de error horizontales
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Línea vertical en x = 0
  labs(
    title = "Análisis de Placebo con RD",
    x = "Estimador RD",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  # Quitar líneas de la cuadrícula horizontal
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 12),  # Aumentar tamaño del texto del eje y
    axis.title.x = element_text(size = 14)  # Aumentar tamaño del texto del título del eje x
  )














stargazer::stargazer(resultados$MadeItToQ3,summary = FALSE,header=F)

rdplot(data$RacePosition, data$GridPosition, 
       c = 10,                 # Punto de corte en 10
       p = 0,                  # Sin ajuste polinomial
       y.lim = c(0, 20), 
       x.lim = c(0, 20),      # Selección de bins con varianza ajustada en cuadrantes
       nbins = 200,
       col.lines = "white",# Aumenta el número de bins a cada lado del punto de corte
       title = " ", 
       x.label = "Grid Position", 
       y.label = "Race Position")

data_filtered <- data %>%
  filter(is.finite(GridPosition), is.finite(RacePosition))

# Definimos el ancho de los bins
binwidth <- 1  # Ajusta este valor según el tamaño de los bins que prefieras

# Agrupamos los datos en bins según `GridPosition`
data_binned <- data_filtered %>%
  mutate(bin = cut(GridPosition, breaks = seq(min(GridPosition, na.rm = TRUE), max(GridPosition, na.rm = TRUE), by = binwidth), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(RacePosition_mean = mean(RacePosition, na.rm = TRUE),
            GridPosition_mean = mean(GridPosition, na.rm = TRUE))

# Creamos el gráfico
ggplot(resultados$MadeItToQ2, aes(x = Estimate, y = reorder(Covariate, Estimate))) +
  geom_point(color = "#1f78b4", size = 3) +  # Puntos para los estimadores
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#1f78b4") +  # Barras de error
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Línea en x = 0
  labs(
    title = "Confidence intervals MadeItToQ2",
    x = "Estimate",
    y = "Covariate"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14)
  )

ggplot(resultados$MadeItToQ3, aes(x = Estimate, y = reorder(Covariate, Estimate))) +
  geom_point(color = "#1f78b4", size = 3) +  # Puntos para los estimadores
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#1f78b4") +  # Barras de error
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Línea en x = 0
  labs(
    title = "Confidence intervals MadeItToQ3",
    x = "Estimate",
    y = "Covariate"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14)
  )




analisis_cutoff_rdrobust <- function(data, running_var, dependent_var, cutoffs, grado_pol = 1) {
  
  # Crear una lista para almacenar los resultados
  resultados <- list()
  
  # Iterar sobre cada punto de corte en el vector `cutoffs`
  for (corte in cutoffs) {
    # Verificar si el umbral está dentro del rango de la variable running
    if (corte < min(data[[running_var]], na.rm = TRUE) | corte > max(data[[running_var]], na.rm = TRUE)) {
      message(paste("El umbral especificado (", corte, ") no está dentro del rango de la variable running:", running_var))
      next  # Omitir este corte y pasar al siguiente
    }
    
    # Realizar el análisis con `rdrobust` para el punto de corte actual
    rd_result <- tryCatch(
      rdrobust(y = data[[dependent_var]], x = data[[running_var]], c = corte, p = grado_pol, kernel = "triangular"),
      error = function(e) {
        message(paste("Error en el análisis con corte:", corte, "\n", e))
        return(NULL)
      }
    )
    
    # Verificar si se generó un error en el modelo
    if (is.null(rd_result)) {
      next  # Pasar al siguiente corte si hay error
    }
    
    # Extraer estimaciones y estadísticas relevantes
    estimador <- rd_result$Estimate[1]
    p_valor <- rd_result$pv[3]
    ci_low <- rd_result$ci[3, 1]
    ci_high <- rd_result$ci[3, 2]
    bw_mse <- round(rd_result$bws[1], 3)
    obs_efectivas <- sum(rd_result$N_h)
    
    # Almacenar resultados en la lista
    resultados[[as.character(corte)]] <- data.frame(
      Cutoff = corte,
      MSE_Optimal_Bandwidth = bw_mse,
      RD_Estimator = round(estimador, 3),
      p_value = round(p_valor, 3),
      CI_Lower = round(ci_low, 3),
      CI_Upper = round(ci_high, 3),
      Eff_Number_Observations = obs_efectivas
    )
  }
  
  # Unir todos los resultados en un solo data frame
  resultados_df <- do.call(rbind, resultados)
  
  # Imprimir el data frame en la consola
  print(resultados_df)
  
  # Generar gráfica de los resultados con estilo personalizado
  ggplot(resultados_df, aes(x = Cutoff, y = RD_Estimator)) +
    geom_point(color = "red", size = 2) +  # Puntos rojos para los estimadores
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") +  # Barras de error verticales en azul
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = .5) +  # Línea horizontal en y = 0
    scale_y_continuous(limits = c(min(resultados_df$CI_Lower) - 5, max(resultados_df$CI_Upper) + 5)) +  # Ajustar los límites del eje y
    labs(
      title = paste("RDD estimate", dependent_var, "different cutoffs"),
      x = "Cutoff",
      y = "RDD estimate"
    ) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),  # Quitar líneas de la cuadrícula horizontal
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(size = 12),  # Aumentar tamaño del texto del eje y
      axis.title.x = element_text(size = 14),  # Aumentar tamaño del texto del título del eje x
      axis.text.x = element_text(size = 12)    # Aumentar tamaño del texto del eje x
    )
}

# Ejecutar la función con los parámetros especificados
analisis_cutoff_rdrobust(data, "GapToKnockoutQ2", "TireHARD", c( -1, -0.5, 0, 0.5, 1))




library(lfe)        # Para felm
library(ggplot2)    # Para graficar
library(rdrobust)   # Para seleccionar el ancho de banda

analyze_cutoff <- function(data, var, cutoffs = seq(-2, 2, by = 0.5)) {
  results <- list()
  
  for (cutoff in cutoffs) {
    # Seleccionar el ancho de banda
    bw <- rdbwselect(y = data[[var]], x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
    bandwidth <- bw$bws[1]
    
    # Filtrar los datos según el ancho de banda
    data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
    
    # Ajustar el modelo con covariables
    model <- felm(as.formula(paste(var, "~ MadeItToQ2 + GapToKnockoutQ1 | Team:GP")), data = data_filtered)
    
    # Extraer los coeficientes y el intervalo de confianza
    coefs <- summary(model)$coefficients
    estimate <- coefs["MadeItToQ2", "Estimate"]
    std_error <- coefs["MadeItToQ2", "Std. Error"]
    
    results[[as.character(cutoff)]] <- data.frame(
      Cutoff = cutoff,
      Estimate = estimate,
      CI_lower = estimate - 1.96 * std_error,
      CI_upper = estimate + 1.96 * std_error,
      Bandwidth = bandwidth,
      Observations = nrow(data_filtered)
    )
  }
  
  # Combinar los resultados en un solo dataframe
  results_df <- do.call(rbind, results)
  
  # Graficar los resultados
  ggplot(results_df, aes(x = Cutoff, y = Estimate)) +
    geom_point(color = "#1f78b4", size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.02, color = "#1f78b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = paste("Análisis de Cutoff para", var),
      x = "Cutoff",
      y = "Estimación del Tratamiento (MadeItToQ2)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 14)
    )
}

# Ejemplo de uso de la función con diferentes cutoffs
analyze_cutoff(data, "GridPosition")


reg<-rdrobust(y = data$TireHARD, x = data$GapToKnockoutQ1, c =0, p = 1, kernel = "triangular")
summary(reg)
