#################################################################################
# Bachelor Thesis David Huber                                                   #
# This R Script conducts the regressions for the Simple Election model          #
# and the Vote Share model                                                      #
#################################################################################


####################### Installing Packages & Loading Data ###############################                          

install.packages("dplyr")
install.packages("stargazer")
install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
install.packages("plm")


library(dplyr)
library(stargazer)
library(readxl)
library(tibble)
library(ggplot2)
library(plm)


# Load Data

Elections <- read_excel("~/Documents/David/University/Thesis/R/Election Data/Elections_p.xlsx", 
                        col_names = TRUE, col_types = NULL, sheet = 1)
Elections

####################### Prepare Data ###############################

#Done in Excel for now

####################### Simple Re-Election Model 3.1.3 ###############################

####################### OLS ###############################

#OLS Model no Fixed Effects
OLS <- lm(inc_elect ~ p_average + suitability + p_average*suitability, data = Elections)
summary(OLS)
#Comment: OLS is not well equipped for dichotomous dpendent variables

#OLS Model with district Fixed Effects
OLSFD <- lm(inc_elect ~ p_average + suitability + p_average*suitability + factor(district_nr)-1, data = Elections)
summary(OLSFD)
#Is Suitability the reason for the colinearity?  

#OLS Model with all Fixed Effects
OLSF <- lm(inc_elect ~ p_average + suitability + p_average*suitability + factor(district_nr)-1 + factor(year)-1, data = Elections)
summary(OLSF)
#With year fixed effects, results become insignificant. Maybe because p already accounts for time fixed effects? 
#I.e p doesnt vary within one year.
#BTW R includes the singular variables from the interaction term automatically
#Do I need to interact (year-1)*(district_nr -1) to get time effects for each province? (See model Hodler wrote down)

####################### Logit ###############################

#Logit Model district fixed effects
LogitFD <- glm(inc_elect ~ p_average + suitability + p_average*suitability +
            district_nr-1, data = Elections, family = binomial)
summary(LogitFD)
#p_average is significant

#Fixed effects logit model
LogitF <- glm(inc_elect ~ p_average + suitability + p_average*suitability +
                 factor(district_nr-1) + factor(year-1), data = Elections, family = binomial)
summary(LogitF)
#Attention: The results change drastically if I use factor(year)-1. Why? Before it was regressed on the actual year number
# now on a dummy, but shouldnt this somehow not make a difference? Because else the order of the numbering would matter, not?


####################### PLM ###############################
PooledSM <- plm(inc_elect ~ p_average + suitability + p_average*suitability,
                            index = c("district_nr", "year"), model = "pooling", data = Elections)
summary(PooledSM)
#This is the same as a very normal OLS

#Panel Model Within
WithinSM <- plm(inc_elect ~ p_average + suitability + p_average*suitability,
             index = c("district_nr", "year"), model = "within", effect = "twoway", data = Elections)
summary(WithinSM)

#Panel Model Random 
RandomSM <- plm(inc_elect ~ p_average + suitability + p_average*suitability,
               index = c("district_nr", "year"), model = "random", effect = "twoway", data = Elections)
summary(RandomSM)


#Include Fixed Effects?

pFtest(WithinSM, PooledSM) 

#Yes! Include fixed effects because the p-value is below 0.05. See https://www.princeton.edu/~otorres/Panel101R.pdf

#Hausmann Test

phtest(WithinSM, RandomSM)

#Breusch Pagan Test

plmtest(PooledSM, type="bp")