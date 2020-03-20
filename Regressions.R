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

Elections$incvote <- Elections$incvote * 100
Elections$p_change <- Elections$p_change * 100

# Take first differences and compute vote share growth

Elections <- mutate(Elections, first_diff = incvote-incvote_last)
Elections <- mutate(Elections, incvote_growth = first_diff / incvote_last)

####################### Simple Re-Election Model 3.1.3 ###############################

####################### OLS ###############################

#OLS Model no Fixed Effects
OLS <- lm(inc_elect ~ p_average + suitability + p_average*suitability, data = Elections)
summary(OLS)
#Comment: OLS is not well equipped for dichotomous dpendent variables

#OLS Model with district Fixed Effects
OLSFD <- lm(inc_elect ~ p_average + suitability + p_average*suitability + 
              factor(district_nr)-1, data = Elections)
summary(OLSFD)
#Is Suitability the reason for the collinearity?  

#OLS Model with all Fixed Effects
OLSF <- lm(inc_elect ~ p_average + suitability + p_average*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(OLSF)
#Do I need to interact (year-1)*(district_nr -1) to get time effects for each province? (See model Hodler wrote down)

####################### Logit ###############################

#Logit Model district fixed effects
LogitFD <- glm(inc_elect ~ p_average + suitability + p_average*suitability +
            factor(district_nr) -1, data = Elections, family = binomial)
summary(LogitFD)
#p_average is significant

#Fixed effects logit model
LogitF <- glm(inc_elect ~ p_average + suitability + p_average*suitability +
                 factor(district_nr)-1 + factor(year)-1, data = Elections, family = binomial)
summary(LogitF)



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

pFtest(WithinSM, OLS) 

#Yes! Include fixed effects because the p-value is below 0.05. See https://www.princeton.edu/~otorres/Panel101R.pdf

#Hausmann Test

phtest(WithinSM, RandomSM)

#Breusch Pagan Test

plmtest(PooledSM, type="bp")

####################### Dynamic Re-Election Model 3.3.1 ###############################

####################### OLS ###############################

#OLS Model no Fixed Effects
OLS <- lm(inc_elect ~ p_change + suitability + p_change*suitability, data = Elections)
summary(OLS)
#Comment: OLS is not well equipped for dichotomous dpendent variables

#OLS Model with district Fixed Effects
OLSFD <- lm(inc_elect ~ p_change + suitability + p_change*suitability + 
              factor(district_nr)-1, data = Elections)
summary(OLSFD)
#Is Suitability the reason for the collinearity?  

#OLS Model with all Fixed Effects
OLSF <- lm(inc_elect ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(OLSF)
#Do I need to interact (year-1)*(district_nr -1) to get time effects for each province? (See model Hodler wrote down)

####################### Logit ###############################

#Logit Model district fixed effects
LogitFD <- glm(inc_elect ~ p_change + suitability + p_change*suitability +
                 factor(district_nr) -1, data = Elections, family = binomial)
summary(LogitFD)


#Fixed effects logit model
LogitF <- glm(inc_elect ~ p_change + suitability + p_change*suitability +
                factor(district_nr)-1 + factor(year)-1, data = Elections, family = binomial)
summary(LogitF)



####################### PLM ###############################
PooledSM <- plm(inc_elect ~ p_change + suitability + p_change*suitability,
                index = c("district_nr", "year"), model = "pooling", data = Elections)
summary(PooledSM)
#This is the same as a very normal OLS

#Panel Model Within
WithinSM <- plm(inc_elect ~ p_change + suitability + p_change*suitability,
                index = c("district_nr", "year"), model = "within", effect = "twoway", data = Elections)
summary(WithinSM)

#Panel Model Random 
RandomSM <- plm(inc_elect ~ p_change + suitability + p_change*suitability,
                index = c("district_nr", "year"), model = "random", effect = "twoway", data = Elections)
summary(RandomSM)


####################### Simple Vote Share Model 3.2.1 ###############################

####################### OLS ###############################

#OLS Model no Fixed Effects
OLS <- lm(incvote ~ p_average + suitability + p_average*suitability, data = Elections)
summary(OLS)

#OLS Model with district Fixed Effects
OLSFD <- lm(incvote ~ p_average + suitability + p_average*suitability + 
              factor(district_nr)-1, data = Elections)
summary(OLSFD)
#Is Suitability the reason for the collinearity?  

#OLS Model with all Fixed Effects
OLSF <- lm(incvote ~ p_average + suitability + p_average*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(OLSF)
#Do I need to interact (year-1)*(district_nr -1) to get time effects for each province? (See model Hodler wrote down)

####################### PLM ###############################
PooledSM <- plm(incvote ~ p_average + suitability + p_average*suitability,
                index = c("district_nr", "year"), model = "pooling", data = Elections)
summary(PooledSM)
#This is the same as a very normal OLS

#Panel Model Within
WithinSM <- plm(incvote ~ p_average + suitability + p_average*suitability,
                index = c("district_nr", "year"), model = "within", effect = "twoway", data = Elections)
summary(WithinSM)

#Panel Model Random 
RandomSM <- plm(incvote ~ p_average + suitability + p_average*suitability,
                index = c("district_nr", "year"), model = "random", effect = "twoway", data = Elections)
summary(RandomSM)

####################### Dynamic Vote Share Model 3.3.2 ###############################

####################### OLS ###############################

#OLS Model no Fixed Effects
OLS <- lm(incvote ~ p_change + suitability + p_change*suitability, data = Elections)
summary(OLS)

#OLS Model with district Fixed Effects
OLSFD <- lm(incvote ~ p_change + suitability + p_change*suitability + 
              factor(district_nr)-1, data = Elections)
summary(OLSFD)
#Is Suitability the reason for the collinearity?  

#OLS Model with all Fixed Effects
OLSF <- lm(incvote ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(OLSF)
#Do I need to interact (year-1)*(district_nr -1) to get time effects for each province? (See model Hodler wrote down)

stargazer(OLSF, 
          type = "latex",
          title = "My Final Results?",
          align = TRUE,
          omit = c("district_nr", "year"))
          
####################### PLM ###############################
PooledSM <- plm(incvote ~ p_change + suitability + p_change*suitability,
                index = c("district_nr", "year"), model = "pooling", data = Elections)
summary(PooledSM)
#This is the same as a very normal OLS

#Panel Model Within
WithinSM <- plm(incvote ~ p_change + suitability + p_change*suitability,
                index = c("district_nr", "year"), model = "within", effect = "twoway", data = Elections)
summary(WithinSM)

#Panel Model Random 
RandomSM <- plm(incvote ~ p_change + suitability + p_change*suitability,
                index = c("district_nr", "year"), model = "random", effect = "twoway", data = Elections)
summary(RandomSM)

####################### Incvote Growth ###############################
OLSFD <- lm(incvote_growth ~ p_change + suitability + p_change*suitability + 
              factor(district_nr)-1, data = Elections)
summary(OLSFD)
#Is Suitability the reason for the collinearity?  

#OLS Model with all Fixed Effects
OLSF <- lm(incvote_growth ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(OLSF)

####################### Country subsets ###############################

#OLS Model with all Fixed Effects
OLSF <- lm(incvote_growth ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = subset(Elections, country == "Argentina"))
summary(OLSF)

OLSF <- lm(incvote_growth ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = subset(Elections, country == "Brazil"))
summary(OLSF)

#There is some significant effects of incvote_growth in the subsets, but in the over the whole set
