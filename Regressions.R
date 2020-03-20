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

# Multiply by 100 to get easier readable percentage format.
Elections$incvote <- Elections$incvote * 100
Elections$p_change <- Elections$p_change * 100

# Take first differences and compute vote share growth

Elections <- mutate(Elections, first_diff = incvote-incvote_last)
Elections <- mutate(Elections, incvote_growth = first_diff / incvote_last)

####################### Static Re-Election Model 3.1.3 ###############################

####################### OLS ###############################

#Comment: OLS is not well equipped for dichotomous dpendent variables

#OLS Model no Fixed Effects
SEOLS <- lm(inc_elect ~ p_average + suitability + p_average*suitability, data = Elections)
summary(SEOLS)

#OLS Model with district Fixed Effects
SEOLSFD <- lm(inc_elect ~ p_average + suitability + p_average*suitability + 
              factor(district_nr)-1, data = Elections)
summary(SEOLSFD)

#OLS Model with all Fixed Effects
SEOLSF <- lm(inc_elect ~ p_average + suitability + p_average*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(SEOLSF)
#Do I need to interact (year-1)*(district_nr -1) to get time effects for each province? (See model Hodler wrote down)

####################### Logit ###############################

#Logit Model district fixed effects
SELogitFD <- glm(inc_elect ~ p_average + suitability + p_average*suitability +
            factor(district_nr) -1, data = Elections, family = binomial)
summary(SELogitFD)
#p_average is significant

#Fixed effects logit model
SELogitF <- glm(inc_elect ~ p_average + suitability + p_average*suitability +
                 factor(district_nr)-1 + factor(year), data = Elections, family = binomial, maxit=100)
summary(SELogitF)
#The 73 Fisher Scoring Iterations show that the Model is not very easy to fit. 

#Country specific: 
SELogitFA <- glm(inc_elect ~ p_average + suitability + p_average*suitability +
                  factor(district_nr)-1 + factor(year), 
                 data = subset(Elections, country == "Argentina"), family = binomial, maxit=100)
summary(SELogitFA)

SELogitFB <- glm(inc_elect ~ p_average + suitability + p_average*suitability +
                   factor(district_nr)-1 + factor(year),
                 data = subset(Elections, country == "Brazil"), family = binomial, maxit=100)
summary(SELogitFB)

####################### Stargazer Output ###############################

stargazer(SELogitF, SELogitFA, SELogitFB, 
          type = "latex",
          title = "Equation 3.13",
          align = TRUE,
          omit = c("district_nr", "year"),
          omit.stat=c("adj.rsq","ser", "aic", "ll"),
          digits = 2)
#Stargazer cannot display scientific results, therefore the tabe has lots of zeros. Anyway its not really worth
#displaying the table anyway as the results are null. 

####################### Dynamic Re-Election Model 3.3.1 ###############################

####################### OLS ###############################

#OLS Model no Fixed Effects
DEOLS <- lm(inc_elect ~ p_change + suitability + p_change*suitability, data = Elections)
summary(DEOLS)
#Comment: OLS is not well equipped for dichotomous dpendent variables

#OLS Model with district Fixed Effects
DEOLSFD <- lm(inc_elect ~ p_change + suitability + p_change*suitability + 
              factor(district_nr)-1, data = Elections)
summary(DEOLSFD)

#OLS Model with all Fixed Effects
DEOLSF <- lm(inc_elect ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(DEOLSF)
#Do I need to interact (year-1)*(district_nr -1) to get time effects for each province? (See model Hodler wrote down)

####################### Logit ###############################

#Logit Model district fixed effects
DELogitFD <- glm(inc_elect ~ p_change + suitability + p_change*suitability +
                 factor(district_nr) -1, data = Elections, family = binomial)
summary(DELogitFD)


#Fixed effects logit model
DELogitF <- glm(inc_elect ~ p_change + suitability + p_change*suitability +
                factor(district_nr)-1 + factor(year)-1, data = Elections, family = binomial)
summary(DELogitF)

#Country specific: 
DELogitFA <- glm(inc_elect ~ p_change + suitability + p_change*suitability +
                   factor(district_nr)-1 + factor(year), 
                 data = subset(Elections, country == "Argentina"), family = binomial, maxit=100)
summary(DELogitFA)

DELogitFB <- glm(inc_elect ~ p_change + suitability + p_change*suitability +
                   factor(district_nr)-1 + factor(year),
                 data = subset(Elections, country == "Brazil"), family = binomial, maxit=100)
summary(DELogitFB)

####################### Stargazer Output ###############################

stargazer(DELogitF, DELogitFA, DELogitFB, 
          type = "latex",
          title = "Equation 3.3.1",
          align = TRUE,
          omit = c("district_nr", "year"),
          omit.stat=c("adj.rsq","ser", "aic", "ll"),
          digits = 2)
#Stargazer cannot display scientific results, therefore the tabe has lots of zeros. Anyway its not really worth
#displaying the table anyway as the results are null. 


####################### Static Vote Share Model 3.2.1 ###############################

####################### OLS ###############################

#OLS Model no Fixed Effects
SVOLS <- lm(incvote ~ p_average + suitability + p_average*suitability, data = Elections)
summary(SVOLS)

#OLS Model with district Fixed Effects
SVOLSFD <- lm(incvote ~ p_average + suitability + p_average*suitability + 
              factor(district_nr)-1, data = Elections)
summary(SVOLSFD)
#Is Suitability the reason for the collinearity?  

#OLS Model with all Fixed Effects
SVOLSF <- lm(incvote ~ p_average + suitability + p_average*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(SVOLSF)

####################### PLM ###############################
#Pooled Model == SVOLS
PooledSV <- plm(incvote ~ p_average + suitability + p_average*suitability,
                index = c("district_nr", "year"), model = "pooling", data = Elections)
summary(PooledSV)

#Panel Model Within == SVOLSF
WithinSV <- plm(incvote ~ p_average + suitability + p_average*suitability,
                index = c("district_nr", "year"), model = "within", effect = "twoway", data = Elections)
summary(WithinSV)

#Panel Model Random 
RandomSV <- plm(incvote ~ p_average + suitability + p_average*suitability,
                index = c("district_nr", "year"), model = "random", effect = "twoway", data = Elections)
summary(RandomSV)

# Country Specific - The Within Models are equivalent to the lm models, but I prefer the LM Models because I see all coefficients

WithinSVA <- plm(incvote ~ p_average + suitability + p_average*suitability,
                index = c("district_nr", "year"), model = "within", effect = "twoway", 
                data = subset(Elections, country == "Argentina"))
summary(WithinSVA)

WithinSVA <- plm(incvote ~ p_average + suitability + p_average*suitability,
                 index = c("district_nr", "year"), model = "within", effect = "twoway", 
                 data = subset(Elections, country == "Brazil"))
summary(WithinSVA)

SVOLSFA <- lm(incvote ~ p_average + suitability + p_average*suitability +
               factor(district_nr)-1 + factor(year), data = subset(Elections, country == "Argentina"))
summary(SVOLSFA)

SVOLSFB <- lm(incvote ~ p_average + suitability + p_average*suitability +
                factor(district_nr)-1 + factor(year), data = subset(Elections, country == "Brazil"))
summary(SVOLSFB)

####################### Stargazer Output ###############################

stargazer(SVOLSF, SVOLSFA, SVOLSFB, 
          type = "latex",
          title = "Equation 3.2.1",
          align = TRUE,
          omit = c("district_nr", "year"),
          omit.stat=c("adj.rsq","ser", "aic", "ll"),
          digits = 2)
 
####################### Dynamic Vote Share Model 3.3.2 ###############################

####################### OLS ###############################

#OLS Model no Fixed Effects
DVOLS <- lm(incvote ~ p_change + suitability + p_change*suitability, data = Elections)
summary(DVOLS)

#OLS Model with district Fixed Effects
DVOLSFD <- lm(incvote ~ p_change + suitability + p_change*suitability + 
              factor(district_nr)-1, data = Elections)
summary(DVOLSFD)
#Is Suitability the reason for the collinearity?  

#OLS Model with all Fixed Effects
DVOLSF <- lm(incvote ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(DVOLSF)
#Do I need to interact (year-1)*(district_nr -1) to get time effects for each province? (See model Hodler wrote down)

####################### Country Specific ###############################

DVOLSFA <- lm(incvote ~ p_change + suitability + p_change*suitability +
               factor(district_nr)-1 + factor(year), data = subset(Elections, country == "Argentina"))
summary(DVOLSFA)

DVOLSFB <- lm(incvote ~ p_change + suitability + p_change*suitability +
                factor(district_nr)-1 + factor(year), data = subset(Elections, country == "Brazil"))
summary(DVOLSFB)


####################### Stargazer Output ###############################

stargazer(DVOLSF, DVOLSFA, DVOLSFB,
          column.labels = c("Total Sample", "Argentina", "Brazil"),
          type = "latex",
          title = "Equation 3.3.2",
          align = TRUE,
          omit = c("district_nr", "year"),
          omit.stat=c("adj.rsq","ser", "aic", "ll","f"),
          digits = 2)

####################### Incvote Growth - P Change ###############################
DGOLSFD <- lm(incvote_growth ~ p_change + suitability + p_change*suitability + 
              factor(district_nr)-1, data = Elections)
summary(DGOLSFD)

#OLS Model with all Fixed Effects
DGOLSF <- lm(incvote_growth ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = Elections)
summary(DGOLSF)

####################### Country subsets ###############################

#OLS Model with all Fixed Effects
DGOLSFA <- lm(incvote_growth ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = subset(Elections, country == "Argentina"))
summary(DGOLSFA)

DGOLSFB <- lm(incvote_growth ~ p_change + suitability + p_change*suitability +
             factor(district_nr)-1 + factor(year), data = subset(Elections, country == "Brazil"))
summary(DGOLSFB)

####################### Stargazer Output ###############################

stargazer(DGOLSF, DGOLSFA, DGOLSFB,
          column.labels = c("Total Sample", "Argentina", "Brazil"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes")),
          model.numbers = FALSE,
          type = "latex",
          title = "Equation 3.3.2",
          align = TRUE,
          omit = c("district_nr", "year"),
          omit.stat=c("adj.rsq","ser", "aic", "ll","f"),
          digits = 2)

