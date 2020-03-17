#################################################################################
# Bachelor Thesis David Huber                                                   #
# This R Script produces the summary statistics for the Suitability Index       #
#################################################################################


# Installing and loading Packages                           

install.packages("dplyr")
install.packages("stargazer")
install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")


library(dplyr)
library(stargazer)
library(readxl)
library(tibble)
library(ggplot2)

# Preparation of Data

# The data was prepared in excel for better use in R. The following operations were performed:
# 1) Select and copy paste data on Argentina and Brazil from the source file into a new file.
# 2) Supplement missing / unsuitable provinces (CABA, Chubut, San Juan, Santa Cruz, Tierra del Fuego) in Argentina: Suitability = 0
# 3) Create additional collumns with suitability only for Argentina and Brazil for easier use of stargazer
# 4) Change column title from "Weighted Average" to “Weighted_Average" 

# Load Data

si <- read_excel("~/Documents/David/University/Thesis/R/Suitability Data/Suitability.xlsx", col_names = TRUE, col_types = NULL)
si

####################### Summary Statistics ###############################

# Produce Summary Statistics using the stargazer package  

stargazer(as.data.frame(si[c("Weighted_Average", "Argentina", "Brazil")]), 
          type = "latex", digits = 2, omit.summary.stat = c("p25", "p75"),
          title = "Summary Statistics Soybean Farming Suitability Index",
          covariate.labels = c("Total Sample", "Argentina", "Brazil"), out = "suitability_summary.txt")

#The format of the table was slightly adapted by hand to fit the standard template in the paper.


