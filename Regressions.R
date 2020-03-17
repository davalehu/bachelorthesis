#################################################################################
# Bachelor Thesis David Huber                                                   #
# This R Script conducts the regressions for the Simple Election model          #
# and the Vote Share model                                                      #
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


# Load Data

Elections <- read_excel("~/Documents/David/University/Thesis/R/Election Data/Elections.xlsx", col_names = TRUE, col_types = NULL)
Elections

####################### Prepare Data ###############################



