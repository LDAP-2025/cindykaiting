# Research Question 
library(tidyverse) 
library(ggplot2) 
library(dplyr) 
library(Hmisc)
library(psych)
library(haven) 
library(foreign)
library(corrplot) 

wave1 <- read_sav("covid-19_wave1_survey_cls.sav") %>%
  as.data.frame()

wave2 <- read_sav("covid-19_wave2_survey_cls.sav") %>%
  as.data.frame()

wave3 <- read_sav("covid-19_wave3_survey_cls.sav") %>% 
  as.data.frame() 