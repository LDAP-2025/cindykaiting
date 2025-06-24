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

# Looking at how did financial stress contribute to relationship satisfaction in cohabitating couples in Covid-19? 
# First step is to look at CLPM without divorce first. 

# First giving each participant only 1 ID (4 options?? Check this)
# Step 1: Create cohabiter ID list from wave1
cohab_id <- wave1 %>%
  mutate(id = coalesce(NCDSID, BCSID, NSID, MCSID)) %>%
  filter(CW1_OTHRELA == 1) %>%
  select(id)

# First filter for cohabitating couples in W1= CW1_OTHRELA 
# cohab_id <- wave1new %>% filter(CW1_OTHRELA == 1) %>% select(id, CW1_OTHRELA) 

# Financial stress variable: 

# Protective factors variable: 

# Relationship satisfaction variable: 
wave1relsat <- wave1 %>% select(CW1_RELSAT) 
wave2relsat <- wave2 %>% select(CW2_RELSAT) 
wave3relsat <- wave3 %>% select(CW3_RELSAT) 

# Cohabiting participants
cohab_id <- wave1 %>%
  mutate(id = coalesce(NCDSID, BCSID, NSID, MCSID)) %>%
  filter(CW1_OTHRELA == 1) %>%
  select(id) 

# RS from wave 1
rs_w1 <- wave1 %>%
  semi_join(cohab_id, by = "id") %>%
  select(id, relsat_w1 = CW1_RELSAT) 

# RS from wave 2
rs_w2 <- wave2 %>%
  semi_join(cohab_id, by = "id") %>%
  select(id, relsat_w2 = CW2_RELSAT) 

# RS from wave 3
rs_w3 <- wave3 %>%
  semi_join(cohab_id, by = "id") %>%
  select(id, relsat_w3 = CW3_RELSAT) 


data <- c(rs_w1, rs_w2, rs_w3)

model <- '
  # latent variable definitions
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual (co)variances
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

fit <- sem(model, 
           data = data) 

coef(fit)