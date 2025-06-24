library(lavaan) 
library(semPlot)
Data <- wide_dat 
str(Data) 
summary(Data) 

# Starting with the simplest CLPM of FINANCIALMAND -> RELSAT at 3 timepoints 
model1 <- '
  # autoregressive paths
  CW2_FINANCIALMAND ~ a1*CW1_FINANCIALMAND
  CW3_FINANCIALMAND ~ a2*CW2_FINANCIALMAND
  CW2_RELSAT ~ b1*CW1_RELSAT
  CW3_RELSAT ~ b2*CW2_RELSAT

  # cross-lagged paths
  CW2_RELSAT ~ c1*CW1_FINANCIALMAND
  CW3_RELSAT ~ c2*CW2_FINANCIALMAND
  CW2_FINANCIALMAND ~ d1*CW1_RELSAT
  CW3_FINANCIALMAND ~ d2*CW2_RELSAT

# covariances 
  CW1_FINANCIALMAND ~~ CW1_RELSAT
  CW2_FINANCIALMAND ~~ CW2_RELSAT
  CW3_FINANCIALMAND ~~ CW3_RELSAT
' 

fit1 <- sem(model1, data = wide_dat, missing = "ML", meanstructure = TRUE) 
summary(fit1, standardized = TRUE, fit.measures = TRUE) 
# Fit is okay.. CFI good fit but RMSEA is poor fit. 
# Significant autoregressive paths for both financial stress and relationship satisfaction 
# Non-significant cross-lagged paths. 
# Covariances: Consistently correlated with each other within each wave, but do not influence each other longitudinally. 

semPlot::semPaths(fit1) 
semPaths(fit1,
         what = "std",  
         layout = "tree2",
         rotation = 4,
         reorder = FALSE,
         color = list(man = "white"),
         fade= FALSE,
         residuals = TRUE,        
         title = FALSE) 

# Now number of rooms ratio and relationship satisfaction: 
model2 <- '
  # autoregressive paths 
  CW2_PRRATIO ~ a1*CW1_PRRATIO
  CW3_PRRATIO ~ a2*CW2_PRRATIO
  CW2_RELSAT ~ b1*CW1_RELSAT
  CW3_RELSAT ~ b2*CW2_RELSAT

  # cross-lagged paths
  CW2_RELSAT ~ c1*CW1_PRRATIO
  CW3_RELSAT ~ c2*CW2_PRRATIO
  CW2_PRRATIO ~ d1*CW1_RELSAT
  CW3_PRRATIO ~ d2*CW2_RELSAT

  # covariances
  CW1_PRRATIO ~~ CW1_RELSAT
  CW2_PRRATIO ~~ CW2_RELSAT
  CW3_PRRATIO ~~ CW3_RELSAT 
  ' 

fit2 <- sem(model2, data = wide_dat, missing = "ML", meanstructure = TRUE) 
# Missing values, some pairwise combinations have less than 10% coverage: 
lavInspect(fit2, "coverage") 

library(naniar)
vis_miss(wide_dat[, c("CW1_PRRATIO", "CW2_PRRATIO", "CW3_PRRATIO",
                      "CW1_RELSAT", "CW2_RELSAT", "CW3_RELSAT")]) 

summary(fit2, standardized = TRUE, fit.measures = TRUE) 
# Model fit is okay, but lots of missing data. CFI >0.9, RMSEA 0.082 (moderate fit), chi square (poor exact fit). 
# Autoregressive paths are significant and stable over time. 
# CL paths: Wave 1 PRRATIO → Wave 2 RELSAT (Higher household crowding predicts a drop in future relationship satisfaction, small significant); 
# Wave 2 PRRATIO → Wave 3 RELSAT (this strengthens over time); 
# RELSAT → PRRATIO Doesn't make sense). 

semPlot::semPaths(fit2) 
semPaths(fit2,
         what = "std",  
         layout = "tree2",
         rotation = 4,
         reorder = FALSE,
         color = list(man = "white"),
         fade= FALSE,
         residuals = TRUE,        
         title = FALSE) 

# Current Pregnancy 
model3 <- '
  # Autoregressive paths
  CW2_CURPREG ~ a1*CW1_CURPREG
  CW3_CURPREG ~ a2*CW2_CURPREG
  CW2_RELSAT ~ b1*CW1_RELSAT
  CW3_RELSAT ~ b2*CW2_RELSAT

  # Cross-lagged paths
  CW2_RELSAT ~ c1*CW1_CURPREG
  CW3_RELSAT ~ c2*CW2_CURPREG
  CW2_CURPREG ~ d1*CW1_RELSAT
  CW3_CURPREG ~ d2*CW2_RELSAT

  # Covariances
  CW1_CURPREG ~~ CW1_RELSAT
  CW2_CURPREG ~~ CW2_RELSAT
  CW3_CURPREG ~~ CW3_RELSAT
' 

fit3 <- sem(model3, data = wide_dat, missing = "ML", meanstructure = TRUE) 
summary(fit3, standardized = TRUE, fit.measures = TRUE) 

semPlot::semPaths(fit3) 
semPaths(fit3,
         what = "std",  
         layout = "tree2",
         rotation = 4,
         reorder = FALSE,
         color = list(man = "white"),
         fade= FALSE,
         residuals = TRUE,        
         title = FALSE) 

# Covariances- negative within the waves. 
# Do not predict each other over time, contrary to our hypothesis. 
# Poor model fit. 

# Okay now adding in a mediator of social support (latent variable) between the stressors: Model 11, 22, 33: 
model11 <- '
# latent factor for social support: 
  CW1_SUPPORT =~ CW1_SOCPROV_1 + CW1_SOCPROV_2 + CW1_SOCPROV_3_R
  CW2_SUPPORT =~ CW2_SOCPROV_1 + CW2_SOCPROV_2 + CW2_SOCPROV_3_R
  CW3_SUPPORT =~ CW3_SOCPROV_1 + CW3_SOCPROV_2 + CW3_SOCPROV_3_R 
  
  # Autoregressive paths
  CW2_FINANCIALMAND ~ a1*CW1_FINANCIALMAND
  CW3_FINANCIALMAND ~ a2*CW2_FINANCIALMAND
  CW2_SUPPORT ~ s1*CW1_SUPPORT
  CW3_SUPPORT ~ s2*CW2_SUPPORT
  CW2_RELSAT ~ b1*CW1_RELSAT
  CW3_RELSAT ~ b2*CW2_RELSAT

  # Cross-lagged mediation paths 
  CW2_SUPPORT ~ c1*CW1_FINANCIALMAND
  CW3_SUPPORT ~ c2*CW2_FINANCIALMAND
  CW2_RELSAT ~ m1*CW1_SUPPORT
  CW3_RELSAT ~ m2*CW2_SUPPORT

  # Direct effects (done in model1?)
  CW2_RELSAT ~ d1*CW1_FINANCIALMAND
  CW3_RELSAT ~ d2*CW2_FINANCIALMAND

  # Covariances 
  CW1_FINANCIALMAND ~~ CW1_SUPPORT + CW1_RELSAT
  CW2_FINANCIALMAND ~~ CW2_SUPPORT + CW2_RELSAT
  CW3_FINANCIALMAND ~~ CW3_SUPPORT + CW3_RELSAT
  CW1_SUPPORT ~~ CW1_RELSAT
  CW2_SUPPORT ~~ CW2_RELSAT
  CW3_SUPPORT ~~ CW3_RELSAT
' 
fit11 <- sem(model11, data = wide_dat, missing = "ML", meanstructure = TRUE) 
summary(fit11, standardized = TRUE, fit.measures = TRUE) 

