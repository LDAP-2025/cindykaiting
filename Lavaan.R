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

semPaths(fit, style="OpenMx") 
         
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
summary(fit2, standardized = TRUE, fit.measures = TRUE) 

# Current Pregnant 

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

