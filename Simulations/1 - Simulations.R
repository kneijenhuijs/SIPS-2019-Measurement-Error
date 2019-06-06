library(SimDesign)
library(lme4)
library(car)
library(ez)
library(tidyverse)

options(scipen=99)

# Design with necessary parameters
# sampleSize: The N of the dataset
# effectSize: The mean effect of the interaction term. This is based on a MCID of 18%.
# measError: The percentage of measurement error
Design <- expand.grid(sampleSize = c(250, 500, 750),
                      effectSize = c(0*4.338, 1*4.338, 2*4.338, 3*4.338), # Change to means
                      measError = c(0, 0.1, 0.2, 0.3, 0.4)) # Percentage measurement error

# Function that generates the data
Generate <- function(condition, fixed_objects=NULL) {
  Attach(condition) # So I don't have to type "condition$" in front of every element in our design
  # Generate measurement error as percentage of total possible value of BDI-II. Example: if Measurement Error = 0.1 (10% of range), the mean of the measurement error distribution is 0.1*63=6.3.
  r <- 0.5 # Intra-individual correlation of measurement error
  Sigma <- matrix(c((3.214286)^2, r, r, (3.214286)^2),nrow=2) # Covariance matrix
  measurementError <- rmvnorm(sampleSize, mean=c(measError*63,measError*63), sigma=Sigma)
  measSign <- rbinom(sampleSize, 1, prob=c(0.5,0.5)) # random binomial to make measurement error randomly positively or negatively signed
  measSign[measSign==0] <- -1 # Change zero to -1 for sign
  measurementError <- measurementError * measSign # Apply sign
  measurementErrorDat <- data.frame(id = sort(rep(c(1:sampleSize))),
                                    Pre = measurementError[,1],
                                    Post = measurementError[,2]) %>% gather("time", "measurementError", -id)
  
  # Random effects
  i <- 0.2 # sd between intercepts
  s <- 0.5 # sd between slopes
  r <- 0.8 # correlation between intercepts and slopes
  
  # This calculates the random effects
  cov.matrix1 <- matrix(c(i^2, r * i * s, r * i * s, s^2), nrow = 2, byrow = T)
  randomEffects <- rmvnorm(sampleSize, mean = c(0, 0), sigma = cov.matrix1)
  
  # Alpha = 24.1 + random intercept
  # Beta = 9.828 (+ time effect for treatment group) + random slope
  indDf <- data.frame(id = c(1:sampleSize), 
                      condition=c(rep('Treatment', sampleSize/2), rep('Control', sampleSize/2))) %>% 
    mutate(alpha = 24.1+randomEffects[, 1],
           beta = ifelse(condition=="Treatment", -(9.828+effectSize+randomEffects[, 2]), -(9.828+randomEffects[, 2])))
  
  timeContrast = c(0,1)
  withinDf <-  data.frame(id = sort(rep(c(1:sampleSize), 2)), 
                          time=c(rep(c('Pre', 'Post'), sampleSize)), 
                          x=rep(timeContrast, 2))
  
  dat <- merge(indDf, withinDf) %>% merge(measurementErrorDat) %>% mutate(DV = alpha + x * beta + measurementError)
  # Truncate to 0 or 63
  dat$DV[dat$DV<0] <- 0
  dat$DV[dat$DV>63] <- 63

  dat
}

# Function for analysing the data set
Analyse <- function(condition, dat, fixed_objects=NULL){
  Attach(condition) # So I don't have to type "condition$" in front of every element in our design
  fit <- lmer(DV ~ condition * time + (1 | id), data=dat) # Model fit
  singular <- isSingular(fit) # Check to see whether the fit is singular
  sig <- Anova(fit) # Significance test
  anova <- ezANOVA(dat, dv=.(DV), wid=.(id), within=.(time), between=.(condition)) # RM ANOVA to calculate eta squared
  ret <- data.frame(coefficientCond=coef(summary(fit))[2,1], seCond=coef(summary(fit))[2,2], pCond=sig$`Pr(>Chisq)`[1], etaCond=anova$ANOVA$ges[1], 
                    coefficientTime=coef(summary(fit))[3,1], seTime=coef(summary(fit))[3,2], pTime=sig$`Pr(>Chisq)`[2], etaTime=anova$ANOVA$ges[2],
                    coefficientInt=coef(summary(fit))[4,1], seInt=coef(summary(fit))[4,2], pInt=sig$`Pr(>Chisq)`[3], etaInt=anova$ANOVA$ges[3],
                    singular=as.numeric(singular), effect_Size=as.numeric(effectSize)) # Extract Coefficients, standard errors, significance, and eta squared
  ret
}

# Function that summarises the analysis results into useful statistics for examining which of the data sets showed less bias
Summarise <- function(condition, results, fixed_objects = NULL) {
  bias_Int <- bias(results$coefficientInt, parameter=results$effect_Size, type="relative") # Relative bias summary statistic for the Interaction coefficient
  MAE_Int <- MAE(results$coefficientInt, parameter=results$effect_Size) # Mean absolute error for the Interaction coefficient
  RMSE_Int <- RMSE (results$coefficientInt, parameter=results$effect_Size) # Root Mean Square Error for the Interaction coefficient
  SE_Int <- mean(results$seInt, na.rm=TRUE) # Mean Standard Error of Interaction
  EDR_Int <- EDR(results$pInt, alpha=.05) # Type I errors and Power for Interaction
  ETA_Int <- mean(results$etaInt, na.rm=TRUE) # Mean Eta²
  
  sing <- sum(results$singular, na.rm=TRUE) # How often was the fit singular? 
  
  ret <- data.frame(bias_Int=bias_Int, MAE_Int=MAE_Int, RMSE_Int=RMSE_Int, SE_Int=SE_Int, ETA_Int=ETA_Int, EDR_Int=EDR_Int,
                    singular=sing, int=int)
  ret
}

# Run the simulation
results <- runSimulation(Design, replications=1000, verbose=FALSE, parallel=FALSE, ncores=6, generate=Generate, analyse=Analyse, summarise=Summarise, progress=TRUE)

saveRDS(results, "results.rds")

