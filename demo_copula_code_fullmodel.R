# R code for correcting for endogeneity in the Corporate Reputation Data PLS model
# using Gaussian Copula Approach as descripted in Park and Gupta (2012)

# PLEASE CITE AS:
# Hult, G. T. M., J. F. Hair, D. Proksch, M. Sarstedt, A. Pinkwart, & C. M. Ringle (2018).
# Addressing Endogeneity in International Marketing Applications of Partial 
# Least Squares Structural Equation Modeling. Journal of International Marketing,
# forthcoming.

# Load required libraries -> PLEASE INSTALL THE "CAR" PACKAGE IF YOU HAVE NOT
# ALREADY. SEE https://www.r-bloggers.com/how-to-install-packages-on-r-screenshots/
# FOR INSTRUCTIONS HOW TO INSTALL A PACKAGE

library(car)
#--------------------------------------------------------------
# Function to create Gausian Copula
# From Gui, Raluca, Markus Meierer, and Rene Algesheimer (2017), 
# "R Package REndo: Fitting Linear Models with Endogenous Regressors using
# Latent Instrumental Variables (Version 1.3)," https://cran.r-project.org/web/packages/REndo/

createCopula <- function(P){
	H.p <- stats::ecdf(P)
	H.p <- H.p(P)
	H.p <- ifelse(H.p==0,0.0000001,H.p)
	H.p <- ifelse(H.p==1,0.9999999,H.p)
	U.p <- H.p
	p.star <- stats::qnorm(U.p)
	return(p.star)	
}
#--------------------------------------------------------------
# Function to calculate corrected p-values for regression based on bootstrapped standard errors

bootstrapedSignificance <- function(dataset, bootstrapresults, numIndependentVariables, numCopulas){
	for (i in 1:nrow(summary(bootstrapresults))){
		t <- summary(bootstrapresults)[i, "original"] / summary(bootstrapresults)[i, "bootSE"]
		# df = n (number of observations) - k (number of independent variables + copulas) - 1
		pvalue <- 2 * pt(-abs(t),df=nrow(dataset)-numIndependentVariables-numCopulas-1)
		cat("Pr(>|t|)", rownames(summary(bootstrapresults))[i], ": ", pvalue, "\n")
	}
}

# Read data (extracted standardized latent variable scores from PLS model)

#==========================================================================
attach(Structural_model)
View(Structural_model)
# Calculate standard regression
stdModel <- lm (Resilience ~ SCAC + Ambidexterity)
summary(stdModel);

#--------------------------------------------------------------
# Calculate copulas for indpendent variables within model
SCAC_star <- createCopula(SCAC)
Ambidexterity_star <- createCopula(Ambidexterity)
#quality_star <- createCopula(quality)

# Set bootstrapping rounds
# FOR TESTING PURPOSE, WE RECOMMEND SETTING THIS VALUE TO 100; FOR REPORTING THE FINAL
# RESULTS WE RECOMMEND SETTING IT TO 10000

bootrounds = 10000

# Calculate Results
#--------------------------------------------------------------
# Include Copula for loyal
# Normal regression

copulaResults1 <- lm (Resilience ~ Ambidexterity + SCAC + SCAC_star + 0)
summary(copulaResults1)

# Bootstrap Standard Errors

bootCopulaResults1 <- Boot(copulaResults1, R=bootrounds)
summary(bootCopulaResults1)

# Calculate corrected p-values based on bootstrapped standard errors

bootstrapedSignificance(demo, bootCopulaResults1, 3, 1)

#--------------------------------------------------------------
# Include Copula for sat
# Normal copula regression

copulaResults2 <- lm (Resilience ~ Ambidexterity + SCAC + Ambidexterity_star + 0)
summary(copulaResults2)

# Bootstrap standard errors

?bootCopulaResults2 <- Boot(copulaResults2, R=bootrounds)
summary(bootCopulaResults2)

# Calculate corrected p-values based on bootstrapped standard errors

bootstrapedSignificance(demo, bootCopulaResults2, 3, 1)

#--------------------------------------------------------------
# Include Copula for quality
# Normal copula regression
copulaResults3 <- lm (behint ~ loyal + sat + quality + quality_star + 0)
summary(copulaResults3)
# Bootstrap standard errors
bootCopulaResults3 <- Boot(copulaResults3, R=bootrounds)
summary(bootCopulaResults3)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(demo, bootCopulaResults3, 3, 1)

#--------------------------------------------------------------
# Include Copula for sat and loyal
# Normal copula regression
copulaResults4 <- lm (Resilience ~ SCAC + Ambidexterity +  SCAC_star + Ambidexterity_star + 0)
summary(copulaResults4)
# Bootstrap standard errors
bootCopulaResults4 <- Boot(copulaResults4, R=bootrounds)
summary(bootCopulaResults4)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(demo, bootCopulaResults4, 3, 2)

#--------------------------------------------------------------
# Include Copula for sat and quality
# Normal copula regression
copulaResults5 <- lm (behint ~ loyal + sat + quality + sat_star + quality_star + 0)
summary(copulaResults5)
# Bootstrap standard errors
bootCopulaResults5 <- Boot(copulaResults5, R=bootrounds)
summary(bootCopulaResults5)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(demo, bootCopulaResults5, 3, 2)

#--------------------------------------------------------------
# Include Copula for loyal and quality
# Normal copula regression
copulaResults6 <- lm (behint ~ loyal + sat + quality + loyal_star + quality_star + 0)
summary(copulaResults6)
# Bootstrap standard errors
bootCopulaResults6 <- Boot(copulaResults6, R=bootrounds)
summary(bootCopulaResults6)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(demo, bootCopulaResults6, 3, 2)

#--------------------------------------------------------------
# Include Copula for sat, loyal and quality
# Normal copula regression
copulaResults7 <- lm (behint ~ loyal + sat + quality + loyal_star + sat_star + quality_star + 0)
summary(copulaResults7)
# Bootstrap standard errors
bootCopulaResults7 <- Boot(copulaResults7, R=bootrounds)
summary(bootCopulaResults7)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(demo, bootCopulaResults7, 3, 3)
#--------------------------------------------------------------