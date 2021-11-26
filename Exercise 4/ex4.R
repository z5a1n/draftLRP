########################################################################################################################
# Exercise 4: LRPs for BH and Ricker Models
########################################################################################################################

rm(list=ls())
library(ggplot2)
library(cowplot)
library(gsl) #for Ricker MSY calcs
source(paste0(getwd(),"/functions.R")) # contains functions "survivorship_F" and "MSYcalc"

# Read in data

Data <- readRDS(paste0(getwd(),"/Exercise 4/ex4_data.rda"))

WAA <- Data$WAA # weight-at-age for years 1:50
MAT <- Data$MAT # maturity-at-age for years 1:50
VUL_BH <- as.data.frame(Data$VULBH) # vulnerability-at-age for years 1:50 for BH model
VUL_R <- as.data.frame(Data$VULR) # vulnerability-at-age for years 1:50 for Ricker model
D_BH <- Data$BH # Model estimated parameters by year for BH model
D_R <- Data$R # Model estimated parameters by year for for Ricker model

########################################################################################################################
# Move h/R0/phi0 parameterization of B-H SRR to a/b parameterization and calculate "unfished equilibrium SSB"
########################################################################################################################
# Model assumes a Beverton-Holt SRR with steepness h = 0.75
h <- 0.75 # model assumed steepness
R0_bh <- 2.395766 # model estimated equilibrium unfished recruitment [provided]
M <- 0.3 # model assumed natural mortality rate

# mean unfished spawning biomass per recruit (phi0) over fist 5 years (5 years = mean generation time)
phi0_5<-mean(D_BH$phi0[1:5])

# B-H a and b. Here we assume that the stock recruitment relationship is estimated using phi0 from the first 5 years
BHa <- 4*h/(phi0_5*(1-h)) # estimated Beverton-Holt a
BHb <- 1/R0_bh*(BHa-1/phi0_5) # estimated Beverton-Holt b

#"virgin" unfished SSB0
SSB0_bh <- R0_bh*phi0_5

########################################################################################################################
# Move h/R0/phi0 parameterization of Ricker SRR to a/b parameterization and calculate "unfished equilibrium SSB"
# Ricker parameterization: R = a*S*exp(-b*S), S = SSB
########################################################################################################################
# Model assumes a Ricker SRR with steepness h = 0.75
R0_r <- 2.33194 # model estimated equilibrium unfished recruitment [provided]

# Ricker a and b. Here we assume that the stock recruitment relationship is estimated using phi0 from the first 5 years
# Use same h = 0.75 and M = 0.3 and phi0 for Ricker

Ra <- ((5*h)^(5/4))/phi0_5 # estimated Ricker a
Rb <- log(Ra*phi0_5)/(R0_r*phi0_5) # estimated Ricker b

#"virgin" unfished SSB0
SSB0_r <- R0_r*phi0_5

########################################################################################################################
# Calculate SSBmsy using WAA, MAT, VUL from first 5 years [Note: 50 years of data]
# Note: MSYcalc function can be used to estimate MSY reference points for various time periods. 
########################################################################################################################

# MSYcalc: function from source.R returns a list with Fmsy, msy, SSBmsy from M=M, waa=weight-at-age, mat=maturity-at-age, sel=vulnerability-at-age, Beverton-Holt a and b
# MSYcalc <- function(M,waa,mat,sel,a,b)

BH_calc_years1_5 <- MSYcalc(M=M,waa=apply(WAA[1:5,],2,mean), mat=apply(MAT[1:5,],2,mean), sel=apply(VUL_BH[1:5,],2,mean),a=BHa, b=BHb)
Ricker_calc_years1_5 <- MSYcalc_Ricker(M=M,waa=apply(WAA[1:5,],2,mean), mat=apply(MAT[1:5,],2,mean), sel=apply(VUL_R[1:5,],2,mean),a=Ra, b=Rb)

calc_last_10_years <- MSYcalc(M=0.3,waa=apply(WAA[41:50,],2,mean), mat=apply(MAT[41:50,],2,mean), sel=apply(VUL_BH[41:50,],2,mean),a=BHa, b=BHb)
Ricker_calc_last_10_years <- MSYcalc_Ricker(M=M,waa=apply(WAA[41:50,],2,mean), mat=apply(MAT[41:50,],2,mean), sel=apply(VUL_R[41:50,],2,mean),a=Ra, b=Rb)

########################################################################################################################
# Plots from data frame D_BH and D_R. Blue plots are BH and Purple plots are Ricker
########################################################################################################################

#Stock recruitment pairs  (labels are years)
ggplot(D_BH[!is.na(D_BH$Rec),],aes(y=Rec,x=SSB,label=Year)) + geom_point(colour="blue") + theme_classic() + labs(x="SSB (kt)", y="Recruitment (10^9)") + expand_limits(y=c(0,12.5)) + expand_limits(x=0) + geom_text(mapping=aes(y=Rec,x=SSB,label=Year),nudge_y = 0.5,size=3) 
ggplot(D_R[!is.na(D_R$Rec),],aes(y=Rec,x=SSB,label=Year)) + geom_point(colour="purple") + theme_classic() + labs(x="SSB (kt)", y="Recruitment (10^9)") + expand_limits(y=c(0,12.5)) + expand_limits(x=0) + geom_text(mapping=aes(y=Rec,x=SSB,label=Year),nudge_y = 0.5,size=3) 

#Stock recruitment pairs and model fit
ggplot() + geom_point(D_BH[!is.na(D_BH$Rec),],mapping=aes(y=Rec,x=SSB),colour="blue") + theme_classic() + labs(x="SSB (kt)", y="Recruitment (10^9)") + expand_limits(y=c(0,12.5)) + expand_limits(x=0) + geom_function(fun=function(x) BHa*x/(1+BHb*x)) 
ggplot() + geom_point(D_R[!is.na(D_R$Rec),],mapping=aes(y=Rec,x=SSB),colour="purple") + theme_classic() + labs(x="SSB (kt)", y="Recruitment (10^9)") + expand_limits(y=c(0,12.5)) + expand_limits(x=0) + geom_function(fun=function(x) Ra*x*exp(-Rb*x)) 

#Historical Recruitment
ggplot() + geom_path(D_BH[!is.na(D_BH$Rec),],mapping=aes(y=Rec,x=Year),colour="blue") + theme_classic() + labs(x="Year", y="Recruitment (10^9)") + expand_limits(y=0,x=50) 
ggplot() + geom_path(D_R[!is.na(D_R$Rec),],mapping=aes(y=Rec,x=Year),colour="purple") + theme_classic() + labs(x="Year", y="Recruitment (10^9)") + expand_limits(y=0,x=50) 

#Historical SSB 
ggplot() + geom_path(D_BH,mapping=aes(y=SSB,x=Year),colour="blue") + theme_classic() + labs(x="Year", y="SSB (kt)") + expand_limits(y=0)
ggplot() + geom_path(D_R,mapping=aes(y=SSB,x=Year),colour="purple") + theme_classic() + labs(x="Year", y="SSB (kt)") + expand_limits(y=0)

#Historical Catch
ggplot(D_BH,aes(y=Catch,x=Year)) + geom_path() + theme_classic() + labs(x="Year", y="Catch (kt)") + expand_limits(y=0)

#Historical F
ggplot() + geom_path(D_BH,mapping=aes(y=f,x=Year),colour="blue") + theme_classic() + labs(x="Year", y="F") + expand_limits(y=0) 
ggplot() + geom_path(D_R,mapping=aes(y=f,x=Year),colour="purple") + theme_classic() + labs(x="Year", y="F") + expand_limits(y=0) 

#Plot Historical SSB with some metrics that could be used for LRPs
#See code from Exercise 3 to generate additional plots.
ggplot(D_BH) + 
  geom_path(mapping=aes(y=SSB,x=Year),colour="blue") +
  theme_classic() + labs(x="Year", y="SSB (kt)") + expand_limits(y=0) +
  geom_hline(yintercept=0.2*SSB0_bh, linetype="dashed", color = "green")  

ggplot(D_R) + 
  geom_path(mapping=aes(y=SSB,x=Year),colour="purple") +
  theme_classic() + labs(x="Year", y="SSB (kt)") + expand_limits(y=0) +
  geom_hline(yintercept=0.2*SSB0_r, linetype="dashed", color = "green")  

