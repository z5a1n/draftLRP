rm(list=ls())
library(ggplot2)
L <- read.csv("Exercise 1/ex1_landings.csv")
D <- read.csv("Exercise 1/ex1_indices.csv")

head(L)
head(D)

#Calculate CPUE for purse seine fleet in SWNS/BoF
D$CPUE <- (D$PS_Catch_MU1)/D$PS_Effort

############################################################
#Plots
############################################################
#landings by MU
ggplot() + geom_area(data=L, mapping=aes(y=Landings_kt,x=Year,fill=MU)) +
  theme_classic() + labs(x='Year', y="Landings (kt)") +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,300), expand = c(0, 0)) 

#MU1 PS Catch
ggplot(D[!(is.na(D$PS_Catch_MU1)),],aes(y=PS_Catch_MU1,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU1 Purse Seine Landings (kt)") + expand_limits(y=0)

#MU1 PS Effort
ggplot(D[!(is.na(D$PS_Effort_MU1)),],aes(y=PS_Effort_MU1,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU1 Purse Seine Effort (trips)") + expand_limits(y=0)

#MU1 PS CPUE
ggplot(D[!(is.na(D$CPUE)),],aes(y=CPUE,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU1 Purse Seine CPUE (kt/trip)") + expand_limits(y=0)

#MU1 and MU2 Bottom trawl survey
ggplot(D[!(is.na(D$BT_Index_MU1_2)),],aes(y=BT_Index_MU1_2,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU1 and 2 Bottom Trawl Survey Biomass Index (kt)") + expand_limits(y=0) +
  geom_smooth(span=0.25,se=F)

#MU1  Bottom trawl survey
ggplot(D[!(is.na(D$BT_Index_MU1)),],aes(y=BT_Index_MU1,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU1 Bottom Trawl Survey Biomass Index (kt)") + expand_limits(y=0) +
  geom_smooth(span=0.25,se=F) 

#MU1 Acoustic Survey
ggplot(D[!(is.na(D$Ac_Index_MU1)),],aes(y=Ac_Index_MU1,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU1 Acoustic Index of SSB (kt)") + 
  scale_x_continuous(limits = c(1,50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.1*max(D$Ac_Index_MU1,na.rm = T)), expand = c(0, 0))  +
  geom_smooth(span=0.5,se=F)
  
#MU3 Acoustic Surveys
ggplot(D[!(is.na(D$Ac_Index_MU3)),],aes(y=Ac_Index_MU3,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU3 Acoustic Index of SSB (kt)") + 
  scale_x_continuous(limits = c(1,50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.1*max(D$Ac_Index_MU3,na.rm = T)), expand = c(0, 0))  +
  geom_smooth(span=0.5,se=F)






# some options for smoothing indices

#Add x yr moving average (example 3 years)
D$MA_Ac_Index_MU1 <- NA
D$MA_Ac_Index_MU1[D$Year%in%28:50] <- apply(cbind(D$Ac_Index_MU1[D$Year%in%28:50],
                                                  D$Ac_Index_MU1[D$Year%in%27:49],
                                                  D$Ac_Index_MU1[D$Year%in%26:48]),1,mean)

#Add loess smoother (example span = 0.5)
lsmooth <- loess(Ac_Index_MU1 ~ Year,data=D,span=0.5)  
D$lowess_Ac_Index_MU1 <- NA
D$lowess_Ac_Index_MU1[D$Year%in%26:50] <- predict(lsmooth)

ggplot(D[!is.na(D$Ac_Index_MU1),]) + geom_path(mapping=aes(y=Ac_Index_MU1,x=Year)) + theme_classic() + labs(x="Year", y="MU1 Acoustic SSB (kt)") + expand_limits(y=0,x=50) +
  geom_path(data=D[!is.na(D$MA_Ac_Index_MU1),],mapping=aes(y=MA_Ac_Index_MU1,x=Year),color="red") +
  geom_path(data=D[!is.na(D$lowess_Ac_Index_MU1),],mapping=aes(y=lowess_Ac_Index_MU1,x=Year),color="blue") 




