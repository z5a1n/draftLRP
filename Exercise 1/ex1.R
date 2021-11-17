rm(list=ls())
library(ggplot2)
L <- read.csv("Exercise 1/ex1_landings.csv")
D <- read.csv("Exercise 1/ex1_indices.csv")

head(L)
head(D)

#Calculate CPUE for purse seine fleet in SWNS/BoF
D$CPUE <- (D$PS_Catch_MU1)/D$PS_Effort

############################################################
#Plots (entire stock area)
############################################################

#landings by MU
ggplot() + geom_area(data=L, mapping=aes(y=Landings_kt,x=Year,fill=MU)) + theme_classic() + labs(x='Year', y="Landings (kt)") + scale_x_continuous(limits = c(0,50), expand = c(0, 0)) + scale_y_continuous(limits = c(0,300), expand = c(0, 0)) 

#MU1 and MU2 bottom trawl survey
D$MA_1 <- NA
D$MA_1[11:50] <- apply(cbind(D$BT_Index_MU1_2[D$Year%in%11:50],
                             D$BT_Index_MU1_2[D$Year%in%10:49],
                             D$BT_Index_MU1_2[D$Year%in%9:48]),1,mean)

#Add loess smoother (example span = 0.5)
lsmooth1 <- loess(BT_Index_MU1_2 ~ Year,data=D,span=0.5)  
D$lowess1 <- NA
D$lowess1[D$Year%in%9:50] <- predict(lsmooth1)

ggplot(D[!is.na(D$BT_Index_MU1_2),]) + geom_path(mapping=aes(y=BT_Index_MU1_2,x=Year)) + theme_classic() + labs(x="Year", y="MU1 and MU2 Bottom Trawl Survey Index (kt)") + expand_limits(y=0,x=50) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0)) +
  geom_path(data=D[!is.na(D$MA_1),],mapping=aes(y=MA_1,x=Year),color="red") +
  geom_path(data=D[!is.na(D$lowess1),],mapping=aes(y=lowess1,x=Year),color="blue") 

#MU1 and MU3 acoustic survey
D$MA_2 <- NA
D$MA_2[28:50] <- apply(cbind(D$Ac_Index_MU1_3[D$Year%in%28:50],
                             D$Ac_Index_MU1_3[D$Year%in%27:49],
                             D$Ac_Index_MU1_3[D$Year%in%26:48]),1,mean)

#Add loess smoother (example span = 0.5)
lsmooth2 <- loess(Ac_Index_MU1_3 ~ Year,data=D,span=0.5)  
D$lowess2 <- NA
D$lowess2[D$Year%in%26:50] <- predict(lsmooth2)

ggplot(D[!is.na(D$Ac_Index_MU1_3),]) + geom_path(mapping=aes(y=Ac_Index_MU1_3,x=Year)) + theme_classic() + labs(x="Year", y="MU1 and MU3 Acoustic SSB Index (kt)") + expand_limits(y=0,x=50) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0)) +
  geom_path(data=D[!is.na(D$MA_2),],mapping=aes(y=MA_2,x=Year),color="red") +
  geom_path(data=D[!is.na(D$lowess2),],mapping=aes(y=lowess2,x=Year),color="blue") 

############################################################
#Plots (MU1)
############################################################

#MU1 PS Catch
ggplot(D[!(is.na(D$PS_Catch_MU1)),],aes(y=PS_Catch_MU1,x=Year)) + geom_path() + theme_classic() + labs(x='Year', y="MU1 Purse Seine Landings (kt)") + expand_limits(y=0)

#MU1 PS Effort
ggplot(D[!(is.na(D$PS_Effort_MU1)),],aes(y=PS_Effort_MU1,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU1 Purse Seine Effort (trips)") + expand_limits(y=0)

#MU1 PS CPUE
ggplot(D[!(is.na(D$CPUE)),],aes(y=CPUE,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="MU1 Purse Seine CPUE (kt/trip)") + expand_limits(y=0)


#MU1  Bottom trawl survey
D$MA_3 <- NA
D$MA_3[11:50] <- apply(cbind(D$BT_Index_MU1[D$Year%in%11:50],
                             D$BT_Index_MU1[D$Year%in%10:49],
                             D$BT_Index_MU1[D$Year%in%9:48]),1,mean)

#Add loess smoother (example span = 0.5)
lsmooth3 <- loess(BT_Index_MU1 ~ Year,data=D,span=0.5)  
D$lowess3 <- NA
D$lowess3[D$Year%in%9:50] <- predict(lsmooth3)

ggplot(D[!is.na(D$BT_Index_MU1),]) + geom_path(mapping=aes(y=BT_Index_MU1,x=Year)) + theme_classic() + labs(x="Year", y="MU1 Bottom Trawl Survey Index (kt)") + expand_limits(y=0,x=50) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0)) +
  geom_path(data=D[!is.na(D$MA_3),],mapping=aes(y=MA_3,x=Year),color="red") +
  geom_path(data=D[!is.na(D$lowess3),],mapping=aes(y=lowess3,x=Year),color="blue") 

#MU1 Acoustic Survey
D$MA_4 <- NA
D$MA_4[28:50] <- apply(cbind(D$Ac_Index_MU1[D$Year%in%28:50],
                             D$Ac_Index_MU1[D$Year%in%27:49],
                             D$Ac_Index_MU1[D$Year%in%26:48]),1,mean)

#Add loess smoother (example span = 0.5)
lsmooth4 <- loess(Ac_Index_MU1 ~ Year,data=D,span=0.5)  
D$lowess4 <- NA
D$lowess4[D$Year%in%26:50] <- predict(lsmooth4)

ggplot(D[!is.na(D$Ac_Index_MU1),]) + geom_path(mapping=aes(y=Ac_Index_MU1,x=Year)) + theme_classic() + labs(x="Year", y="MU1 Acoustic Index of SSB (kt)") + expand_limits(y=0,x=50) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0)) +
  geom_path(data=D[!is.na(D$MA_4),],mapping=aes(y=MA_4,x=Year),color="red") +
  geom_path(data=D[!is.na(D$lowess4),],mapping=aes(y=lowess4,x=Year),color="blue") 