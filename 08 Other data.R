###############################LA



Brexit = Brexit2
Brexit_Vote  <-merge (x = Map_Class_LA, y = Brexit, by = c("LA_Code"))
#Brexit_Vote[,2] <- as.numeric(as.character(Brexit_Vote[,2]))
Brexit_Vote = Brexit_Vote  %>% select (-"LA_Code", )




Brexit_Box = ggplot(data = Brexit_Vote, aes(x = Classification, y = Pct_Leave))+ 
  geom_boxplot(aes(group = Classification, fill = Classification))+
  #geom_point(aes(x = Classification, y = Pct_Leave, color = Classification))+
  scale_color_brewer(palette="Paired")+ 
  stat_summary(fun=mean, geom="point", shape=23, size=5) +
  theme(legend.position="bottom") + 
  ggtitle("Proportion of 'Leave' Brexit Votes for each cluster") +
  xlab("Cluster") + 
  ylab("Percentage of Leave Votes")
Brexit_Box

####################
IMD =LA_IMD
IMD  <-merge (x = Map_Class_LA, y = IMD, by = c("LA_Code"))
#IMD[,2] <- as.numeric(as.character(IMD[,2]))
IMD = IMD  %>% select (-"LA_Code", -"LA_Name", -"IMD_Av_Rank")

IMD$IMD_LA_Rank = as.numeric(as.character(IMD$IMD_LA_Rank))
IMD$Prop_LSOA_Dep_10 = as.numeric(as.character(IMD$Prop_LSOA_Dep_10))

IMD_Box = ggplot(data = IMD, aes(x = Classification, y = IMD_LA_Rank))+ 
  #geom_boxplot(aes(group = Classification, fill = Classification))+
  geom_point(aes(x = Classification, y = IMD_LA_Rank, color = Classification))+
  scale_color_brewer(palette="Paired")+ 
  stat_summary(fun=mean, geom="point", shape=23, size=5) +
  theme(legend.position="bottom") + 
  ggtitle("IMD Ranking") +
  xlab("Cluster") + 
  ylab("IMD Local Authority Rank")
IMD_Box

IMD1 = IMD %>%filter(Classification == "A) Thriving Suburbanites")
IMD2 = IMD %>%filter(Classification == "B) Neglected Rural")
IMD3 = IMD %>%filter(Classification == "C) Connected Urban")
IMD4 = IMD %>%filter(Classification == "D) Inner City Disadvantages")
IMD5 = IMD %>%filter(Classification == "E) Metropolitan Core")
IMD6 = IMD %>%filter(Classification == "F) Promising Rural")
IMD7 = IMD %>%filter(Classification == "G) Neglected Urban Fringe")

boxplot(IMD3$IMD_LA_Rank)

############
Covid = Covid2[,1:6]
Covid = Covid %>%filter(Cause == "COVID-19")
Covid = Covid %>%filter(Sex == "Persons")
#Covid = Covid %>% rename(Code = LA_Code)
Covid <- Covid %>% right_join(Map_Class_LA, by="LA_Code", all = T)
Covid <- na.omit(Covid)
Covid_Pop = Covid %>% right_join(LA_Pop_Code, by="LA_Code", all = T)
Covid<- Covid[,4:7]
Covid = Covid[,3:4]
Covid$Rate = as.numeric(as.character(Covid$Rate))

Covid_Box = ggplot(data = Covid, aes(x = Classification, y = Rate))+ 
  #geom_boxplot(aes(group = Classification, fill = Classification))+
  geom_point(aes(x = Classification, y = Rate, color = Classification))+
  scale_color_brewer(palette="Paired")+ 
  stat_summary(fun=mean, geom="point", shape=23, size=5) +
  theme(legend.position="bottom") + 
  ggtitle("Rate of Covid19 Deaths for each cluster") +
  xlab("Cluster") + 
  ylab("COVID-19 Death Rate")
Covid_Box

Covid_Pop = Covid_Pop[,c(6:8)]
Covid_Pop$Rate = as.numeric(as.character(Covid_Pop$Rate))
Covid_Pop$LogPopDen = log(Covid_Pop$PopDen)
ggplot(Covid_Pop, aes(x=LogPopDen, y=Rate, color=Classification.x)) + 
  geom_point() + scale_color_brewer(palette="Paired")

###############################MSOA
#MSOA_Brexit = Brexit2


#MSOA_Brexit  <-merge (x = MSOA_Classification11, y = MSOA_Brexit, by = c("LA_Code"))
#MSOA_Brexit[,2] <- as.numeric(as.character(MSOA_Brexit[,2]))
#MSOA_Brexit = MSOA_Brexit  %>% select (-"MSOA_Code", )
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "1"] <- "Cluster1"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "2"] <- "Cluster2"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "3"] <- "Cluster3"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "4"] <- "Cluster4"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "5"] <- "Cluster5"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "6"] <- "Cluster6"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "7"] <- "Cluster7"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "8"] <- "Cluster8"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "9"] <- "Cluster9"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "10"] <- "Cluster10"
#MSOA_Brexit$Classification[MSOA_Brexit$Classification == "11"] <- "Cluster11"



#Brexit_Box = ggplot(data = MSOA_Brexit, aes(x = Classification, y = Pct_Leave))+ 
 # #geom_boxplot(aes(group = Classification, fill = Classification))+
  #geom_point(aes(x = Classification, y = Pct_Leave, color = Classification))+
  #scale_color_brewer(palette="Paired")+ 
  #stat_summary(fun=mean, geom="point", shape=23, size=5) +
  #theme(legend.position="bottom") + 
  #ggtitle("Proportion of 'Leave' Brexit Votes for each cluster") +
  #xlab("Cluster") + 
  #ylab("Percentage of Leave Votes")+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


####################
#IMD = data.frame(LSOA_IMD2)
#MSOA_to_LSOA = data.frame(OA_to_MSOA2[,c(3:6)])
#MSOA_to_LSOA = MSOA_to_LSOA[!duplicated(MSOA_to_LSOA$LSOA_Code), ]
#IMD  <-merge (x = MSOA_to_LSOA, y = IMD, by = c("LSOA_Code"))
#MSOA_IMD = aggregate(IMD_Rank ~ MSOA_code, IMD, mean)

#length(unique(Test_Conversion$MSOA11NM))
       
MSOA_IMD = MSOA_Weighted_IMD
MSOA_IMD  <-merge (x = MSOA_Classes, y = MSOA_IMD, by = c("MSOA_Code"))
#MSOA_IMD[,2] <- as.numeric(as.character(MSOA_IMD[,2]))
#MSOA_IMD$Classification[MSOA_IMD$Classification == "1"] <- "Cluster1"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "2"] <- "Cluster2"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "3"] <- "Cluster3"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "4"] <- "Cluster4"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "5"] <- "Cluster5"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "6"] <- "Cluster6"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "7"] <- "Cluster7"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "8"] <- "Cluster8"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "9"] <- "Cluster9"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "10"] <- "Cluster10"
#MSOA_IMD$Classification[MSOA_IMD$Classification == "11"] <- "Cluster11"

MSOA_IMD_Box = ggplot(data = MSOA_IMD, aes(x = Classification, y = MSOA_Av_Rank))+ 
   #geom_boxplot(aes(group = Classification, fill = Classification))+
  geom_point(aes(x = Classification, y = MSOA_Av_Rank, color = Classification))+
  scale_color_brewer(palette="Set3")+ 
  stat_summary(fun=mean, geom="point", shape=23, size=5) +
  theme(legend.position="bottom") + 
  ggtitle("IMD Ranking") +
  xlab("Cluster") + 
  ylab("IMD MSOA Rank")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

MSOA_IMD_Box
############
#Covid = Covid %>% rename(Code = LA_Code)
MSOA_C19 <- MSOA_Covid %>% right_join(MSOA_Classes, by="MSOA_Code", all = T)
MSOA_C19 <- MSOA_C19 %>% right_join(MSOA_Dataset[,c(1,4)], by="MSOA_Code", all = T)
MSOA_C19['Covid_Rate'] = (MSOA_C19$X3_Months_Covid/MSOA_C19$Population)* 100000 
MSOA_C19 <- MSOA_C19 %>% right_join(MSOA_Pop_Code, by="MSOA_Code", all = T)
MSOA_C19 = MSOA_C19[,c(6:8)]

MSOA_C19$LogPopDen = log(MSOA_C19$PopDen)

# Basic scatter plot
ggplot(MSOA_C19, aes(x=LogPopDen, y=Covid_Rate, color=Classification.y)) + 
  geom_point() + scale_color_brewer(palette="Paired")


#MSOA_C19$Classification = as.numeric(MSOA_C19$Classification)
#MSOA_Covid <- na.omit(MSOA_Covid)
#MSOA_C19$Classification[MSOA_C19$Classification == "1"] <- "Cluster1"
#MSOA_C19$Classification[MSOA_C19$Classification == "2"] <- "Cluster2"
#MSOA_C19$Classification[MSOA_C19$Classification == "3"] <- "Cluster3"
#MSOA_C19$Classification[MSOA_C19$Classification == "4"] <- "Cluster4"
#MSOA_C19$Classification[MSOA_C19$Classification == "5"] <- "Cluster5"
#MSOA_C19$Classification[MSOA_C19$Classification == "6"] <- "Cluster6"
#MSOA_C19$Classification[MSOA_C19$Classification == "7"] <- "Cluster7"
#MSOA_C19$Classification[MSOA_C19$Classification == "8"] <- "Cluster8"
#MSOA_C19$Classification[MSOA_C19$Classification == "9"] <- "Cluster9"
#MSOA_C19$Classification[MSOA_C19$Classification == "10"] <- "Cluster10"
#MSOA_C19$Classification[MSOA_C19$Classification == "11"] <- "Cluster11"
#MSOA_C19 = MSOA_C19[,4:6]
#MSOA_Covid$Rate = as.numeric(as.character(MSOA_Covid$Rate))


MSOA_Covid_Box = ggplot(data = MSOA_C19, aes(x = Classification, y = Covid_Rate))+ 
  geom_boxplot(aes(group = Classification, fill = Classification))+
  #geom_point(aes(x = Classification, y = Covid_Rate, color = Classification))+
  scale_color_brewer(palette="Paired")+ 
  stat_summary(fun=mean, geom="point", shape=23, size=5) +
  theme(legend.position="bottom") + 
  ggtitle("Rate of Covid19 Deaths for each cluster") +
  xlab("Cluster") + 
  ylab("COVID-19 Death Rate")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
MSOA_Covid_Box

