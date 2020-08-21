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


