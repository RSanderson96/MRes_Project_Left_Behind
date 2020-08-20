
#################################LA Population data
LA_Pop = LA_Dataset[,c(1,3,4)]
#Original_Pop$MSOA_Area = as.numeric(as.character(Dataset$MSOA_Area))
LA_Pop$PopDen = LA_Pop$Population/((LA_Pop$Area)/1000)
LA_Pop = LA_Pop[,c(1,4)]
LA_Pop  <- LA_Pop %>% left_join(Map_Class_LA, by="LA_Code", all = T)
LA_Pop_Code = LA_Pop
LA_Pop = LA_Pop  %>% select (-"LA_Code", )

#LA_Pop$Classification[LA_Pop$Classification == "1"] <- "A) Thriving Suburbanites"
#LA_Pop$Classification[LA_Pop$Classification == "2"] <- "B) Neglected Rural"
#LA_Pop$Classification[LA_Pop$Classification == "3"] <- "C) Connected Urban"
#LA_Pop$Classification[LA_Pop$Classification == "4"] <- "D) Inner City Disadvantages"
#LA_Pop$Classification[LA_Pop$Classification == "5"] <- "E) Metropolitan Core"
#LA_Pop$Classification[LA_Pop$Classification == "6"] <- "F) Promising Rural"
#LA_Pop$Classification[LA_Pop$Classification == "7"] <- "G) Neglected Urban Fringe"


#################Plot - population density


LA_Pop$LogPopDen = log(LA_Pop$PopDen)

ggplot(LA_Pop, aes(x=PopDen)) + geom_histogram(binwidth=1)
ggplot(LA_Pop, aes(x=LogPopDen)) + geom_histogram(binwidth=0.6)

LA_Box = ggplot(data = LA_Pop, aes(x = Classification, y = LogPopDen, fill = Classification))+ 
  geom_boxplot(aes(group = Classification))+
  scale_fill_brewer(palette="Paired")+ 
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  theme(legend.position="bottom") + 
  ggtitle("Population Density for Each Cluster") +
  xlab("Cluster") + 
  ylab("Population Density (Population per Km^2")
LA_Box




LA_Population = LA_Dataset[,c(1,4)]
LA_Population  <- LA_Population %>% left_join(LA_Classification, by="LA_Code", all = T)
LA_Population[,2] <- as.numeric(as.character(LA_Population[,2]))
LA_Population = LA_Population  %>% select (-"LA_Code", )
Pop1 = LA_Population %>%filter(Classification == 1)
Pop2 = LA_Population %>%filter(Classification == 2)
Pop3 = LA_Population %>%filter(Classification == 3)
Pop4 = LA_Population %>%filter(Classification == 4)
Pop5 = LA_Population %>%filter(Classification == 5)
Pop6 = LA_Population %>%filter(Classification == 6)
Pop7 = LA_Population %>%filter(Classification == 7)


LA_Populations= c(sum(Pop1[, 'Population']),
               sum(Pop2[, 'Population']),
               sum(Pop3[, 'Population']),
               sum(Pop4[, 'Population']),
               sum(Pop5[, 'Population']),
               sum(Pop6[, 'Population']),
               sum(Pop7[, 'Population'])
)

LA_PopulationDF = data.frame(Cluster = c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7"), Population = LA_Populations)


LA_Proportion = (LA_PopulationDF[,2]/(sum(LA_PopulationDF$Population)))*100
LA_Proportion = round(LA_Proportion, digits = 1)
LA_PopulationDF = cbind(LA_PopulationDF, LA_Proportion)


LA_PopulationDF <- LA_PopulationDF %>% 
  mutate(Population = "Population")


LA_Households = LA_Dataset[,c(1,5)]
LA_Households  <- LA_Households %>% left_join(LA_Classification, by="LA_Code", all = T)
LA_Households[,2] <- as.numeric(as.character(LA_Households[,2]))
LA_Households = LA_Households  %>% select (-"LA_Code", )
Households1 = LA_Households %>%filter(Classification == 1)
Households2 = LA_Households %>%filter(Classification == 2)
Households3 = LA_Households %>%filter(Classification == 3)
Households4 = LA_Households%>%filter(Classification == 4)
Households5 = LA_Households %>%filter(Classification == 5)
Households6 = LA_Households %>%filter(Classification == 6)
Households7 = LA_Households %>%filter(Classification == 7)

LA_HouseCount= c(sum(Households1[, 'Households']),
              sum(Households2[, 'Households']),
              sum(Households3[, 'Households']),
              sum(Households4[, 'Households']),
              sum(Households5[, 'Households']),
              sum(Households6[, 'Households']),
              sum(Households7[, 'Households'])
)


LA_HousingDF = data.frame(Cluster = c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7"), Households = LA_HouseCount)

LA_House_Prop = (LA_HousingDF[,2]/(sum(LA_HousingDF$Households)))*100
LA_House_Prop = round(LA_House_Prop, digits = 1)
LA_House_Prop = cbind(LA_HousingDF, LA_House_Prop)

LA_House_Prop <- LA_House_Prop %>% 
  mutate(Households = "Households")

LACount = c(nrow(Households1),nrow(Households2), nrow(Households3), nrow(Households4), nrow(Households5), nrow(Households6), nrow(Households7))
LADF = data.frame(Cluster = c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7"), Local_Authorities = LACount)

LAProp = (LADF[,2]/(sum(LADF$Local_Authorities)))*100
LAProp = round(LAProp, digits = 1)
LAProp = cbind(LADF, LAProp)
LAProp <- LAProp %>% 
  mutate(Local_Authorities = "LA")

LA_House_Prop = LA_House_Prop %>% dplyr::rename(Variable = Households,, Percent = LA_House_Prop )
LAProp = LAProp %>% dplyr::rename(Variable = Local_Authorities, Percent = LAProp)
LA_PopulationDF = LA_PopulationDF %>% dplyr::rename(Variable = Population, Percent = LA_Proportion)
LA_Proportions = rbind(LA_PopulationDF, LA_House_Prop, LAProp)

LA_Proportions$Cluster[LA_Proportions$Cluster == "Cluster1"] <- "A) Thriving Suburbanites"
LA_Proportions$Cluster[LA_Proportions$Cluster == "Cluster2"] <- "B) Neglected Rural"
LA_Proportions$Cluster[LA_Proportions$Cluster == "Cluster3"] <- "C) Connected Urban"
LA_Proportions$Cluster[LA_Proportions$Cluster == "Cluster4"] <- "D) Inner City Disadvantages"
LA_Proportions$Cluster[LA_Proportions$Cluster == "Cluster5"] <- "E) Metropolitan Core"
LA_Proportions$Cluster[LA_Proportions$Cluster == "Cluster6"] <- "F) Promising Rural"
LA_Proportions$Cluster[LA_Proportions$Cluster == "Cluster7"] <- "G) Neglected Urban Fringe"

PP = ggplot(LA_Proportions, aes(x = Variable, y = Percent, fill = Cluster)) +
  geom_col(width = 0.9) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab("Variable") +
  labs(title=("Proportion of Variables in Each Cluster ( Local Authorities)"), size = 0.1) +
  theme(plot.title = element_text(size=15, face = "bold"))

PP

#####MSOA - area vs popn vs households
MSOA_Population = MSOA_Dataset[,c(1,3,4)]
MSOA_Population  <- MSOA_Population %>% left_join(MSOA_Classification, by="MSOA_Code", all = T)
#MSOA_Population[,2] <- as.numeric(as.character(Population[,2]))
MSOA_Population = MSOA_Population  %>% select (-"MSOA_Code", )
Pop1 = MSOA_Population %>%filter(Classification == 1)
Pop2 = MSOA_Population %>%filter(Classification == 2)
Pop3 = MSOA_Population %>%filter(Classification == 3)
Pop4 = MSOA_Population %>%filter(Classification == 4)
Pop5 = MSOA_Population %>%filter(Classification == 5)
Pop6 = MSOA_Population %>%filter(Classification == 6)
Pop7 = MSOA_Population %>%filter(Classification == 7)
MSOA_Populations= c(sum(Pop1[, 'Population']),
               sum(Pop2[, 'Population']),
               sum(Pop3[, 'Population']),
               sum(Pop4[, 'Population']),
               sum(Pop5[, 'Population']),
               sum(Pop6[, 'Population']),
               sum(Pop7[, 'Population'])
)
PopulationDF = data.frame(Cluster = c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7"), Population = MSOA_Populations)
Proportion = (PopulationDF[,2]/(sum(PopulationDF$Population)))*100
Proportion = round(Proportion, digits = 1)
PopulationDF = cbind(PopulationDF, Proportion)
PopulationDF <- PopulationDF %>% 
  mutate(Population = "Population")
Households = MSOA_Dataset[,c(1,5)]
Households  <- Households %>% left_join(MSOA_Classification, by="MSOA_Code", all = T)
Households[,2] <- as.numeric(as.character(Households[,2]))
Households = Households  %>% select (-"MSOA_Code", )
Households1 = Households %>%filter(Classification == 1)
Households2 = Households %>%filter(Classification == 2)
Households3 = Households %>%filter(Classification == 3)
Households4 = Households%>%filter(Classification == 4)
Households5 = Households %>%filter(Classification == 5)
Households6 = Households %>%filter(Classification == 6)
Households7 = Households %>%filter(Classification == 7)
HouseCount= c(sum(Households1[, 'Households']),
              sum(Households2[, 'Households']),
              sum(Households3[, 'Households']),
              sum(Households4[, 'Households']),
              sum(Households5[, 'Households']),
              sum(Households6[, 'Households']),
              sum(Households7[, 'Households'])
)
HousingDF = data.frame(Cluster = c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7"), Households = HouseCount)
Prop = (HousingDF[,2]/(sum(HousingDF$Households)))*100
Prop = round(Prop, digits = 1)
Prop = cbind(HousingDF, Prop)
Prop <- Prop %>% 
  mutate(Households = "Households")
MSOACount = c(nrow(Households1),nrow(Households2), nrow(Households3), nrow(Households4), nrow(Households5), nrow(Households6), nrow(Households7))
MSOADF = data.frame(Cluster = c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7"), MSOA = MSOACount)

MSOAProp = (MSOADF[,2]/(sum(MSOADF$MSOA)))*100
MSOAProp = round(MSOAProp, digits = 1)
MSOAProp = cbind(MSOADF, MSOAProp)
MSOAProp <- MSOAProp %>% 
  mutate(MSOA = "MSOA")

Prop = Prop %>% dplyr::rename(Variable = Households,, Percent = Prop )
MSOAProp = MSOAProp %>% dplyr::rename(Variable = MSOA, Percent = MSOAProp)
PopulationDF = PopulationDF %>% dplyr::rename(Variable = Population, Percent = Proportion)
Proportions6 = rbind(PopulationDF, Prop, MSOAProp)

MSOA_PP = ggplot(Proportions6, aes(x = Variable, y = Percent, fill = Cluster)) +
  geom_col(width = 0.9) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab("Variable") +
  labs(title=("Proportion of Variables in Each Cluster (MSOA)"), size = 0.1) +
  theme(plot.title = element_text(size=15, face = "bold"))



MSOA_PP


#####MSOA 11 clusters

#####
#################################MSOA Population data
MSOA_Pop = MSOA_Dataset[,c(1,3,4)]
MSOA_Pop$PopDen = MSOA_Pop$Population/((MSOA_Pop$MSOA_Area)/1000)
MSOA_Pop = MSOA_Pop[,c(1,4)]
MSOA_Pop  <- MSOA_Pop %>% left_join(MSOA_Classes, by="MSOA_Code", all = T)
MSOA_Pop_Code = MSOA_Pop
MSOA_Pop = MSOA_Pop  %>% select (-"MSOA_Code", )
MSOA_Pop$Classification[MSOA_Pop$Classification == "1"] <- "2b) Fringe Beneficiaries"
MSOA_Pop$Classification[MSOA_Pop$Classification == "2"] <- "2a) Connected Towns"
MSOA_Pop$Classification[MSOA_Pop$Classification == "3"] <- "2c) Vibrant Inner City"
MSOA_Pop$Classification[MSOA_Pop$Classification == "4"] <- "2d) Prosperous Suburbanites"
MSOA_Pop$Classification[MSOA_Pop$Classification == "5"] <- "1a) Rural Isolation"
MSOA_Pop$Classification[MSOA_Pop$Classification == "6"] <- "3a) Inner City Deptivation"
MSOA_Pop$Classification[MSOA_Pop$Classification == "7"] <- "4a) Middling Inner City"
MSOA_Pop$Classification[MSOA_Pop$Classification == "8"] <- "4c) Modest Urban Outskirts"
MSOA_Pop$Classification[MSOA_Pop$Classification == "9"] <- "4b) Flexible Inner City"
MSOA_Pop$Classification[MSOA_Pop$Classification == "10"] <- "3b) Forgotten Urban Fringes"
MSOA_Pop$Classification[MSOA_Pop$Classification == "11"] <- "1b) Remote Towns"



MSOA_Pop$LogPopDen = log(MSOA_Pop$PopDen)

ggplot(MSOA_Pop, aes(x=PopDen)) + geom_histogram(binwidth=1)
ggplot(MSOA_Pop, aes(x=LogPopDen)) + geom_histogram(binwidth=0.2)

#################Plot - population density
Box = ggplot(data = MSOA_Pop, aes(x = Classification, y = LogPopDen, fill = Classification))+ 
  geom_boxplot(aes(group = Classification))+
  scale_fill_brewer(palette="Set3")+ 
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  theme(legend.position="bottom") + 
  ggtitle("Population Density for Each Cluster") +
  xlab("Cluster") + 
  ylab("Population Density (Population per Km^2")
Box

