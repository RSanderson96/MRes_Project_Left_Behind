#Comparisons

MSOA_Lookup = Postcode_Lookup[,c(7:15)]
OA_Region = OA_to_Region_Lookup[,c(1:3)]
names(OA_Region) = c("oa11", "Reg_Code", "Reg_Name")
MSOA_Lookup  <- MSOA_Lookup %>% left_join(OA_Region, by="oa11", all = T)
MSOA_Lookup = MSOA_Lookup %>%filter(str_detect(msoa11cd, 'E'))
MSOA_Lookup = MSOA_Lookup [!duplicated(MSOA_Lookup[c(3)]),]
MSOA_Lookup = MSOA_Lookup[,-c(1:2)]
MSOA_Lookup = MSOA_Lookup[,c(1,2,4,5,8,9)]
names(LAD_to_Region) = c("LA_Code", "LA_Name", "Reg_Code", "Reg_Name")
names(MSOA_Lookup) = c("MSOA_Code", "LA_Code", "MSOA_Name", "LA_Name","Reg_Code", "Reg_Name")


MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Bournemouth"] <- "Bournemouth, Christchurch and Poole"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Christchurch"] <- "Bournemouth, Christchurch and Poole"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Poole"] <- "Bournemouth, Christchurch and Poole"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "East Dorset"] <- "Dorset"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "North Dorset"] <- "Dorset"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Purbeck"] <- "Dorset"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "West Dorset"] <- "Dorset"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Suffolk Coastal"] <- "East Suffolk"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Waveney"] <- "East Suffolk"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Forest Heath"] <- "West Suffolk"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "St Edmundsbury"] <- "West Suffolk"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Taunton Deane"] <- "Somerset West and Taunton"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "West Somerset"] <- "Somerset West and Taunton"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Shepway"] <- "Folkestone and Hythe"
MSOA_Lookup$LA_Name[MSOA_Lookup$LA_Name == "Weymouth and Portland"] <- "Dorset"




#How many LA clusters are in each region? 
LA_Class_Region  <-merge (x = Map_Class_LA, y = LAD_to_Region, by = c("LA_Code"))
PerRegions = unique(LA_Class_Region$Reg_Name)



Unique = vector()
y = 1:(length(PerRegions))
for(i in y){
  Reg = LA_Class_Region %>%filter(Reg_Name == PerRegions[i])
  Unique[i] = length(unique(Reg$Classification))
}

Unique = data.frame(Unique)
PerRegions = cbind(Unique,PerRegions)

PerRegions[order(PerRegions$Unique),]

LA_Class_Region2 <- LA_Class_Region %>% 
  group_by(Classification,Reg_Name) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count))*100)


LA_Class_Region2$perc =  round(LA_Class_Region2$perc , digits = 1)

ggplot(LA_Class_Region2, aes(x = Classification, y = perc, fill = Reg_Name)) +
  geom_col() +
  geom_text(aes(label = paste0(perc, "%")),
             position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 16) +
  ylab("Percent") +
  xlab("Classification") +
  labs(title=("Proportion of Variables in Each Cluster"), size = 0.1) +
  theme(plot.title = element_text(size=15, face = "bold"))+
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5, hjust=1))

Region_Class <- LA_Class_Region %>% 
  group_by(Reg_Name,Classification) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count))*100)


Region_Class$perc =  round(Region_Class$perc , digits = 0)

ggplot(Region_Class, aes(x = Reg_Name, y = perc, fill = Classification)) +
  geom_col() +
  geom_text(aes(label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_size = 16) +
  ylab("Percent") +
  xlab("Classification") +
  labs(title=("Proportion of Variables in Each Cluster"), size = 0.1) +
  theme(plot.title = element_text(size=15, face = "bold"))+
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5, hjust=1))


LA_Class_Count <- LA_Class_Region %>% 
  group_by(Classification) %>% 
  summarise(count=n())

LA_Count_Plot = ggplot(LA_Class_Count, aes(x = Classification, y = count, fill = Classification)) +
  geom_col() +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_size = 16) +
  ylab("Count") +
  xlab("Classification") +
  labs(title=("Proportion of Variables in Each Cluster"), size = 0.1) +
  theme(plot.title = element_text(size=10, face = "bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
LA_Count_Plot


#How many MSOA clusters are in each region? 
MSOA_Class_region <-merge (x = MSOA_Classes, y = MSOA_Lookup, by = c("MSOA_Code"))
MSOA_Dist <- MSOA_Class_region %>% 
  group_by(Classification,Reg_Name) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count))*100)

MSOA_Dist$perc =  round(MSOA_Dist$perc , digits = 0)


MSOA_Dist_plot = ggplot(MSOA_Dist, aes(x = Classification, y = perc, fill = Reg_Name)) +
  geom_col() +
  geom_text(aes(label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 16) +
  ylab("Percent") +
  xlab("Classification") +
  labs(title=("Proportion of Variables in Each Cluster"), size = 0.1) +
  theme(plot.title = element_text(size=10, face = "bold"))+
  theme(axis.text.x = element_text(angle = 90, size = 10)) 
MSOA_Dist_plot

Reg_MSOA <- MSOA_Class_region %>% 
  group_by(Reg_Name,Classification) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count))*100)
Reg_MSOA$perc =  round(Reg_MSOA$perc , digits = 0)

ggplot(Reg_MSOA, aes(x = Reg_Name, y = perc, fill = Classification)) +
  geom_col() +
  geom_text(aes(label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 16) +
  ylab("Percent") +
  xlab("Classification") +
  labs(title=("Proportion of Variables in Each Cluster"), size = 0.1) +
  theme(plot.title = element_text(size=10, face = "bold"))+
  theme(axis.text.x = element_text(angle = 90, size = 10)) 


MSOA_Class_Count <- MSOA_Class_region %>% 
  group_by(Classification) %>% 
  summarise(count=n())

MSOA_Count_plot = ggplot(MSOA_Class_Count, aes(x = Classification, y = count, fill = Classification)) +
  geom_col() +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 16) +
  ylab("Count") +
  xlab("Classification") +
  labs(title=("Proportion of Variables in Each Cluster"), size = 0.1) +
  theme(plot.title = element_text(size=10, face = "bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
MSOA_Count_plot

###How many MSOAs for each LA?

MSOA_LA_Class <-merge (x = MSOA_Classes, y =MSOA_Lookup , by = c("MSOA_Code"))
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "1"] <- "2b) Fringe Beneficiaries"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "2"] <- "2a) Connected Towns"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "3"] <- "2c) Vibrant Inner City"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "4"] <- "2d) Prosperous Suburbanites"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "5"] <- "1a) Rural Isolation"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "6"] <- "3a) Inner City Deptivation"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "7"] <- "4a) Middling Inner City"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "8"] <- "4c) Modest Urban Outskirts"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "9"] <- "4b) Flexible Inner City"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "10"] <- "3b) Forgotten Urban Fringes"
#MSOA_LA_Class$Classification.x[MSOA_LA_Class$Classification.x == "11"] <- "1b) Remote Towns"
LA = unique(MSOA_LA_Class$LA_Name)

Test = MSOA_Lookup %>% left_join(LA_England, by="LA_Name", all = T)

Unique = vector()
y = 1:(length(LA))
for(i in y){
  Reg = MSOA_LA_Class %>%filter(LA_Name == LA[i])
  Unique[i] = length(unique(Reg$Classification))
}

Unique = data.frame(Unique)
LA_Count = cbind(Unique,LA)

LA_Top = LA_Count %>% top_n(12, Unique)
 LA_Top[order(LA_Top$Unique),]

 #How many MSOA clusters in each LA cluster?
 
MSOA_LA_Class  <-merge (x = MSOA_LA_Class, y = LA_Classification_Name, by = c("LA_Name"))
MSOA_LA_Class2 <- MSOA_LA_Class %>% 
   group_by(Classification.y,Classification.x) %>% 
   summarise(count=n()) %>% 
   mutate(perc=count/sum(count))



MSOA_LA_Class2$Classification.y[MSOA_LA_Class2$Classification.y == "1"] <- "A) Thriving Suburbanites"
MSOA_LA_Class2$Classification.y[MSOA_LA_Class2$Classification.y == "2"] <- "B) Neglected Rural"
MSOA_LA_Class2$Classification.y[MSOA_LA_Class2$Classification.y == "3"] <- "C) Connected Urban"
MSOA_LA_Class2$Classification.y[MSOA_LA_Class2$Classification.y == "4"] <- "D) Inner City Disadvantages"
MSOA_LA_Class2$Classification.y[MSOA_LA_Class2$Classification.y == "5"] <- "E) Metropolitan Core"
MSOA_LA_Class2$Classification.y[MSOA_LA_Class2$Classification.y == "6"] <- "F) Promising Rural"
MSOA_LA_Class2$Classification.y[MSOA_LA_Class2$Classification.y == "7"] <- "G) Neglected Urban Fringe"

ggplot(MSOA_LA_Class2, aes(x = Classification.y, y = perc, fill = Classification.x)) +
   geom_col() +
   #geom_text(aes(label = paste0(Percent, "%")),
  #           position = position_stack(vjust = 0.5)) +
   scale_fill_brewer(palette = "Set3") +
   theme_minimal(base_size = 16) +
   ylab("Percent") +
   xlab("Classification") +
   labs(title=("Proportion of Variables in Each Cluster"), size = 0.1) +
   theme(plot.title = element_text(size=15, face = "bold"))+
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5, hjust=1))
 
MSOA_LA_Class3 <- MSOA_LA_Class %>% 
  group_by(Classification.x,Classification.y) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

MSOA_LA_Class3$Classification.y[MSOA_LA_Class3$Classification.y == "1"] <- "A) Thriving Suburbanites"
MSOA_LA_Class3$Classification.y[MSOA_LA_Class3$Classification.y == "2"] <- "B) Neglected Rural"
MSOA_LA_Class3$Classification.y[MSOA_LA_Class3$Classification.y == "3"] <- "C) Connected Urban"
MSOA_LA_Class3$Classification.y[MSOA_LA_Class3$Classification.y == "4"] <- "D) Inner City Disadvantages"
MSOA_LA_Class3$Classification.y[MSOA_LA_Class3$Classification.y == "5"] <- "E) Metropolitan Core"
MSOA_LA_Class3$Classification.y[MSOA_LA_Class3$Classification.y == "6"] <- "F) Promising Rural"
MSOA_LA_Class3$Classification.y[MSOA_LA_Class3$Classification.y == "7"] <- "G) Neglected Urban Fringe"


ggplot(MSOA_LA_Class3, aes(x = Classification.x, y = perc, fill = Classification.y)) +
  geom_col() +
  #geom_text(aes(label = paste0(Percent, "%")),
  #           position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 16) +
  ylab("Percent") +
  xlab("Classification") +
  labs(title=("Proportion of Variables in Each Cluster"), size = 0.1) +
  theme(plot.title = element_text(size=15, face = "bold"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,size = 10, hjust=1))


