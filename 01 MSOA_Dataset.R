
Dataset = data.frame(MSOA_Boundaries)
Dataset = Dataset %>%filter(str_detect(MSOA_Code, 'E'))

Population = MSOA_Population
Population= Population %>% select (-"MSOA_Name", )
Dataset  <- Dataset %>% left_join(Population, by="MSOA_Code", all = T)
Dataset$Population <-as.numeric(as.character(Dataset$Population))


Households = MSOA_Households
Households= Households %>% select (-"MSOA_Name", )
Dataset  <- Dataset %>% left_join(Households, by="MSOA_Code", all = T)
Dataset$Households <-as.numeric(as.character(Dataset$Households))


#1) Security Deposits
Deposits = MSOA_Security_Deposit2[,1:3]
Deposits= Deposits %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Deposits, by="MSOA_Code", all = T)
Dataset$Secure_Deposits = ((Dataset$Count)/(Dataset$Households))*100
Dataset= Dataset %>% select (-"Count", )


#2)Claimants
Claimants = MSOA_Claimants
Claimants= Claimants %>% select (-"MSOA_Name", )
names(Claimants)[names(Claimants) == "X2019Av"] <- "Claimants"
Dataset <- Dataset %>% left_join(Claimants, by="MSOA_Code", all = T)
Dataset$Claimants_Percent = ((Dataset$Claimants)/(Dataset$Population))*100
Dataset= Dataset %>% select (-"Claimants", )
names(Dataset)[names(Dataset) == "Claimants_Percent"] <- "Claimants"


#3) Food Store distance: Walk
Food = MSOA_Food_Travel
Food = Food %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Food, by="MSOA_Code", all = T)

#4) GP_Count
GP = MSOA_GP_Count
GP = GP %>% select (-"MSOA_Name", )
Dataset<-Dataset %>% left_join (GP, by = 'MSOA_Code', all=T)
Dataset['GP_Coverage'] = (Dataset$Population/Dataset$GP_Count)
Dataset = Dataset %>% select (-"GP_Count", )
is.na(Dataset$GP_Coverage)<-sapply(Dataset$GP_Coverage, is.infinite)

Dataset$GP_Coverage[is.na(Dataset$GP_Coverage)] <- Dataset$Population[is.na(Dataset$GP_Coverage)]


#5)Primary Teachers

#PriTeachers = MSOA_Primary_Teachers
#PriTeachers= PriTeachers %>% select (-"MSOA_Name", )
#Dataset <- Dataset %>% left_join(PriTeachers, by="MSOA_Code", all = T)
#Dataset$Primary_Teachers = Dataset$Pupils_Headcount/Dataset$Teachers_Headcount
#Dataset= Dataset %>% select (-"Pupils_Headcount" )
#Dataset= Dataset %>% select (-"Teachers_Headcount" )
#Dataset$Primary_Teachers[is.na(Dataset$Primary_Teachers)]<-0
#
PriTeachers = MSOA_Nearby_Primary2
PriTeachers= PriTeachers %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(PriTeachers, by="MSOA_Code", all = T)
Dataset$Primary_Teachers = Dataset$Pupil_Count/Dataset$Teacher_Count
Dataset= Dataset %>% select (-"Pupil_Count" )
Dataset= Dataset %>% select (-"Teacher_Count" )
Dataset$Primary_Teachers[is.na(Dataset$Primary_Teachers)]<-0


#6)Secondary Teachers
SecTeachers = MSOA_Secondary_Teachers
SecTeachers= SecTeachers %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(SecTeachers, by="MSOA_Code", all = T)
Dataset$SecTeachers = Dataset$Pupil_Count/Dataset$Teacher_Count
Dataset= Dataset %>% select (-"Pupil_Count" )
Dataset= Dataset %>% select (-"Teacher_Count" )
Dataset$SecTeachers[is.na(Dataset$SecTeachers)]<-0

#SecTeachers = MSOA_Nearby_Secondary
#SecTeachers= SecTeachers %>% select (-"MSOA_Name", )
#Dataset <- Dataset %>% left_join(SecTeachers, by="MSOA_Code", all = T)
#Dataset$SecTeachers = Dataset$Pupils_Count/Dataset$Teachers_Count
#Dataset= Dataset %>% select (-"Pupils_Count" )
#Dataset= Dataset %>% select (-"Teachers_Count" )
#Dataset$SecTeachers[is.na(Dataset$SecTeachers)]<-0



#7)Fuel Poverty
Fuel= MSOA_Fuel_Poverty
Fuel= Fuel %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Fuel, by="MSOA_Code", all = T)
Dataset$Fuel_Poverty = ((Dataset$Fuel_Poverty_Count)/(Dataset$Households))*100
Dataset= Dataset %>% select (-"Fuel_Poverty_Count", )
Dataset= Dataset %>% select (-"Households_Count", )


#8) Central Heating
CH= MSOA_Central_Heating2
CH= CH %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(CH, by="MSOA_Code", all = T)


#9)Landfill
Landfill= MSOA_Landfill
Landfill = Landfill %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Landfill, by="MSOA_Code", all = T)
Dataset['Landfill_Prop'] = (Dataset$Landfill/Dataset$MSOA_Area)*100
Dataset = Dataset %>% select (-"Landfill_Area")


#10)Premature - need to check code
Premature = Premature_Deaths[,c(2,4)]
Dataset <- Dataset %>% left_join(Premature, by="MSOA_Code", all = T)
Dataset$Premature_Deaths <-as.numeric(as.character(Dataset$Premature_Deaths))
Dataset$Population <-as.numeric(as.character(Dataset$Population))
Dataset['Premature_Rate'] = (Dataset$Premature_Deaths/Dataset$Population)*1000
Dataset = Dataset %>% select (-"Premature_Deaths" )

#11)Proportion Living at 60% median income

Poverty = MSOA_Household_Poverty
Poverty= Poverty  %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Poverty, by="MSOA_Code",all = T)

#12)Access Network
Access= MSOA_Access_Network
Access= Access  %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Access, by="MSOA_Code", all = T)
Dataset$Access_Network = (Dataset$Access_Network/Dataset$MSOA_Area)*100

#13)Manufacturing Emp
Man_Emp=MSOA_Manufacturing
Man_Emp= Man_Emp %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Man_Emp, by="MSOA_Code", all = T)

#14)Train distance
Car= MSOA_Car_to_Train
Car= Car %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Car, by="MSOA_Code", all = T)

#15)L2 or higher

L2 = MSOA_Qualifications
L2 = L2%>% select (-"MSOA_Name")
Dataset <- Dataset %>% left_join(L2, by="MSOA_Code", all = T)

#16,17)Unpaid_Care

Care = MSOA_Care
Care = Care%>% select (-"MSOA_Name")
Dataset <- Dataset %>% left_join(Care, by="MSOA_Code", all = T)


#18)Recep Obesity

#Obesity = MSOA_Obesity
#Obesity = Obesity %>%filter(Area.Type == "MSOA")
#Recep_Ob = Obesity %>%filter(Indicator.Name == "Obese children Reception Year, three year average")
#Recep_Ob = Recep_Ob %>%filter(Time.period == "2016/17 - 18/19")
#Recep_Ob = Recep_Ob[,c(5,6,18,19)]
#names(Recep_Ob)[names(Recep_Ob) == "Area.Code"] <- "MSOA_Code"
#names(Recep_Ob)[names(Recep_Ob) == "Area.Name"] <- "MSOA_Name"
#Recep_Ob$Recep_Percent = (Recep_Ob$Count/Recep_Ob$Denominator)*100
#Recep_Ob = Recep_Ob %>% select (-"Count", -"Denominator" )
#Recep_Ob = Recep_Ob%>% select (-"MSOA_Name")
#Dataset <- Dataset %>% left_join(Recep_Ob, by="MSOA_Code", all = T)

#Excess_Recep =  Local_health %>%filter(Indicator.Name =="Children with excess weight Reception Year, three year average")
#Excess_Recep =  Excess_Recep %>%filter(Area.Type =="MSOA")
#Excess_Recep = Excess_Recep[,c(3,4,11)]
#names(Excess_Recep)<-c("MSOA_Code", "MSOA_Name", "Excess_Recep")
#write.csv(Excess_Recep,"C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project3/data/MSOA_Excess_Recep.csv")

Excess_Recep = MSOA_Excess_Recep[, c(2,4)]
Dataset <- Dataset %>% left_join(Excess_Recep, by="MSOA_Code", all = T)

#19)Yr6 Obesity
#Obesity = MSOA_Obesity
#Obesity = Obesity %>%filter(Area.Type == "MSOA")
#Yr6_Ob = Obesity %>%filter(Indicator.Name == "Obese children Year 6, three year average")
#Yr6_Ob = Yr6_Ob %>%filter(Time.period == "2016/17 - 18/19")
#Yr6_Ob = Yr6_Ob[,c(5,6,18,19)]
#names(Yr6_Ob)[names(Yr6_Ob) == "Area.Code"] <- "MSOA_Code"
#names(Yr6_Ob)[names(Yr6_Ob) == "Area.Name"] <- "MSOA_Name"
#Yr6_Ob$Yr6_Percent = (Yr6_Ob$Count/Yr6_Ob$Denominator)*100
#Yr6_Ob = Yr6_Ob %>% select (-"Count", -"Denominator" )
#Yr6_Ob = Yr6_Ob%>% select (-"MSOA_Name")
#Dataset <- Dataset %>% left_join(Yr6_Ob, by="MSOA_Code", all = T)

#Local_health = read.csv("C:/Users/b9054751/OneDrive - Newcastle University/PhD/MRes/Data/MSOA_Prep/Local Health Indicators.csv")
#Excess_Yr6 =  Local_health %>%filter(Indicator.Name =="Children with excess weight Year 6, three year average")
#Excess_Yr6 =  Excess_Yr6 %>%filter(Area.Type =="MSOA")
#Excess_Yr6 = Excess_Yr6[,c(3,4,11)]
#names(Excess_Yr6)<-c("MSOA_Code", "MSOA_Name", "Excess_Yr6")
#write.csv(Excess_Yr6,"C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project3/data/MSOA_Excess_Yr6.csv")

Excess_Yr6 = MSOA_Excess_Yr6[, c(2,4)]
Dataset <- Dataset %>% left_join(Excess_Yr6, by="MSOA_Code", all = T)
#20)Internet

#Internet = data.frame(OA_Broadband_cut)
#Covert = data.frame(Convert)
#colnames(Covert)<- c("geo_code","MSOA_Code","MSOA_Name")
#Internet <- Internet %>% left_join(Covert, by="geo_code", all = T)
#Internet2 <- Internet %>%
#  group_by(MSOA_Code) %>%
#  summarise(Fibre_Internet_Count = sum(Fibre_Internet), Premises_Count = sum(Premises))
#Internet2$Fibre_Internet = (Internet2$Fibre_Internet_Count/Internet2$Premises_Count)*100

#Internet2 = Internet2[,c(1,4)]
#Dataset <- Dataset %>% left_join(Internet2, by="MSOA_Code", all = T)

#OA_Broadband = read.csv("C:/Users/b9054751/OneDrive - Newcastle University/PhD/MRes/Data/MSOA_Prep/OA_Broadband.csv")
#OA_Broadband = OA_Broadband%>% left_join(Covert, by="geo_code", all = T)
#Test = aggregate(SFBB.availability....premises. ~ MSOA_Code, OA_Broadband, sum)
#Test2 = aggregate(All.Premises ~ MSOA_Code, OA_Broadband, sum)
#OA_Broadband = Test%>% left_join(Test2, by="MSOA_Code", all = T)
#OA_Broadband[,2] <- as.numeric(as.character(OA_Broadband[,2]))
#OA_Broadband[,3] <- as.numeric(as.character(OA_Broadband[,3]))
#OA_Broadband$Internet = (OA_Broadband$SFBB.availability....premises./OA_Broadband$All.Premises)*100
#OA_Broadband = OA_Broadband[,c(1,4)]
#Dataset <- Dataset %>% left_join(OA_Broadband, by="MSOA_Code", all = T)

MSOA_List = Dataset[,c(1,2)]
Internet = MSOA_Broadband_Speeds_2019_2 [,c(1,3)]
names(Internet)<-c("MSOA_Code", "Internet_Speed")
Dataset <- Dataset %>% left_join(Internet, by="MSOA_Code","MSOA_Name", all = T)
Dataset <- Dataset %>% distinct(MSOA_Code, .keep_all = TRUE)


#21) Household Income

Income = data.frame(MSOA_Household_Income2)
Income = Income%>% select (-"MSOA_Name")
Dataset <- Dataset %>% left_join(Income, by="MSOA_Code", all = T)


#22)Smokers

#Smoking = read.csv("C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project3/data/MSOA_Smoking.csv")
#Smoking =  Smoking %>%filter(Indicator =="Smoking prevalence at age 15 - regular smokers (modelled estimates)")
#Smoking = Smoking[,-1]
#Smoking =  Smoking %>%filter(Area.Type =="MSOA")
#Smoking = Smoking[,c(1,2,3,9)]
#write.csv(Smoking,"C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project3/data/MSOA_Smoking2.csv")

Smoking = MSOA_Smoking2
Smoking = Smoking[,c(2,5)]
names(Smoking)<-c("MSOA_Code", "Smoking_Percent")
Dataset <- Dataset %>% left_join(Smoking, by="MSOA_Code", all = T)


#23) Child Dev

#Local_health = read.csv("C:/Users/b9054751/OneDrive - Newcastle University/PhD/MRes/Data/MSOA_Prep/Local Health Indicators.csv")
#Child_Dev =  Local_health %>%filter(Indicator.Name =="Child Development at age 5 (%)")
#Child_Dev =  Child_Dev %>%filter(Area.Type =="MSOA")
#Child_Dev = Child_Dev[,c(3,4,10,11)]
#Child_Dev[["Value"]][is.na(Child_Dev[["Value"]])] <- mean(Child_Dev$Value, na.rm = TRUE)
#names(Child_Dev)<-c("MSOA_Code", "MSOA_Name", "Year", "Child_Dev")
#write.csv(Child_Dev,"C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project3/data/MSOA_Child_Dev.csv")

Child_Dev = MSOA_Child_Dev[,c(2,5)]
names(Child_Dev)<-c("MSOA_Code", "Child_Dev")
Dataset <- Dataset %>% left_join(Child_Dev, by="MSOA_Code", all = T)

#24) GCSE
#Local_health = read.csv("C:/Users/b9054751/OneDrive - Newcastle University/PhD/MRes/Data/MSOA_Prep/Local Health Indicators.csv")
#GCSE =  Local_health %>%filter(Indicator.Name =="GCSE Achievement (5A*-C including English & Maths)")
#GCSE =  GCSE %>%filter(Area.Type =="MSOA")
#GCSE = GCSE[,c(3,4,11)]
#GCSE[["Value"]][is.na(GCSE[["Value"]])] <- mean(GCSE$Value, na.rm = TRUE)
#names(GCSE)<-c("MSOA_Code", "MSOA_Name", "GCSE")
#write.csv(GCSE,"C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project3/data/MSOA_GCSE.csv")
GCSE = MSOA_GCSE[, c(2,4)]
Dataset <- Dataset %>% left_join(GCSE, by="MSOA_Code", all = T)

#25) FSM
FSM = MSOA_FSM
FSM$FSM_Prop = ((FSM$FSM_500m)/(FSM$Pupils_500m))*100
FSM = FSM[,c(1,8)]
Dataset <- Dataset %>% left_join(FSM, by="MSOA_Code", all = T)


#Dataset = Dataset[!Dataset$MSOA_Name == "Isles of Scilly 001", ]
Dataset[["Excess_Recep"]][is.na(Dataset[["Excess_Recep"]])] <- mean(Dataset$Excess_Recep, na.rm = TRUE)
Dataset[["Excess_Yr6"]][is.na(Dataset[["Excess_Yr6"]])] <- mean(Dataset$Excess_Yr6, na.rm = TRUE)

MSOA_Dataset = Dataset
#Smokers - https://fingertips.phe.org.uk/search/smoking/page-options/ovw-do-0#page/9/gid/1/pat/101/par/E09000002/ati/3/are/E02000002/cid/4/tbm/1/page-options/ovw-do-0
#https://fingertips.phe.org.uk/profile/local-health/data#page/9/gid/1938133180/pat/101/par/E09000002/ati/3/are/E02000002/cid/4/page-options/ovw-do-0

