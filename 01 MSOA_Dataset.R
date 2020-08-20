
Dataset = data.frame(MSOA_Boundaries) #File - MSOA names, starts the data-frame
Dataset = Dataset %>%filter(str_detect(MSOA_Code, 'E')) #Select only english dataframes

Population = MSOA_Population #Need population to make variables #Need population to make variables
Population= Population %>% select (-"MSOA_Name", )
Dataset  <- Dataset %>% left_join(Population, by="MSOA_Code", all = T)
Dataset$Population <-as.numeric(as.character(Dataset$Population))


Households = MSOA_Households #Need households to make variables
Households= Households %>% select (-"MSOA_Name", )
Dataset  <- Dataset %>% left_join(Households, by="MSOA_Code", all = T)
Dataset$Households <-as.numeric(as.character(Dataset$Households))


#1) Security Deposits
Deposits = MSOA_Security_Deposit2[,1:3] #Calculates the proportion of households that are occupied by recognised renters
Deposits= Deposits %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Deposits, by="MSOA_Code", all = T)
Dataset$Secure_Deposits = ((Dataset$Count)/(Dataset$Households))*100
Dataset= Dataset %>% select (-"Count", )


#2)Claimants
Claimants = MSOA_Claimants #Calculates the proportion of population that are claiming UC
Claimants= Claimants %>% select (-"MSOA_Name", )
names(Claimants)[names(Claimants) == "X2019Av"] <- "Claimants"
Dataset <- Dataset %>% left_join(Claimants, by="MSOA_Code", all = T)
Dataset$Claimants_Percent = ((Dataset$Claimants)/(Dataset$Population))*100
Dataset= Dataset %>% select (-"Claimants", )
names(Dataset)[names(Dataset) == "Claimants_Percent"] <- "Claimants"


#3) Food Store distance: Walk
Food = MSOA_Food_Travel #import data - no change
Food = Food %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Food, by="MSOA_Code", all = T)

#4) GP_Count
GP = MSOA_GP_Count #Calculates the ratio of patients to GP
GP = GP %>% select (-"MSOA_Name", )
Dataset<-Dataset %>% left_join (GP, by = 'MSOA_Code', all=T)
Dataset['GP_Coverage'] = (Dataset$Population/Dataset$GP_Count)
Dataset = Dataset %>% select (-"GP_Count", )
is.na(Dataset$GP_Coverage)<-sapply(Dataset$GP_Coverage, is.infinite)

Dataset$GP_Coverage[is.na(Dataset$GP_Coverage)] <- Dataset$Population[is.na(Dataset$GP_Coverage)]


#5)Primary Teachers
PriTeachers = MSOA_Nearby_Primary2 #Calculates the ratio of pupils to teachers
PriTeachers= PriTeachers %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(PriTeachers, by="MSOA_Code", all = T)
Dataset$Primary_Teachers = Dataset$Pupil_Count/Dataset$Teacher_Count
Dataset= Dataset %>% select (-"Pupil_Count" )
Dataset= Dataset %>% select (-"Teacher_Count" )
Dataset$Primary_Teachers[is.na(Dataset$Primary_Teachers)]<-0


#6)Secondary Teachers
SecTeachers = MSOA_Secondary_Teachers #Calculates the ratio of pupils to teachers
SecTeachers= SecTeachers %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(SecTeachers, by="MSOA_Code", all = T)
Dataset$SecTeachers = Dataset$Pupil_Count/Dataset$Teacher_Count
Dataset= Dataset %>% select (-"Pupil_Count" )
Dataset= Dataset %>% select (-"Teacher_Count" )
Dataset$SecTeachers[is.na(Dataset$SecTeachers)]<-0

#7)Fuel Poverty
Fuel= MSOA_Fuel_Poverty #proportion of households in poverty
Fuel= Fuel %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Fuel, by="MSOA_Code", all = T)
Dataset$Fuel_Poverty = ((Dataset$Fuel_Poverty_Count)/(Dataset$Households))*100
Dataset= Dataset %>% select (-"Fuel_Poverty_Count", )
Dataset= Dataset %>% select (-"Households_Count", )


#8) Central Heating
CH= MSOA_Central_Heating2 #import dataset
CH= CH %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(CH, by="MSOA_Code", all = T)


#9)Landfill
Landfill= MSOA_Landfill #proportion of area covered with landfill
Landfill = Landfill %>% select (-"MSOA_Name", )
Dataset <- Dataset %>% left_join(Landfill, by="MSOA_Code", all = T)
Dataset['Landfill_Prop'] = (Dataset$Landfill/Dataset$MSOA_Area)*100
Dataset = Dataset %>% select (-"Landfill_Area")


#10)Premature - need to check code
Premature = Premature_Deaths[,c(2,4)] #death rate
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
Access= MSOA_Access_Network #score by area
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
Excess_Recep = MSOA_Excess_Recep[, c(2,4)]
Dataset <- Dataset %>% left_join(Excess_Recep, by="MSOA_Code", all = T)

#19)Yr6 Obesity
Excess_Yr6 = MSOA_Excess_Yr6[, c(2,4)]
Dataset <- Dataset %>% left_join(Excess_Yr6, by="MSOA_Code", all = T)

#20)Internet
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

Smoking = MSOA_Smoking2
Smoking = Smoking[,c(2,5)]
names(Smoking)<-c("MSOA_Code", "Smoking_Percent")
Dataset <- Dataset %>% left_join(Smoking, by="MSOA_Code", all = T)


#23) Child Dev
Child_Dev = MSOA_Child_Dev[,c(2,5)]
names(Child_Dev)<-c("MSOA_Code", "Child_Dev")
Dataset <- Dataset %>% left_join(Child_Dev, by="MSOA_Code", all = T)

#24) GCSE
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

