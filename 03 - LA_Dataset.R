
#1,2,3)

LA = LA_2019_UK
names(LA)[names(LA) == "lad19cd"] <- "LA_Code"
names(LA)[names(LA) == "lad19nm"] <- "LA_Name"
LA_England = LA %>%filter(str_detect(LA_Code, 'E'))

Dataset = LA_England[,3:4]
Dataset['Area'] = LA_England['Shape_Area']
#Dataset[,3] <-as.numeric(as.character(Dataset[,3]))

#4)
Population = LA_2018_Population.Estimates
Population = Population  %>% select (-"Row", )
Population = Population  %>% select (-"LA_Name", )
Dataset  <- Dataset %>% left_join(Population, by="LA_Code", all = T)
#Dataset[,4] <-as.numeric(as.character(Dataset[,4]))

#5)
Households = LA_Household_Count2017
Households= Households  %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Households, by="LA_Code", all = T)
names(Dataset)[names(Dataset) == "X2017"] <- "Households"

#Dataset[,5] <-as.numeric(as.character(Dataset[,5]))

#6,7)Proportion Living at 60% median income

Poverty = LA_Poverty_AHC_BHC
Poverty= Poverty  %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Poverty, by="LA_Code", all = T)
#Dataset[,6] <-as.numeric(as.character(Dataset[,6]))
#Dataset[,7] <-as.numeric(as.character(Dataset[,7]))

#8)Number of Secure Deposits per 100 people

Deposits = LA_Count_Secure_Deposits
Deposits= Deposits %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Deposits, by="LA_Code", all = T)
#Dataset$Count_Secure_Deposits <-as.numeric(as.character(Dataset[,8]))
Dataset$Secure_Deposits = ((Dataset$Count_Secure_Deposits)/(Dataset$Households))*100

Dataset= Dataset %>% select (-"Count_Secure_Deposits", )

#9)Claimants
Claimants = LA_Claimants_Count
Claimants= Claimants %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Claimants, by="LA_Code", all = T)
#Dataset$Population <-as.numeric(as.character(Dataset[,4]))
#Dataset$Claimants <-as.numeric(as.character(Dataset[,9]))
Dataset$Claimants_Percent = ((Dataset$Claimants)/(Dataset$Population))*100

Dataset= Dataset %>% select (-"Claimants", )
names(Dataset)[names(Dataset) == "Claimants_Percent"] <- "Claimants"
#Dataset[,9] <-as.numeric(as.character(Dataset[,9]))


#10)Obesity in Reception
Recep_Ob = LA_Reception_Obesity
Recep_Ob = Recep_Ob %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Recep_Ob, by="LA_Code", all = T)
names(Dataset)[names(Dataset) == "Obesity"] <- "Recep_Ob"

#Dataset[,10] <-as.numeric(as.character(Dataset[,10]))


#11)Obesity in Yr 6
Yr6_ob = LA_Yr6_Obesity
#Test <- Dataset %>% left_join(Yr6_ob, by="LA_Code", all = T)
Yr6_ob = Yr6_ob %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Yr6_ob, by="LA_Code", all = T)
#Dataset[,11] <-as.numeric(as.character(Dataset[,11]))


#12,13,14) Food Store distance: Car
Food = LA_Food_Store_Distance2
#Test <- Dataset %>% left_join(Food, by="LA_Code", all = T)
Food = Food %>% select (-"LA_Name", )

Dataset <- Dataset %>% left_join(Food, by="LA_Code", all = T)
#Dataset[,12] <-as.numeric(as.character(Dataset[,12]))
#Dataset[,13] <-as.numeric(as.character(Dataset[,13]))
#Dataset[,14] <-as.numeric(as.character(Dataset[,14]))


#15)Obese Adults
Adult_Ob = LA_Obese_Adults
#Test <- Dataset %>% left_join(Adult_Ob, by="LA_Code", all = T)
Adult_Ob = Adult_Ob %>% select (-"LA_Name", )

Dataset <- Dataset %>% left_join(Adult_Ob, by="LA_Code", all = T)
#Dataset[,15] <-as.numeric(as.character(Dataset[,15]))

#16)Free School Meals

FSM = LA_Free_School_Meals
#Test <- Dataset %>% left_join(FSM, by="LA_Code", all = T)
FSM = FSM %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(FSM, by="LA_Code", all = T)
#Dataset[,16] <-as.numeric(as.character(Dataset[,16]))
#Dataset[,17] <-as.numeric(as.character(Dataset[,17]))


#18)Child Mortality
CM= LA_Child_mortality
Test <- Dataset %>% left_join(CM, by="LA_Code", all = T)
CM = CM %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(CM, by="LA_Code", all = T)
#Dataset[,18] <-as.numeric(as.character(Dataset[,18]))
#Dataset[,19] <-as.numeric(as.character(Dataset[,19]))

#20)Unintentional Mortality
UM = LA_Unintentional_Poisoning
Test <- Dataset %>% left_join(UM, by="LA_Code", all = T)
UM = UM %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(UM, by="LA_Code", all = T)
#Dataset$Total_Deaths_Unintentional_Poisoning <-as.numeric(as.character(Dataset[,20]))
Dataset['UM_Rate'] = (Dataset$Total_Deaths_Unintentional_Poisoning/Dataset$Population)*100000
Dataset = Dataset %>% select (-"Total_Deaths_Unintentional_Poisoning")
#Dataset[,20] <-as.numeric(as.character(Dataset[,20]))

#21)GP Count

GP = LA_GP_Count_Table
GP = GP %>% select (-"LA_Name", )
Dataset<-Dataset %>% left_join (GP, by = 'LA_Code', all=T)
#Dataset[,21] <-as.numeric(as.character(Dataset[,21]))
Dataset['GP_Coverage'] = (Dataset$Population/Dataset$GP_Count)
Dataset = Dataset %>% select (-"GP_Count", )
#Dataset[,21] <-as.numeric(as.character(Dataset[,21]))

#22,23,24)Smoking
Smokers = LA.2018.Smokers.Percent
Test <- Dataset %>% left_join(Smokers, by="LA_Code", all = T)
Smokers = Smokers %>% select (-"LA_Name")
Smokers = Smokers %>% select (-"X")
Dataset <- Dataset %>% left_join(Smokers, by="LA_Code", all = T)
#Dataset[,22] <-as.numeric(as.character(Dataset[,22]))
#Dataset[,23] <-as.numeric(as.character(Dataset[,23]))
#Dataset[,24] <-as.numeric(as.character(Dataset[,24]))

#25)Adolescent Birth Rate
ABR = LA_Adolescent_Birth_Rate
Test <- Dataset %>% left_join(ABR, by="LA_Code", all = T)
ABR = ABR[,4:5]
Dataset <- Dataset %>% left_join(ABR, by="LA_Code", all = T)
#Dataset[,25] <-as.numeric(as.character(Dataset[,25]))

#26)Suicide

Suicide = LA_Suicide_Mortality_Rates
Suicide = Suicide %>% select (-"LA_Name")
Dataset <- Dataset %>% left_join(Suicide, by="LA_Code", all = T)
#Dataset$Suicides <-as.numeric(as.character(Dataset[,25]))
Dataset['Suicide_Rate'] = (Dataset$V4_0/Dataset$Population)*100000
Dataset = Dataset %>% select (-"V4_0")
#Dataset = Dataset %>% select (-"Suicides")
#Dataset[,26] <-as.numeric(as.character(Dataset[,26]))


#27)Premature
Premature = LA_Total_Cardio_etc_Deaths
Premature = Premature[,6:7]
Dataset <- Dataset %>% left_join(Premature, by="LA_Code", all = T)
#Dataset[,27] <-as.numeric(as.character(Dataset[,27]))

#28)Road Accidents
KSI = LA_KSI
KSI = KSI%>% select (-"LA_Name")
Dataset <- Dataset %>% left_join(KSI, by="LA_Code", all = T)
#Dataset[,28] <-as.numeric(as.character(Dataset[,28]))


#29)Home Maternities
Home_Maternities = LA_Home_Maternities
Home_Maternities = Home_Maternities[,7:8]
Dataset <- Dataset %>% left_join(Home_Maternities, by="LA_Code", all = T)
#Dataset[,29] <-as.numeric(as.character(Dataset[,29]))


##30,31)Primary Attainment

Primary = LA_.Primary.Attainment
Primary = Primary%>% select (-"LA_Name")
Dataset <- Dataset %>% left_join(Primary, by="LA_Code", all = T)
#Dataset[,30] <-as.numeric(as.character(Dataset[,30]))
#Dataset[,31] <-as.numeric(as.character(Dataset[,31]))

#32)GCSE Attainment

GCSE = LA_GCSE_Attainment
GCSE = GCSE%>% select (-"LA_Code")
GCSE = GCSE%>% select (-"Count_A_Star_to_C_EnglishMaths")
Dataset <- Dataset %>% left_join(GCSE, by="LA_Name", all = T)
#Dataset[,32] <-as.numeric(as.character(Dataset[,32]))

#33,34)KS2 Attainment

KS2Maths = LA_KS2_Maths3
drop = c("Pupil_Count","Number_Expected_Standard","Number_Higher_Standard")
KS2Maths = KS2Maths[,!(names(KS2Maths) %in% drop)]
KS2Maths = KS2Maths%>% select (-"LA_Name")
Dataset <- Dataset %>% left_join(KS2Maths, by="LA_Code", all = T)
#Dataset[,33] <-as.numeric(as.character(Dataset[,33]))


KS2Read = LA_KS2_Reading
drop = c("Eligible","Expected_Standard","Higher_Standard")
KS2Read = KS2Read[,!(names(KS2Read) %in% drop)]
KS2Read = KS2Read%>% select (-"LA_Code")
Dataset <- Dataset %>% left_join(KS2Read, by="LA_Name", all = T)
#Dataset[,34] <-as.numeric(as.character(Dataset[,34]))

#35)L2 or higher

L2 = LA_L2_or_higher
L2 = L2%>% select (-"LA_Name")
Dataset <- Dataset %>% left_join(L2, by="LA_Code", all = T)
#Dataset[,35] <-as.numeric(as.character(Dataset[,35]))


#36)Primary Teachers

PriTeachers = LA_Teachers_Pupils_Primary
PriTeachers = PriTeachers[,4:5]
Dataset <- Dataset %>% left_join(PriTeachers, by="LA_Code", all = T)
#Dataset[,36] <-as.numeric(as.character(Dataset[,36]))

#37)Secondary Teachers
SecTeachers = LA_Teachers_Pupils_Secondary
SecTeachers = SecTeachers[,4:5]
Dataset <- Dataset %>% left_join(SecTeachers, by="LA_Code", all = T)
#Dataset[,37] <-as.numeric(as.character(Dataset[,37]))


#38, 39, 40)Unpaid_Care

Care = LA_Care_All
Care = Care%>% select (-"LA_Name")
Dataset <- Dataset %>% left_join(Care, by="LA_Code", all = T)
#Dataset[,38] <-as.numeric(as.character(Dataset[,38]))
#Dataset[,39] <-as.numeric(as.character(Dataset[,39]))
#Dataset[,40] <-as.numeric(as.character(Dataset[,40]))

#41)Fuel Poverty
Fuel= LA_Fuel_Poverty
Fuel= Fuel %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Fuel, by="LA_Code", all = T)
#Dataset$Fuel_Poverty<-as.numeric(as.character(Dataset[,41]))
Dataset$Fuel_Poverty = ((Dataset$Households_FP_Count)/(Dataset$Households))*100
Dataset= Dataset %>% select (-"Households_FP_Count", )
#Dataset[,41] <-as.numeric(as.character(Dataset[,41]))

#42) Central Heating
CH= LA_Central_Heating
CH = CH[,10:11]
Dataset <- Dataset %>% left_join(CH, by="LA_Code", all = T)
#Dataset[,42] <-as.numeric(as.character(Dataset[,42]))

#43)Renewable Energy

RE= LA_Renewable_Energy_Capacity
RE= RE %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(RE, by="LA_Code", all = T)
#Dataset$Renewable <-as.numeric(as.character(Dataset[,43]))
Dataset$Renewable <-(Dataset$Renew_Capacity.MWH *1000)/Dataset$Population
Dataset= Dataset %>% select (-"Renew_Capacity.MWH", )
#Dataset[,43] <-as.numeric(as.character(Dataset[,43]))


#44) GDP
GDP= LA_GDP.Per.Capita
GDP= GDP %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(GDP, by="LA_Code", all = T)
#Dataset[,44] <-as.numeric(as.character(Dataset[,44]))


#45)Median Pay
Pay_All= LA_Median_Hourly_All_Update
Pay_All= Pay_All %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Pay_All, by="LA_Code", all = T)
#Dataset[,45] <-as.numeric(as.character(Dataset[,45]))

#46)Median Pay Gap
Pay_Gap= LA_Median_Pay_Gap_Update
Pay_Gap= Pay_Gap %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Pay_Gap, by="LA_Code", all = T)
#Dataset[,46] <-as.numeric(as.character(Dataset[,46]))


#47)Scientists
Scientists= LA_Scientists_Count
Scientists= Scientists %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Scientists, by="LA_Code", all = T)
#Dataset$Scientists <-as.numeric(as.character(Dataset[,47]))
Dataset['Scientists'] = (Dataset$Scientists_Count/Dataset$Population)*10000
Dataset = Dataset %>% select (-"Scientists_Count")
#Dataset[,47] <-as.numeric(as.character(Dataset[,47]))


#48)Manufacturing Emp
Man_Emp=LA_Manufacturing_Emp
Test <- Dataset %>% left_join(Man_Emp, by="LA_Code", all = T)
Man_Emp= Man_Emp %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Man_Emp, by="LA_Code", all = T)
#Dataset[,48] <-as.numeric(as.character(Dataset[,48]))

#49)Manufacturing GVA
Man_GVA= LA_Manufacturing_GVA
Test <- Dataset %>% left_join(Man_GVA, by="LA_Code", all = T)
Man_GVA = Man_GVA[,4:5]
Dataset <- Dataset %>% left_join(Man_GVA, by="LA_Code", all = T)
#Dataset[,49] <-as.numeric(as.character(Dataset[,49]))


#50)Mobile Coverage
Mobiles= LA_Mobile_Phone_Coverage
Mobiles= Mobiles %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Mobiles, by="LA_Code", all = T)
#Dataset[,50] <-as.numeric(as.character(Dataset[,50]))


#51)Land Consumption
LC= LA_Land_Consumption
LC= LC %>% select (-"LA_Code", )
Dataset <- Dataset %>% left_join(LC, by="LA_Name", all = T)
#Dataset[,51] <-as.numeric(as.character(Dataset[,51]))


#52,3)Train distance
Car= LA_Car_to_Train
Pub= LA_Public_Transport_to_Train
Car= Car %>% select (-"LA_Name", )
Pub= Pub%>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Car, by="LA_Code", all = T)
Dataset <- Dataset %>% left_join(Pub, by="LA_Code", all = T)
#Dataset[,52] <-as.numeric(as.character(Dataset[,52]))
#Dataset[,53] <-as.numeric(as.character(Dataset[,53]))


#54)Culture_Spending
Culture= LA_Culture_Spending
Culture= Culture %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Culture, by="LA_Code", all = T)
#Dataset[,54]<-as.numeric(as.character(Dataset[,54]))
Dataset$Culture_Per_Capita = ((Dataset$Culture_Heritage_Spending/Dataset$Population)*1000)
Dataset = Dataset %>% select(-"Culture_Heritage_Spending", )
#Dataset[,54] <-as.numeric(as.character(Dataset[,54]))


#55)Violent Crime
Crime= LA_Violent_Crime_Update
Crime= Crime %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Crime, by="LA_Code", all = T)
#Dataset[,55] <-as.numeric(as.character(Dataset[,55]))


#56)Sexual Offences
SO= LA_Sexual_Offences
SO= SO %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(SO, by="LA_Code", all = T)
#Dataset[,56] <-as.numeric(as.character(Dataset[,56]))

#Recycling

#Recycling = read.csv("LA_Recycling.csv")
#Recycling2018  = filter(Recycling , Year == "2018-19")
#Test2018 <- LA %>% left_join(Recycling, by="LA_Code", all = T)

#Recycling2017  = filter(Recycling , Year == "2017-18")
#Test2017 <- LA %>% left_join(Recycling2017, by="LA_Code", all = T)

#57)Internet Access
Internet= LA_Internet_Access2
Internet= Internet %>% select(-LA_Name)
Dataset <- Dataset %>% left_join(Internet, by="LA_Code", all = T)
#Dataset[,57] <-as.numeric(as.character(Dataset[,57]))

#58)Green Space Spending
Open_Space = LA_Open_Spaces_Spending
Open_Space = Open_Space[,1:3]
Open_Space= Open_Space %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Open_Space, by="LA_Code", all = T)
#Dataset[,58] <-as.numeric(as.character(Dataset[,58]))

#59)Access Network
Access= LA_Access_Network
Access= Access[,4:5]
Dataset <- Dataset %>% left_join(Access, by="LA_Code", all = T)
#Dataset[,59] <-as.numeric(as.character(Dataset[,59]))

#60)Homicides
Homicides= LA_Homicides
Test <- Dataset %>% left_join(Homicides, by="LA_Code", all = T)
Homicides= Homicides[,3:4]
Dataset <- Dataset %>% left_join(Homicides, by="LA_Code", all = T)
#Dataset[,60] <-as.numeric(as.character(Dataset[,60]))
Dataset['Homicide_Rate'] = (Dataset$Homicide_Count/Dataset$Population)*100000
Dataset = Dataset %>% select (-"Homicide_Count")
#Dataset[,60] <-as.numeric(as.character(Dataset[,60]))

#61)Landfill
Landfill= LA_Landfill_Area
Landfill = Landfill %>% select (-"LA_Name", )
Dataset <- Dataset %>% left_join(Landfill, by="LA_Code", all = T)
#Dataset[,61] <-as.numeric(as.character(Dataset[,61]))
Dataset['Landfill_Prop'] = (Dataset$Landfill/Dataset$Area)*10000
Dataset = Dataset %>% select (-"Landfill")


Dataset = data.frame(Dataset)
Dataset = Dataset[!Dataset$LA_Name == "City of London", ]
Dataset = Dataset[!Dataset$LA_Name == "Isles of Scilly", ]
Dataset = Dataset[!duplicated(Dataset$LA_Code),]


Dataset[["Neonatal_Rate"]][is.na(Dataset[["Neonatal_Rate"]])] <- mean(Dataset$Neonatal_Rate, na.rm = TRUE)
Dataset[["Home_Maternities"]][is.na(Dataset[["Home_Maternities"]])] <- mean(Dataset$Home_Maternities, na.rm = TRUE)
Dataset[["Violent_Crime"]][is.na(Dataset[["Violent_Crime"]])] <-mean(Dataset$Violent_Crime, na.rm = TRUE)
Dataset[["Sexual_Offences"]][is.na(Dataset[["Sexual_Offences"]])] <-mean(Dataset$Sexual_Offences, na.rm = TRUE)



value <- colnames(Dataset)
x = ncol(Dataset)
y = c(3:x)
Number <-Dataset
for (i in y){
  Number[,value[i]]<- as.numeric(as.character(Number[,value[i]]))
}

LA_Dataset =  Number





