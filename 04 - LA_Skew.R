Original_Variables= LA_Dataset[,-c(2:4)]

Original_Variables = Original_Variables[, -c(2,17,44,57,11,4)]
drop = c("Culture_Per_Capita","Pub_to_Train_Av_Time", "Neonatal_Rate", "FSM_KS4_Percent","GVA_Pounds", "Average_Count_30_Walk")
Original_Variables = Original_Variables[,!(names(Original_Variables) %in% drop)]
Original_Variables$Unpaid_Carers_Time = Original_Variables$Proportion_Women_50hrs - Original_Variables$Proportion_Men_50hrs 
drop = c("Proportion_Women_50hrs", "Proportion_Men_50hrs")
Original_Variables = Original_Variables[,!(names(Original_Variables) %in% drop)]
Original_Variables['KS1_Achievement'] = (Original_Variables$KS1_Maths +Original_Variables$KS1_Reading) / 2
Original_Variables['KS2_Achievement'] = (Original_Variables$KS2_Maths +Original_Variables$KS2_Reading) / 2
drop = c("KS1_Maths", "KS1_Reading")
Original_Variables = Original_Variables[,!(names(Original_Variables) %in% drop)]
drop = c("KS2_Maths", "KS2_Reading")
Original_Variables = Original_Variables[,!(names(Original_Variables) %in% drop)]


#"Culture_Per_Capita", 
Inv = data.frame(Original_Variables)
Inv$Renewable = 1/Inv$Renewable
Inv$Access_Network = 1/Inv$Access_Network
Inv$Manufacturing_Emp = 1/Inv$Manufacturing_Emp
Inv$GCSE_Percent=1/Inv$GCSE_Percent
Inv$KS1_Achievement = 1/Inv$KS1_Achievement
Inv$KS2_Achievement = 1/Inv$KS2_Achievement
Inv$Open_Spaces = 1/Inv$Open_Spaces
Inv$X4G_prem_in_4 = 1/Inv$X4G_prem_in_4
Inv$All_Median_Pay = 1/Inv$All_Median_Pay
Inv$GDP_2018 = 1/Inv$GDP_2018
Inv$Fibre_Internet = 1/Inv$Fibre_Internet
Inv$Never_Percent = 1/Inv$Never_Percent
Inv$L2_or_Higher = 1/Inv$L2_or_Higher

is.na(Inv)<-sapply(Inv, is.infinite)
Inv[is.na(Inv)]<-0

LA_Inverse = Inv


x = Inv$Mean_Poverty_BHC
Test = log10(x)
Inv$Mean_Poverty_BHC = Test

x = Inv$Secure_Deposits
Test = log10(x)
Inv$Secure_Deposits = Test


x = Inv$Claimants
Test = log10(x)
Inv$Claimants = Test


x = Inv$Average_Travel_Time_Walk
Test = log10(x)
Inv$Average_Travel_Time_Walk = Test

x = Inv$FSM_KS2_Percent
Test = log10(x)
Inv$FSM_KS2_Percent = Test

x = Inv$Infant_Rate
Test = (x)^(1/2)
Inv$Infant_Rate = Test

x = Inv$GP_Coverage
Test = (x)^(1/2)
Inv$GP_Coverage = Test

x = Inv$Never_Percent
Test = log10(x)
Inv$Never_Percent = Test

x = Inv$Adolecent_Births
Test = (x)^(1/3)
Inv$Adolecent_Births = Test

x = Inv$Suicide_Rate
Test = (x)^(1/3)
Inv$Suicide_Rate = Test

x = Inv$KSI_Rate
Test = log10(x)
Inv$KSI_Rate = Test

x = Inv$Home_Maternities
Test = log10(x)
Inv$Home_Maternities = Test

x = Inv$L2_or_Higher
Test = log10(x)
Inv$L2_or_Higher = Test

x = Inv$Secondary_Teachers
Test = log10(x)
Inv$Secondary_Teachers = Test

x = Inv$Central_Heating
Test = log10(x)
Inv$Central_Heating = Test

x = Inv$Renewable
Test = log10(x)
Inv$Renewable = Test

x = Inv$Manufacturing_Emp
Test = log10(x)
Inv$Manufacturing_Emp = Test

x = Inv$X4G_prem_in_4
Test = log10(x)
Inv$X4G_prem_in_4 = Test

x = Inv$Land_Consumption_Ratio
Test = log10(x)
Inv$Land_Consumption_Ratio = Test

x = Inv$Car_to_Train_Av_Time
Test = log10(x)
Inv$Car_to_Train_Av_Time = Test

x = Inv$Violent_Crime
Test = (x)^(1/2)
Inv$Violent_Crime = Test

x = Inv$Sexual_Offences
Test = log10(x)
Inv$Sexual_Offences = Test

x = Inv$Fibre_Internet
Test = log10(x+0.01)
Inv$Fibre_Internet = Test


Inv$Open_Spaces[Inv$Open_Spaces < (-0.24)] <- (-0.00495)
x = Inv$Open_Spaces
Test = log10(x+0.05)
Inv$Open_Spaces = Test

x = Inv$Access_Network
Test = log10(x)
Inv$Access_Network = Test

x = Inv$Landfill_Prop
Test = log10(x+0.1)
Inv$Landfill_Prop = Test

x = Inv$Unpaid_Carers_Time
Test = log10(x)
Inv$Unpaid_Carers_Time = Test

x = Inv$KS1_Achievement
Test = log10(x)
Inv$KS1_Achievement = Test

x = Inv$KS2_Achievement
Test = log10(x)
Inv$KS2_Achievement = Test


(skewness(Inv[,2:43], na.rm = TRUE))


#value <- colnames(Inv)
#x = ncol(Inv)
#y = c(2:x)
#Log_Test <-Inv
#for (i in y){
#  Log_Test[,value[i]]<- (log10((Inv[,value[i]])+1))
  #Log_Test[,value[i]]<- ((Log_Test[,value[i]])^(1/2))
  
#}

#Log_Test$Open_Spaces[Log_Test$Open_Spaces < (-0.24)] <- (-0.00495)


Numbers = Inv[,2:43]
LA_Numbers = Inv[,2:43]
#(skewness(Numbers, na.rm = TRUE))
#colnames(Numbers)[colSums(is.na(Numbers)) > 0]

names(LA_Numbers) = c("Poverty_Percentage_BHC", "Secure_Deposit_Households", "Benefits_Claimants", "Recepion_Obesity", "Yr6_Obesity", 
                              "Food_Stores_ Walking_Time", "Adult_Obesity", "Free_School_Meals_KS2", "Infant_Death_Rate","GP_Count","Current_Smokers", 
                              "Ex_Smokers", "Never_Smoked","Adolescent_Maternities","Suicide_Rates","Premature_Mortality_Rate",
                              "Killed_Injured_on_Roads", "Home_Maternities","GCSE_Education","Qualified_L2_or_Higher","Primary_Pupil_Teacher_Ratio", "Secondary _Pupil_Teacher_Ratio", "Proportion_Carers_Female", 
                              "Fuel_Poverty", "No_Central_Heating", "Renewable_Energy_Capacity", "GDP_2018","Median_Hourly_Pay", "Gap_Median_Hourly_Pay", "Manufacturing_Employment", "4G_Access_Percentage_Coverage",
                              "Land_Consumption_Ratio", "Av_Travel_Time_to_Rail_Station", "Violent_Crime_Offences", "Sexual_Crime_Offences","Fibre_Broadband_Percentage_Coverage",
                              "Open_Spaces_Funding", "Access_Network_Score", "Landfill_Proportion_Area","Unpaid_Care_Gender_Difference", "KS1_Education","KS2_Education")



value <- colnames(Numbers)
x = ncol(Numbers)
y = c(1:x)
Stand_Test <-Numbers
for (i in y){
  Stand_Test[,value[i]]<- scale(Numbers[,value[i]])
}

Z_Standardised = Stand_Test#%>% select (-"Open_Spaces", )
max(Z_Standardised)
min(Z_Standardised)
Z_Standardised[Z_Standardised < (-3)] <- (-3)
Z_Standardised[Z_Standardised > (3)] <- (3)

LA_Z = Z_Standardised

pdf(file = "C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project4/graphs/Correlation.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 7) # The height of the plot in inches
corr2 <- cor(LA_Numbers)
corrplot(corr2, tl.cex =0.4, tl.col="black", tl.srt=45, type = "lower")
dev.off()
