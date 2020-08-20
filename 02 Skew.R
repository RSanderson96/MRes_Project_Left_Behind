
Inv = data.frame(MSOA_Dataset)
Inv$Access_Network = 1/Inv$Access_Network
Inv$Manufacturing_Emp = 1/Inv$Manufacturing_Emp
Inv$L2_or_Higher = 1/Inv$L2_or_Higher
Inv$Internet_Speed = 1/Inv$Internet_Speed
Inv$Total_Income = 1/Inv$Total_Income
Inv$Child_Dev = 1/(Inv$Child_Dev)
Inv$GCSE = 1/Inv$GCSE
Inv$Internet_Speed = 1/Inv$Internet_Speed
is.na(Inv)<-sapply(Inv, is.infinite)
#is.na(Inv$Manufacturing_Emp)<-sapply(Inv$Manufacturing_Emp, is.infinite)
#is.na(Inv$Fibre_Internet)<-sapply(Inv$Fibre_Internet, is.infinite)
Inv[is.na(Inv)]<-0
#Inv$Access_Network[is.na(Inv$Access_Network)]<- max(Inv$Access_Network)
#Inv$Fibre_Internet[is.na(Inv$Fibre_Internet)]<-0
Inv= Inv %>% select (-"Landfill_Prop", )
MSOA_Inv = Inv

Tidy = MSOA_Inv


x = Tidy$Secure_Deposits
Test = log10(x)
Tidy$Secure_Deposits = Test

x = Tidy$Claimants
Test = log10(x)
Tidy$Claimants = Test

x = Tidy$Average_Travel_Time_Walk
Test = log10(x)
Tidy$Average_Travel_Time_Walk = Test

x = Tidy$GP_Coverage
Test = x^(1/3)
Tidy$GP_Coverage = Test


x = Tidy$SecTeachers
#boxplot(x)
Test = x^(1/2)
#boxplot(Test)
Tidy$SecTeachers = Test
#Tidy$SecTeachers[Tidy$SecTeachers > (35)] <- (35)

x = Tidy$Fuel_Poverty
Test = log10(x)
Tidy$Fuel_Poverty = Test

x = Tidy$Percentage_None
Test = log10(x)
Tidy$Percentage_None = Test


x = Tidy$Premature_Rate
Test = x^(1/3)
Tidy$Premature_Rate = Test

x = Tidy$Household_Poverty_BHC
Test = log10(x)
Tidy$Household_Poverty_BHC = Test

x = Tidy$Access_Network
#boxplot(x)
Test = log10(x+0.1)
#Test = x^(1/2)
#Test = x^(1/3)
#boxplot(Test)
Tidy$Access_Network = Test

x = Tidy$Manufacturing_Emp
Test = x^(1/3)
Tidy$Manufacturing_Emp = Test

x = Tidy$Car_to_Train_Av_Time
Test = log10(x)
Tidy$Car_to_Train_Av_Time = Test

x = Tidy$L2_or_Higher
Test = log10(x)
Tidy$L2_or_Higher = Test

x = Tidy$Diff_50
Test = log10(x+1)
Tidy$Diff_50 = Test

#x = Tidy$Recep_Percent
#Test = x^(1/2)
#Tidy$Recep_Percent = Test

#x = Tidy$Internet
#boxplot(x)
#Y = log10((Tidy$Internet)+0.1)
#Y = x^(1/2)
#Y = x^(1/3)
#boxplot(Y)
#Tidy$Internet = Y

x = Tidy$Total_Income
Test = log10(x)
Tidy$Total_Income = Test

Tidy$Smoking_Percent = (Tidy$Smoking_Percent)^0.5

Tidy$Child_Dev = log10(Tidy$Child_Dev)
Tidy$GCSE = log10(Tidy$GCSE)
Tidy$FSM_Prop = (Tidy$FSM_Prop)^0.5
Tidy$Internet_Speed = (Tidy$Internet_Speed)^0.5

Numbers = Tidy[,6:29]
MSOA_Numbers = Numbers


value <- colnames(Numbers)
x = ncol(Numbers)
y = c(1:x)
Stand_Test <-Numbers
for (i in y){
  Stand_Test[,value[i]]<- scale(Numbers[,value[i]])
}

colnames(Stand_Test)[colSums(is.na(Stand_Test)) > 0]
Z_Standardised = Stand_Test
max(Z_Standardised)
min(Z_Standardised)
#Z_Standardised[Z_Standardised < (-3)] <- (-3)
#Z_Standardised[Z_Standardised > (3)] <- (3)
colnames(Z_Standardised)[colSums(is.na(Z_Standardised)) > 0]
MSOA_Z = Z_Standardised



pdf(file = "C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project4/graphs/MSOA_Correlation.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 7) # The height of the plot in inches
corr2 <- cor(MSOA_Numbers)
corrplot(corr2, tl.cex =0.4, tl.col="black", tl.srt=45, type = "lower")
dev.off()