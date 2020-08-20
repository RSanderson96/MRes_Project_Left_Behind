#First clustering - k means with 7 clusters
HC6 <-kmeans(LA_Z,7,nstart = 25, iter.max = 1000,) #Clustering
HCClusters6 <-as.matrix(HC6$cluster) #Extract sizes
HCClusters6 <- as.data.frame(HCClusters6)
HCSizes6 = data.frame(table(HCClusters6)) 

#Making the clusterplot
Final_Clusters = fviz_cluster(HC6, data = LA_Z, geom = "point", ellipse = F, pointsize = 3,
                              ggtheme = theme_classic())+scale_colour_brewer(palette = "Paired")

################Map for K6
LA_Classification <-as.data.frame(cbind((LA_Inverse$LA_Code), HCClusters6))
names(LA_Classification)<-c("LA_Code", "Classification") 
#write.csv(LA_Classification, "C:/Users/b9054751/OneDrive - Newcastle University/PhD/new_project4/data/LA_Classification.csv")
LA_Classification = LA_Classification_File [,2:3]
LA_Classification_Name <-LA_Classification %>% left_join(LA_Dataset[,c(1,2)], by="LA_Code", all = T)
LA_Classification$Classification = as.character(LA_Classification$Classification)
Map_Class_LA = LA_Classification 
Map_Class_LA$Classification[Map_Class_LA$Classification == "1"] <- "A) Thriving Suburbanites"
Map_Class_LA$Classification[Map_Class_LA$Classification == "2"] <- "B) Neglected Rural"
Map_Class_LA$Classification[Map_Class_LA$Classification == "3"] <- "C) Connected Urban"
Map_Class_LA$Classification[Map_Class_LA$Classification == "4"] <- "D) Inner City Disadvantages"
Map_Class_LA$Classification[Map_Class_LA$Classification == "5"] <- "E) Metropolitan Core"
Map_Class_LA$Classification[Map_Class_LA$Classification == "6"] <- "F) Promising Rural"
Map_Class_LA$Classification[Map_Class_LA$Classification == "7"] <- "G) Neglected Urban Fringe"



LA_Class<-merge (x = Local_Authorities, y = Map_Class_LA, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))
LA_Bub_7 = tm_shape(LA_Class) + tm_bubbles(size = 0.1, col = "Classification", palette = "Paired", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)+
  tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)+ tm_layout(main.title = "Local Authority Clustering",main.title.position = "center")+tm_layout(legend.text.size = 0.7) 
#LA_Bub_7 - final LA Map

LA_Full_Classification  <- LA_Classification %>% left_join(Inv, by="LA_Code", all = T)
LA_Full_Classification = LA_Full_Classification  %>% select (-"LA_Code")
LA_Full_Classification$Classification = as.numeric(as.character(LA_Full_Classification$Classification))

#Making the heatmap
means_by_cluster <- data.frame(matrix(ncol = 43, nrow = 0))
x <- colnames(LA_Full_Classification)
colnames(means_by_cluster) <- x
x = 7
y = c(1:x)
for (i in y){
  Cluster = LA_Full_Classification %>%filter(Classification == i)
  Means = c(colMeans(Cluster, na.rm = FALSE, dims = 1))
  means_by_cluster[i,] = Means
}
Mean_By_Col = colMeans(LA_Full_Classification, na.rm = FALSE, dims=1)
sd_by_col<-apply(LA_Full_Classification, 2,sd)
LA_z_clus = scale(means_by_cluster, center = Mean_By_Col, scale = sd_by_col)
LA_z_clus = data.frame(LA_z_clus)
LA_z_clus<-LA_z_clus%>% select (-"Classification", )
LA_z_clus = as.matrix(LA_z_clus)
LA_z_clus = data.frame(LA_z_clus)
LA_z_clus = data.frame(t(LA_z_clus))
pheatmap(LA_z_clus, cluster_cols = T,cluster_rows=T, clustering_distance_cols = "euclidean",clustering_method = "ward",cutree_cols = 4, cutree_rows = 5)

Variable_LA_Z_Clus = data.frame(t(LA_z_clus))

names(Variable_LA_Z_Clus) = c("Poverty_Percentage_BHC", "Secure_Deposit_Households", "Benefits_Claimants", "Recepion_Obesity", "Yr6_Obesity", 
  "Food_Stores_ Walking_Time", "Adult_Obesity", "Free_School_Meals_KS2", "Infant_Death_Rate","GP_Count","Current_Smokers", 
  "Ex_Smokers", "Never_Smoked","Adolescent_Maternities","Suicide_Rates","Premature_Mortality_Rate",
  "Killed_Injured_on_Roads", "Home_Maternities","GCSE_Education","Qualified_L2_or_Higher","Primary_Pupil_Teacher_Ratio", "Secondary _Pupil_Teacher_Ratio", "Proportion_Carers_Female", 
  "Fuel_Poverty", "No_Central_Heating", "Renewable_Energy_Capacity", "GDP_2018","Median_Hourly_Pay", "Gap_Median_Hourly_Pay", "Manufacturing_Employment", "4G_Access_Percentage_Coverage",
  "Land_Consumption_Ratio", "Av_Travel_Time_to_Rail_Station", "Violent_Crime_Offences", "Sexual_Crime_Offences","Fibre_Broadband_Percentage_Coverage",
  "Open_Spaces_Funding", "Access_Network_Score", "Landfill_Proportion_Area","Unpaid_Care_Gender_Difference", "KS1_Education","KS2_Education")



Variable_LA_Z_Clus = data.frame(t(Variable_LA_Z_Clus))
names(Variable_LA_Z_Clus) = c("Thriving Suburbanites", "Neglected Rural", "Connected Urban", "Inner City DIsadvantages",
                              "Metropolitan Core", "Promising Rural", "Neglected Urban Fringe")
pheatmap(Variable_LA_Z_Clus, cluster_cols = T,cluster_rows=T, main = "A Heatmap of the Results of LA Clustering",clustering_distance_cols = "euclidean",clustering_method = "ward",cutree_cols = 4, angle_col = 45, cutree_rows = 5)

#######################Individual Maps
Classification1 <- Map_Class_LA
y = nrow(LA_Classification)
for (i in 1:y){
  if (Classification1[i,2] != "A) Thriving Suburbanites")
  {Classification1[i,2] = "Other"}}

Classification2 <- Map_Class_LA
y = nrow(LA_Classification)
for (i in 1:y){
  if (Classification2[i,2] != "B) Neglected Rural")
  {Classification2[i,2] = "Other"}}

Classification3 <- Map_Class_LA
y = nrow(LA_Classification)
for (i in 1:y){
  if (Classification3[i,2] != "C) Connected Urban")
  {Classification3[i,2] = "Other"}}

Classification4 <- Map_Class_LA
y = nrow(LA_Classification)
for (i in 1:y){
  if (Classification4[i,2] != "D) Inner City Disadvantages")
  {Classification4[i,2] = "Other"}}

Classification5 <- Map_Class_LA
y = nrow(LA_Classification)
for (i in 1:y){
  if (Classification5[i,2] != "E) Metropolitan Core")
  {Classification5[i,2] = "Other"}}

Classification6 <- Map_Class_LA
y = nrow(LA_Classification)
for (i in 1:y){
  if (Classification6[i,2] != "F) Promising Rural")
  {Classification6[i,2] = "Other"}}

Classification7 <- Map_Class_LA
y = nrow(LA_Classification)
for (i in 1:y){
  if (Classification7[i,2] != "G) Neglected Urban Fringe")
  {Classification7[i,2] = "Other"}}




LA_Class1<-merge (x = Local_Authorities, y = Classification1, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))
LA_Class2<-merge (x = Local_Authorities, y = Classification2, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))
LA_Class3<-merge (x = Local_Authorities, y = Classification3, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))
LA_Class4<-merge (x = Local_Authorities, y = Classification4, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))
LA_Class5<-merge (x = Local_Authorities, y = Classification5, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))
LA_Class6<-merge (x = Local_Authorities, y = Classification6, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))
LA_Class7<-merge (x = Local_Authorities, y = Classification7, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))

TM1 = tm_shape(LA_Class1) + tm_bubbles(size = 0.1, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
TM2 = TM1 +tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)
LA_Map1 = TM2+tm_layout(legend.text.size = 0.9)

TM1 = tm_shape(LA_Class2) + tm_bubbles(size = 0.1, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
TM2 = TM1 +tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)
LA_Map2 = TM2+tm_layout(legend.text.size = 0.9)

TM1 = tm_shape(LA_Class3) + tm_bubbles(size = 0.1, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
TM2 = TM1 +tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)
LA_Map3 = TM2+tm_layout(legend.text.size = 0.9)


TM1 = tm_shape(LA_Class4) + tm_bubbles(size = 0.1, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
TM2 = TM1 +tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)
LA_Map4 = TM2+tm_layout(legend.text.size = 0.9)

TM1 = tm_shape(LA_Class5) + tm_bubbles(size = 0.1, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
TM2 = TM1 +tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)
LA_Map5 = TM2+tm_layout(legend.text.size = 0.9)

TM1 = tm_shape(LA_Class6) + tm_bubbles(size = 0.1, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
TM2 = TM1 +tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)
LA_Map6 = TM2+tm_layout(legend.text.size = 0.9)

TM1 = tm_shape(LA_Class7) + tm_bubbles(size = 0.1, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
TM2 = TM1 +tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)
LA_Map7 = TM2+tm_layout(legend.text.size = 0.9)

#Testing making a radial plot
Radial = t(LA_z_clus)
Radial = data.frame(Radial)
Radial[8,]<-c(0)
par(cex.axis = 0.6, cex.lab = 0.5,xpd = TRUE)
radial.plot(Radial[c(7,8),],labels = colnames(Radial),
            boxed.radial = FALSE, show.radial.grid = TRUE,
            line.col = "blue", radlab = TRUE, rp.type="p", show.grid.labels = 3, main = "Cluster 7",
            mar =c(4,3,7,3),  radial.lim = c(-2,2))

#MSOA Clustering - 7 clusters Kmeans - allows comparison

KMZ7<-kmeans(MSOA_Z,7,nstart = 50, iter.max = 1000,) #Clustering
KMClusters7 <-as.matrix(KMZ7$cluster) #identify cluster sizes
KMClusters7 <- as.data.frame(KMClusters7)
Sizes7 = data.frame(table(KMClusters7)) #notice sample size for cluster size
Sizes7 = Sizes7 %>% dplyr::rename(Cluster = KMClusters7, Count_7 = Freq)

MSOA_Classification <-as.data.frame(cbind(as.character(MSOA_Inv$MSOA_Code), KMClusters7))
names(MSOA_Classification)<-c("MSOA_Code", "Classification") 
MSOA_Classification$Classification = as.character(MSOA_Classification$Classification)
MSOA_Full_Classification  <- MSOA_Classification %>% left_join(MSOA_Inv, by="MSOA_Code", all = T)
MSOA_Full_Classification[,2] <- as.numeric(as.character(MSOA_Full_Classification[,2]))
MSOA_Full_Classification = MSOA_Full_Classification  %>% select (-"MSOA_Code", -"MSOA_Area", -"MSOA_Name", -"Population", -"Households")

MSOA_7_Class<-merge (x = MSOA, y = MSOA_Classification, by.x = c("msoa11cd"), by.y = c("MSOA_Code"))



Map = tm_shape(MSOA_7_Class) + tm_bubbles(size = 0.1, col = "Classification", 
                                        palette = "Paired", style = "quantile",
                                        legend.size.show = FALSE, title.col = "Cluster", 
                                        border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
MSOA_Bub7 = Map+ tm_shape(Local_Authorities)+tm_borders("black", lwd = .7,)+ tm_layout(main.title = "MSOA Clustering",main.title.position = "center")

#MSOA Clustering - 11 clusters Kmeans

##############Making 11 clusters


KMZ11 <-kmeans(MSOA_Z,11,nstart = 25, iter.max = 1000,) #Clustering
KMClusters11 <-as.matrix(KMZ11$cluster) #identify cluster sizes
KMClusters11 <- as.data.frame(KMClusters11)
Sizes11 = data.frame(table(KMClusters11)) #notice sample size for cluster size
Sizes11 = Sizes11 %>% dplyr::rename(Cluster = KMClusters11, Count_11 = Freq)

#Cluster plot for 11 MSOA clusters
Final_Clusters = fviz_cluster(KMZ11, data = MSOA_Z, geom = "point", ellipse = F, pointsize = 3,
                              ggtheme = theme_classic())+scale_colour_brewer(palette = "Paired")





MSOA_Classification11 <-as.data.frame(cbind(as.character(MSOA_Inv$MSOA_Code), KMClusters11))
names(MSOA_Classification11)<-c("MSOA_Code", "Classification")   
MSOA_Classification11 = MSOA_Classification_File2[,2:3]
MSOA_Classification11_Name <-MSOA_Classification11 %>% left_join(MSOA_Dataset[,c(1,2)], by="MSOA_Code", all = T)

MSOA_Classification11$Classification = as.character(MSOA_Classification11$Classification)
MSOA_Full_Classification11  <- MSOA_Classification11 %>% left_join(MSOA_Inv, by="MSOA_Code", all = T)
MSOA_Full_Classification11 = MSOA_Full_Classification11  %>% select (-"MSOA_Code", -"MSOA_Area", -"MSOA_Name", -"Population", -"Households")
MSOA_Full_Classification11$Classification <- as.numeric(as.character(MSOA_Full_Classification11$Classification))



MSOA_Classes<- MSOA_Classification11

MSOA_Classes$Classification[MSOA_Classes$Classification == "1"] <- "2b) Fringe Beneficiaries"
MSOA_Classes$Classification[MSOA_Classes$Classification == "2"] <- "2a) Connected Towns"
MSOA_Classes$Classification[MSOA_Classes$Classification == "3"] <- "2c) Vibrant Inner City"
MSOA_Classes$Classification[MSOA_Classes$Classification == "4"] <- "2d) Prosperous Suburbanites"
MSOA_Classes$Classification[MSOA_Classes$Classification == "5"] <- "1a) Rural Isolation"
MSOA_Classes$Classification[MSOA_Classes$Classification == "6"] <- "3a) Inner City Deptivation"
MSOA_Classes$Classification[MSOA_Classes$Classification == "7"] <- "4a) Middling Inner City"
MSOA_Classes$Classification[MSOA_Classes$Classification == "8"] <- "4c) Modest Urban Outskirts"
MSOA_Classes$Classification[MSOA_Classes$Classification == "9"] <- "4b) Flexible Inner City"
MSOA_Classes$Classification[MSOA_Classes$Classification == "10"] <- "3b) Forgotten Urban Fringes"
MSOA_Classes$Classification[MSOA_Classes$Classification == "11"] <- "1b) Remote Towns"

MSOA_11_Class<-merge (x = MSOA, y = MSOA_Classes, by.x = c("msoa11cd"), by.y = c("MSOA_Code"))



Map = tm_shape(MSOA_11_Class) + tm_bubbles(size = 0.1, col = "Classification", 
                                          palette = "Set3", style = "quantile",
                                          legend.size.show = FALSE, title.col = "Cluster", 
                                          border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
Bub11 = Map+ tm_shape(Local_Authorities)+tm_borders("black", lwd = .7,)+ tm_layout(main.title = "MSOA Clustering",main.title.position = "center")

#Bub11 - map of all 11 clusters

#Making the heatmap for MSOA data
means_by_cluster <- data.frame(matrix(ncol = 25, nrow = 0))
x <- colnames(MSOA_Full_Classification11)
colnames(means_by_cluster) <- x
x = 11
y = c(1:x)
for (i in y){
  Cluster = MSOA_Full_Classification11 %>%filter(Classification == i)
  Means = c(colMeans(Cluster, na.rm = FALSE, dims = 1))
  means_by_cluster[i,] = Means
}



Mean_By_Col = colMeans(MSOA_Full_Classification11, na.rm = FALSE, dims=1)
sd_by_col<-apply(MSOA_Full_Classification11, 2,sd)


z_clus = scale(means_by_cluster, center = Mean_By_Col, scale = sd_by_col)
z_clus = data.frame(z_clus)
z_clus<-z_clus%>% select (-"Classification", )
z_clus = as.matrix(z_clus)
z_clus = data.frame(z_clus)
z_clus = data.frame(t(z_clus))
names(z_clus)<-c("2b)Fringe Beneficiaries", "2a) Connected Towns", "2c) Vibrant Inner City", "2d) Prosperous Suburbanites", "1a) Rural Isolation", 
                 "3a) Inner City Deptivation", "4a) Middling Inner City", "4c)Modest Urban Outskirts", "4b) Flexible Inner City", "3b) Forgotten Urban Fringes", "1b) Remote Towns")   

pheatmap(z_clus, cluster_cols = T,cluster_rows=T, cutree_rows = 3,cutree_cols = 4)
pheatmap(z_clus, cluster_cols = T,cluster_rows=T, main = "A Heatmap of the Results of MSOA Clustering",clustering_distance_cols = "euclidean",clustering_method = "ward",cutree_cols = 4, angle_col = 45, cutree_rows = 3)

z_clus_class = z_clus

pheatmap(z_clus_class, cluster_cols = T,cluster_rows=T, cutree_rows = 3,cutree_cols = 5)
pheatmap(z_clus_class, cluster_cols = T,cluster_rows=T, main = "A Heatmap of the Results of MSOA Clustering",clustering_distance_cols = "euclidean",clustering_method = "ward",cutree_cols = 4, angle_col = 45, cutree_rows = )

row.names(z_clus_class) 
row.names(z_clus_class)<- c("Secure_Deposit_Households", "Benefits_Claimants", "Food_Stores_Walking_Time",
                            "GP_Count", "Primary_Pupil_Teacher_Ratio", "Secondary_Pupil_Teacher_Ratio", "Fuel_Poverty",
                            "No_Central_Heating", "Premature_Mortality_Rate", "Poverty_Percentage_BHC", "Access_Network_Score",
                            "Manufacturing_Employment", "Av_Travel_Time_to_Rail_Station", "Qualified_L2_or_Higher",
                            "Proportion_Carers_Female", "Unpaid_Care_Gender_Differences", "Reception_Obesity", "Yr6_Obesity",
                            "Average_Internet_Speed", "Average_Household_Income", "Percentage_Smokers", "Under_5_Child_Development",
                            "GCSE_Education", "Free_School_Meals_Proportion")

###Group Maps
MSOA_Group1<-subset(MSOA_11_Class,(Classification == "1b) Remote Towns" | Classification == "1a) Rural Isolation"))
TM1 = tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)+tm_shape(MSOA_Group1) + 
  tm_bubbles(size = 0.05, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Group 1", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
MSOA_MapGroup1 = TM1

MSOA_Group2<-subset(MSOA_11_Class,(Classification == "2a) Connected Towns" | Classification == "2d) Prosperous Suburbanites"|
                                     Classification == "2c) Vibrant Inner City"| Classification == "2b) Fringe Beneficiaries"))
TM1 = tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)+tm_shape(MSOA_Group2) + 
  tm_bubbles(size = 0.05, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Group 2", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
MSOA_MapGroup2 = TM1
MSOA_MapGroup2

MSOA_Group3<-subset(MSOA_11_Class,(Classification == "3a) Inner City Deptivation" | Classification == "3b) Forgotten Urban Fringes"))
TM1 = tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)+tm_shape(MSOA_Group3) + 
  tm_bubbles(size = 0.05, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Group 3", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
MSOA_MapGroup3 = TM1
MSOA_MapGroup3

MSOA_Group4<-subset(MSOA_11_Class,(Classification == "4a) Middling Inner City" | Classification == "4c) Modest Urban Outskirts"|
                                     Classification == "4b) Flexible Inner City"))
TM1 = tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)+tm_shape(MSOA_Group4) + 
  tm_bubbles(size = 0.05, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Group 4", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)
MSOA_MapGroup4 = TM1
MSOA_MapGroup1


#Making radial plots

Radial2 = t(z_clus)
Radial2 = data.frame(Radial2)
Radial2[12,]<-c(0)

par(cex.axis = 0.6, cex.lab = 0.5,xpd = TRUE)
radial.plot(Radial2[c(6,12),],labels = colnames(Radial2),
            boxed.radial = FALSE, show.radial.grid = TRUE,
            line.col = "blue", radlab = TRUE, rp.type="p", show.grid.labels = 3, main = "Cluster 7",
            mar =c(4,3,7,3),  radial.lim = c(-2,2))

