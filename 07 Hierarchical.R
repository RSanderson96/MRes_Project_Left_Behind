#Mapping 2 H clusters
HC2 <- eclust(LA_Z, "hclust", k = 2,metric = "euclidean",
              method = "complete", graph = FALSE) #Clustering
HCClusters2 <-as.matrix(HC2$cluster) #Extract sizes
HCClusters2 <- as.data.frame(HCClusters2)
HCSizes2 = data.frame(table(HCClusters2)) #notice sample size for cluster size
Classification2 <-as.data.frame(cbind(as.character(LA_Inverse$LA_Code), HCClusters2[,1]))
names(Classification2)<-c("LA_Code", "Classification")   
HClass2<-merge (x = Local_Authorities, y = Classification2, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))
Map_Bub_2 = tm_shape(HClass2) + tm_bubbles(size = 0.2, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)+
  tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)

#Split new clusters into 2
Class_2 = data.frame(LA_Z)
Class_2$Classification =  HCClusters2[,1]
Cluster_1 = Class_2 %>%filter(Classification == 1)
Cluster_2 = Class_2 %>%filter(Classification == 2)

Class_2 = Class_2  %>% select (-"Classification", )
Cluster_1 = Cluster_1  %>% select (-"Classification", )
Cluster_2 = Cluster_2  %>% select (-"Classification", )

#CLuster 1 split in 2
HC21 <- eclust(Cluster_1, "hclust", k = 2,metric = "euclidean",
               method = "complete", graph = FALSE) #Clustering
HCClusters21 <-as.matrix(HC21$cluster) #Extract sizes
HCClusters21 <- as.data.frame(HCClusters21)
HCSizes21 = data.frame(table(HCClusters21)) #notice sample size for cluster size

Cluster_1$Classification2 = HCClusters21[,1]

#CLuster 2 split in 2
HC22 <- eclust(Cluster_2, "hclust", k = 2,metric = "euclidean",
               method = "complete", graph = FALSE) #Clustering
HCClusters22 <-as.matrix(HC22$cluster) #Extract sizes
HCClusters22 <- as.data.frame(HCClusters22)
HCSizes22 = data.frame(table(HCClusters22)) #notice sample size for cluster size
Cluster_2$Classification2 = HCClusters22[,1]
#Rename the clusters in cluster 2 (clusters 3 and 4)
y = nrow(Cluster_2)
for (i in 1:y){
  if (Cluster_2[i,43] == 1)
  {Cluster_2[i,43] = "3"}
  else if (Cluster_2[i,43] == 2)
  {Cluster_2[i,43] = "4"}}
Step2 = rbind(Cluster_1, Cluster_2)
Step2$Mean_Poverty_BHC = as.numeric(as.character(Step2$Mean_Poverty_BHC))
LA_Z$Mean_Poverty_BHC = as.numeric(as.character(LA_Z$Mean_Poverty_BHC))
#Map of the 4 clusters
Step2 = Step2[,c(1,43)]
Solution<- data.frame(Step2) %>% right_join(LA_Z, by="Mean_Poverty_BHC", all = T)
Classification4 <-as.data.frame(cbind(as.character(LA_Inverse$LA_Code), Solution[,2]))
names(Classification4)<-c("LA_Code", "Classification") 
HClass4<-merge (x = Local_Authorities, y = Classification4, by.x = c("LA_Engla_3"), by.y = c("LA_Code"))

Map_Bub_4 = tm_shape(HClass4) + tm_bubbles(size = 0.2, col = "Classification", palette = "Set1", style = "quantile",legend.size.show = FALSE, title.col = "Cluster", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)+
  tm_shape(Regions)+tm_borders("black", lwd = .9,)+tm_text("rgn19nm", size = 0.6)
Map_Bub_4
