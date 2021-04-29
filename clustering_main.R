############## Outfield players ##############
########### No positions ###############
###### K means | Clara #######


library(tidyverse)
master <- read_csv("fifa_final_28mar.csv")

df<-master

df<-df[,-c(24:49)]
df<-df[,c(2,4,5,12,13,22,24:57)]

library(NbClust)
library(cluster)
library(factoextra)

dfs<-scale(df)

colnames(df)
fviz_nbclust(dfs, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
#k=7

# Silhouette method
fviz_nbclust(dfs, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

km <- kmeans(dfs,7,iter.max = 15,nstart = 20)

fviz_cluster(km,dfs)

# Plot faithful data set
fviz_pca_ind(prcomp(df), title = "",
             axes = c(1,2),
             habillage = km$cluster, palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

table(km$cluster, master$PositionGroup)
table(kmed$clustering, master$PositionGroup)

# table(Pos7, km$cluster)
# require(plyr)
# Pos7 <- mapvalues(master$Position,
#                          from = c("RF","ST","LW","GK","RCM","LF","RS","RCB","LCM","CB","LDM","CAM","CDM","LS","LCB","RM","LAM","LM","LB","RDM","RW","CM","RB","RAM","CF","RWB","LWB"),
#                          to = c("F","ST","ST","GK","M","F","ST","B","M","B","DM","AM","DM","ST","B","M","AM","M","B","DM","ST","M","B","AM","F","B","B"))


aggregate(master,by=list(kmed$clustering),mean) %>% 
  select(Age,Overall, Potential, Value, Wage)

unique(master$Position)


km$betweenss

#####clara
kmed <- clara(dfs, 7, metric = "manhattan", stand = FALSE,
      samples = 100, pamLike = T)

fviz_nbclust(dfs, clara, method = "silhouette")

fviz_cluster(kmed,
#             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)

kmed$clustering


data <- data.frame(
  size=km$size,  
  cluster=c(1,2,3,4,5,6,7)
)
  
ggplot(data, aes(x=cluster, y=size, fill=cluster) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")
