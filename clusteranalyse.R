library(cluster)
library(dendextend)
library(tidyverse)
library(gridExtra)

#################################################################################
#                 Clusteranalyse mit Datensatz ohne NAs
#                             2 Cluster
################################################################################
# data transformation
data_clust <- data_NA %>%
  #mutate_if(is.numeric,function(x){as.numeric(scale(x))}) %>%
  filter(f0201new == "0") %>% #fahrradfahrer raus
  dplyr::select(-f0201new)

#Clusteranalyse
dist_gower <- daisy(data_clust, metric = "gower")

hc_obj <- hclust(dist_gower, method = "complete")

#Dendogramm
plot(hc_obj)
hc_dend <- as.dendrogram(hc_obj)
hc_dend_col <- color_branches(hc_dend, k = 2)
plot(hc_dend_col)

ind_clust_3 <- cutree(hc_obj, k = 2)

data_clust2 <- data_clust %>%
  mutate(cluster = ind_clust_3)

plot_list <- sapply(1:(dim(data_clust2)[2]-1), function(f){
  temp_data <- dplyr::select(data_clust2,c(f,dim(data_clust2)[2]))
  temp_col_names <- colnames(temp_data)
  colnames(temp_data) <- c("x","cluster")
  ggplot(temp_data)+
    geom_bar(aes(x = x,y = ..prop..,group = factor(cluster), fill = factor(cluster)),stat = "count",position = "dodge")+
    labs(title = temp_col_names[1])+
    theme_bw()
},simplify = F)

#Besondere Unrerschiede in diesen Plots
plot_list[[3]]
plot_list[[5]]
plot_list[[10]]
plot_list[[13]]
plot_list[[14]]
plot_list[[20]]
plot_list[[24]]
plot_list[[25]]
plot_list[[28]]
plot_list[[29]]
plot_list[[30]]
plot_list[[34]]
plot_list[[35]]
plot_list[[39]]


ggplot(data_clust2,aes(x = f20,fill = factor(cluster)))+
  geom_bar(position = "dodge")


#################################################################################
#                 Clusteranalyse mit Datensatz einschließlich NAs
#                             3 Cluster
################################################################################

# data transformation
data_clust1 <- data %>%
  #mutate_if(is.numeric,function(x){as.numeric(scale(x))}) %>%
  filter(f0201new == "0") %>% #fahrradfahrer raus
  dplyr::select(-f0201new)

dist_gower1 <- daisy(data_clust1, metric = "gower")

hc_obj1 <- hclust(dist_gower1, method = "complete")

plot(hc_obj1)
hc_dend1 <- as.dendrogram(hc_obj1)
hc_dend_col1 <- color_branches(hc_dend1, k = 3)
plot(hc_dend_col1)

ind_clust_31 <- cutree(hc_obj1, k = 3)

data_clust21 <- data_clust1 %>%
  mutate(cluster = ind_clust_31)

plot_list1 <- sapply(1:(dim(data_clust21)[2]-1), function(f){
  temp_data <- dplyr::select(data_clust21,c(f,dim(data_clust21)[2]))
  temp_col_names <- colnames(temp_data)
  colnames(temp_data) <- c("x","cluster")
  ggplot(temp_data)+
    geom_bar(aes(x = x,y = ..prop..,group = factor(cluster), fill = factor(cluster)),stat = "count",position = "dodge")+
    labs(title = temp_col_names[1])+
    theme_bw()
},simplify = F)

plot_list1[[1]]
plot_list1[[2]]
plot_list1[[3]]
plot_list1[[4]]
plot_list1[[6]]
plot_list1[[10]]
plot_list1[[13]]
plot_list1[[14]]
plot_list1[[18]]
plot_list1[[20]]
plot_list1[[28]]
plot_list1[[29]]
plot_list1[[30]]
plot_list1[[31]]
plot_list1[[32]]
plot_list1[[35]]
plot_list1[[39]]
plot_list1[[46]]



ggplot(data_clust21,aes(x = f20,fill = factor(cluster)))+
  geom_bar(position = "dodge")

#################################################################################
#                 Clusteranalyse nur mit Daten die laut AIC wichtig sind
################################################################################
# data transformation
data_clust2 <- data_aic %>%
  #mutate_if(is.numeric,function(x){as.numeric(scale(x))}) %>%
  filter(f0201new == "0") %>% #fahrradfahrer raus
  dplyr::select(-f0201new)

dist_gower2 <- daisy(data_clust2, metric = "gower")

hc_obj2 <- hclust(dist_gower2, method = "complete")

plot(hc_obj2)
hc_dend2 <- as.dendrogram(hc_obj2)
hc_dend_col2 <- color_branches(hc_dend2, k = 2)
plot(hc_dend_col2)

ind_clust_32 <- cutree(hc_obj2, k = 2)

data_clust22 <- data_clust2 %>%
  mutate(cluster = ind_clust_32)

plot_list2 <- sapply(1:(dim(data_clust22)[2]-1), function(f){
  temp_data <- dplyr::select(data_clust22,c(f,dim(data_clust22)[2]))
  temp_col_names <- colnames(temp_data)
  colnames(temp_data) <- c("x","cluster")
  ggplot(temp_data)+
    geom_bar(aes(x = x,y = ..prop..,group = factor(cluster), fill = factor(cluster)),stat = "count",position = "dodge")+
    labs(title = temp_col_names[1])+
    theme_bw()
},simplify = F)

plot_list2[[1]]
plot_list2[[2]]
plot_list2[[3]]
plot_list2[[7]]
plot_list2[[16]]
plot_list2[[17]]
plot_list2[[22]]
plot_list2[[23]]


#################################################################################
#                 Clusteranalyse mit Datensatz ohne NAs und gefilter mit aIC
#                             2 Cluster
################################################################################
# data transformation
data_clust4 <- data_NA %>%
  #mutate_if(is.numeric,function(x){as.numeric(scale(x))}) %>%
  filter(f0201new == "0") %>% #fahrradfahrer raus
  dplyr::select(f0101 ,f05 , f07 , f0803 , f0808 , f0811 , f0819 , 
                f0823 , f0903 , f1003 , f11 , f12 , f1603 , f0202new , f0203new , 
                f1602 , f20 , infra , image , zweck , umfeld , infra2 , anstr)

#Clusteranalyse
dist_gower4 <- daisy(data_clust4, metric = "gower")

hc_obj4 <- hclust(dist_gower4, method = "complete")

#Dendogramm
plot(hc_obj4)
hc_dend4 <- as.dendrogram(hc_obj4)
hc_dend_col4 <- color_branches(hc_dend4, k = 2)
plot(hc_dend_col4)

ind_clust_34 <- cutree(hc_obj4, k = 2)

data_clust24 <- data_clust4 %>%
  mutate(cluster = ind_clust_34)

plot_list4 <- sapply(1:(dim(data_clust24)[2]-1), function(f){
  temp_data <- dplyr::select(data_clust24,c(f,dim(data_clust24)[2]))
  temp_col_names <- colnames(temp_data)
  colnames(temp_data) <- c("x","cluster")
  ggplot(temp_data)+
    geom_bar(aes(x = x,y = ..prop..,group = factor(cluster), fill = factor(cluster)),stat = "count",position = "dodge")+
    labs(title = temp_col_names[1])+
    theme_bw()
},simplify = F)

#Besondere Unrerschiede in diesen Plots
plot_list4[[4]]
plot_list4[[9]]
plot_list4[[17]]
plot_list4[[18]]

