library(cluster)
library(dendextend)
library(tidyverse)
library(gridExtra)

#einmal mit data_NA
# data transformation
data_clust <- data_NA %>%
  #mutate_if(is.numeric,function(x){as.numeric(scale(x))}) %>%
  filter(f0201new == "0") %>% #fahrradfahrer raus
  dplyr::select(-f0201new)

dist_gower <- daisy(data_clust, metric = "gower")

hc_obj <- hclust(dist_gower, method = "complete")

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

##################################################################
#Ein mal mit data normal 
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
plot_list1[[32]]
plot_list1[[35]]
plot_list1[[39]]
plot_list1[[46]]



ggplot(data_clust21,aes(x = f20,fill = factor(cluster)))+
  geom_bar(position = "dodge")
