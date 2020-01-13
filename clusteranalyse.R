library(cluster)
library(dendextend)
library(tidyverse)
library(gridExtra)

# data transformation
data_clust <- data %>%
  mutate_if(is.numeric,function(x){as.numeric(scale(x))}) %>%
  filter(f0201new == "0") %>% #fahrradfahrer raus
  select(-f0201new)

dist_gower <- daisy(data_clust, metric = "gower")

hc_obj <- hclust(dist_gower, method = "complete")

plot(hc_obj)
hc_dend <- as.dendrogram(hc_obj)
hc_dend_col <- color_branches(hc_dend, k = 3)
plot(hc_dend_col)

ind_clust_3 <- cutree(hc_obj, k = 3)

data_clust2 <- data_clust %>%
  mutate(cluster = ind_clust_3)

plot_list <- sapply(1:(dim(data_clust2)[2]-1), function(f){
  temp_data <- select(data_clust2,c(f,dim(data_clust2)[2]))
  temp_col_names <- colnames(temp_data)
  colnames(temp_data) <- c("x","cluster")
  ggplot(temp_data)+
    geom_bar(aes(x = x,y = ..prop..,group = factor(cluster), fill = factor(cluster)),stat = "count",position = "dodge")+
    labs(title = temp_col_names[1])+
    theme_bw()
},simplify = F)

plot_list[[59]]

ggplot(data_clust2,aes(x = f20,fill = factor(cluster)))+
  geom_bar(position = "dodge")


