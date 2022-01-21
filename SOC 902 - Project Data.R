library(cfbscrapR)
library(tidyverse)
library(devtools)
library(reshape)
library(igraph)
library(cluster)
library(gt)
#library(sna) #Will need this to complete the blockchain analysis
#install.packages("devtools")
#devtools::install_github("meysubb/cfbscrapR")
#remotes::install_github("rstudio/gt")

data <- cfb_pbp_data(2019, week = 1)

defense_data <- 
  cfb_drives(2019) %>%
  select('offense', 'offense_conference', 'defense', 'defense_conference', 'plays', 'yards', 'drive_result',
         'time_minutes_start', 'time_minutes_end', 'time_minutes_elapsed') %>%
  filter(drive_result == 'FUMBLE' | drive_result == 'INT' | drive_result == 'INT TD' | drive_result == 'FUMBLE TD' | drive_result == 'FUMBLE RETURN TD')

all_def_data = c()

for (j in 2015:2019){
  for (i in 1:15){ ##change the weeks here and only do regular season
    def_data <- 
  cfb_pbp_data(j, week = i, season_type = "both") %>%
  select("game_id","offense_play","offense_conference","defense_play","defense_conference","home","away",
         "yard_line", "yards_to_goal","down","distance","scoring","yards_gained","play_type") %>%
  filter(play_type == "Sack" | play_type == "Fumble Recovery (Opponent)" |  play_type == "Pass Interception Return" |  play_type == "Fumble Return Touchdown" | play_type == "Interception Return Touchdown")
  
  all_def_data <- rbind(all_def_data, def_data)
  }
}

#Create the social network dataset
all_def_data <- 
  all_def_data %>%
  select(offense_play,offense_conference,defense_play,defense_conference,home,away,play_type)  %>%
  dplyr::count(play_type,offense_play,offense_conference,defense_play,defense_conference,home,away) %>% 
  tidyr::spread(key = play_type,value = n) %>% 
  replace(is.na(.), 0)
#   filter(defense_conference %in% c('ACC', 'SEC', 'Big Ten', 'Pac 12', 'Big 12'))
all_def_data$offense_conference <- replace(all_def_data$offense_conference, all_def_data$offense_conference == "0", "Other")
all_def_data$defense_conference <- replace(all_def_data$defense_conference, all_def_data$defense_conference == "0", "Other")

all_def_data2 <-  
  all_def_data %>% 
  select_all() %>%
  mutate(
    weights = 1*Sack + 0.5*`Fumble Recovery (Opponent)` + 6*`Fumble Return Touchdown` + 6*`Interception Return Touchdown` + 1*`Pass Interception Return`)


network_data <- 
  all_def_data %>% 
  select(offense_play,offense_conference,defense_play,defense_conference,home,away, play_type) %>%
  dplyr::count(play_type,offense_play,offense_conference,defense_play,defense_conference,home,away) %>% 
  tidyr::spread(key = play_type,value = n) %>% 
  replace(is.na(.), 0) %>%
  filter(defense_conference %in% c('ACC', 'SEC', 'Big Ten', 'Pac-12', 'Big 12')) #& offense_conference %in% c('ACC', 'SEC', 'Big Ten', 'Pac-12', 'Big 12')) 

network_data2 <-  
  network_data %>% 
  select_all() %>%
  mutate(
    weights = 1*Sack + 0.5*`Fumble Recovery (Opponent)` + 6*`Fumble Return Touchdown` + 6*`Interception Return Touchdown` + 1*`Pass Interception Return`)

save(all_def_data,all_def_data2,network_data,network_data2, file="/Users/denisebradford/Documents/SOC 902 - Network Analysis/Data Project.RData")

acc_network_data <-  
  network_data %>% 
  filter(defense_conference %in% c('ACC') & offense_conference %in% c('ACC', 'Big Ten', 'Pac-12', 'Big 12')) %>%
  select_all() %>%
  mutate(
    weights = 1*Sack + 0.5*`Fumble Recovery (Opponent)` + 6*`Fumble Return Touchdown` + 6*`Interception Return Touchdown` + 1*`Pass Interception Return`)

sec_network_data <-  
  network_data %>% 
  filter(defense_conference %in% c('SEC') & offense_conference %in% c('ACC', 'Big Ten', 'Pac-12', 'Big 12')) %>%
  select_all() %>%
  mutate(
    weights = 1*Sack + 0.5*`Fumble Recovery (Opponent)` + 6*`Fumble Return Touchdown` + 6*`Interception Return Touchdown` + 1*`Pass Interception Return`)

big10_network_data <-  
  network_data %>% 
  filter(defense_conference %in% c('Big Ten') & offense_conference %in% c('ACC', 'Big Ten', 'Pac-12', 'Big 12')) %>%
  select_all() %>%
  mutate(
    weights = 1*Sack + 0.5*`Fumble Recovery (Opponent)` + 6*`Fumble Return Touchdown` + 6*`Interception Return Touchdown` + 1*`Pass Interception Return`)

big12_network_data <-  
  network_data %>% 
  filter(defense_conference %in% c('Big 12') & offense_conference %in% c('ACC', 'Big Ten', 'Pac-12', 'Big 12')) %>%
  select_all() %>%
  mutate(
    weights = 1*Sack + 0.5*`Fumble Recovery (Opponent)` + 6*`Fumble Return Touchdown` + 6*`Interception Return Touchdown` + 1*`Pass Interception Return`)

pac12_network_data <-  
  network_data %>% 
  filter(defense_conference %in% c('Pac-12') & offense_conference %in% c('ACC', 'Big Ten', 'Pac-12', 'Big 12')) %>%
  select_all() %>%
  mutate(
    weights = 1*Sack + 0.5*`Fumble Recovery (Opponent)` + 6*`Fumble Return Touchdown` + 6*`Interception Return Touchdown` + 1*`Pass Interception Return`)

save(acc_network_data, sec_network_data, big10_network_data, big12_network_data, pac12_network_data,
     file="/Users/denisebradford/Documents/SOC 902 - Network Analysis/Conference Data Project.RData")

#Try the bimode analysis


#Making dataset into a edgelist (Only using the offense vs defense)
#egdelist=graph_from_data_frame(acc_network_data[c(2,4,7:12)], directed=TRUE) 

#plot(egdelist, edge.arrow.size=.5)

#edge_layout <- layout_nicely(egdelist)
#plot(egdelist, layout=edge_layout, main = "ACC Network", edge.arrow.size=.5, edge.color="slateblue", edge.width=E(egdelist)$weights)

degree(egdelist)

de=igraph::degree(egdelist)
#Kinda pretty plot but not very useful :-)
#plot(egdelist, vertex.label="", vertex.color="gold", edge.color="slateblue", vertex.size=de/100, edge.width=E(egdelist)$weights/5)
attributes.name<-(unique(network_data2$defense_conference))

defense_full <- graph.data.frame(network_data2[c(2,4,7:12)], directed = TRUE, vertices=attributes.name) 
summary(defense_full)

edgelist_sack=network_data2[network_data2$Sack>0, c("defense_conference", "offense_conference", "Sack")]
def_sack=graph_from_data_frame(d=edgelist_sack, directed=F) 
def_sack=simplify(def_sack, edge.attr.comb="mean") 

sack_mat=as_adjacency_matrix(def_sack, attr="Sack", sparse=F) 
sack_mat_in=t(sack_mat)

sack_mat_std=(sack_mat-mean(sack_mat))/sd(sack_mat) 
sack_mat_in_std=t(sack_mat_std)

defense_sack <- delete.edges(defense_full, E(defense_full)[get.edge.attribute(defense_full,name = "Sack")==0])
defense_fumble <- delete.edges(defense_full, E(defense_full)[get.edge.attribute(defense_full,name = "Fumble Recovery (Opponent)")==0])
defense_fumble_TD <- delete.edges(defense_full, E(defense_full)[get.edge.attribute(defense_full,name = "Fumble Return Touchdown")==0])
defense_int_TD <- delete.edges(defense_full, E(defense_full)[get.edge.attribute(defense_full,name = "Interception Return Touchdown")==0])
defense_int <- delete.edges(defense_full, E(defense_full)[get.edge.attribute(defense_full,name = "Pass Interception Return")==0])


sack_layout <- layout_on_sphere(defense_sack)
plot(defense_sack, layout=sack_layout, main = "Sack", edge.arrow.size=.5)

defense_sack_und <- as.undirected(defense_sack, mode='collapse')
defense_sack_no_iso <- delete.vertices(defense_sack_und, V(defense_sack_und)[degree(defense_sack_und)==0])

sack_comm_wt <- walktrap.community(defense_sack_no_iso) #, steps=200,modularity=TRUE

sack_comm_dend <- as.dendrogram(sack_comm_wt, use.modularity=TRUE)
plot(sack_comm_dend)

sack_comm_eb <- edge.betweenness.community(defense_sack_no_iso)
plot(as.dendrogram(sack_comm_eb))


