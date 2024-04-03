rm(list = ls())
setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/sr_network/analysis")
library(igraph)
library(dplyr)
library(knitr)
library(kableExtra)
library(assortnet)


g <- read_graph(paste0("./final_nets/co_conflict_voting.gml"), format = "gml")
# Run Louvain clustering
clusters <- cluster_louvain(g, weights = E(g)$weights, resolution = 1)
# Assign the cluster membership as a vertex attribute
V(g)$community <- membership(clusters)
write_graph(g, paste0("./final_nets/co_conflict_voting.graphml"), format = "graphml")


# check assortativity
adj_matrix <- round(as.matrix(as_adjacency_matrix(g, attr = "weight", sparse = FALSE)),4)
assort_weighted <- assortment.discrete(graph = adj_matrix, 
                                       types = as.factor(V(g)$group), 
                                       weighted = TRUE, 
                                       SE = T, 
                                       M = 100, 
                                       na.rm = T)

quantile(E(g)$weight,c(0.3,0.5,0.75,0.95))




# ------- case by case ----------
# copy code from "net_parallel.R"
rm(list = ls())
library(igraph)
library(foreach)
library(doParallel)
library(jsonlite)

numCores <- detectCores() - 1  # Leave one core free for system processes
registerDoParallel(cores=numCores)

setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network")
top_quantile = 0
data_type <- "new"
top_n_members <- 500 # downsample subreddits that has super large number of members

source("./script/shiny.R")
source("./script/sr_network/sub_shiny.R")
source("./script/sr_network/by_conflict_user/func_rm.R")
res_path <- "./script/sr_network/by_conflict_user/commenter/res"

library(Matrix)
alpha_random = 0.25
alpha_troll = 0.25

df_user_all <- readRDS(paste0(res_path,"/df_user_p_parallel.rds"))
df_user_all <- df_user_all[which(df_user_all$p_value<alpha_random),] %>%
  filter(is.na(error)) %>%
  select(f,a,nu,nd,n,rd,p_value)

colnames(df_user_all) <- c("comment_sr_id", "comment_author_id", "nu", "nd", "n", "rd", "p")
df_user_all$u2d <- NA
df_user_all$u2d[which(df_user_all$nu==df_user_all$nd)] <- 0
df_user_all$u2d[which(df_user_all$nu>df_user_all$nd)] <- 1
df_user_all$u2d[which(df_user_all$nu<df_user_all$nd)] <- -1
df_user_all <- df_user_all[which(!is.na(df_user_all$u2d)),]
df_user_all$sr_id <- df_user_all$comment_sr_id
df_user_all <- merge(df_user_all, distinct(df[,c("sr_id","sr_name","group")]),all.x = TRUE)
forum_author_matrix_org <- reshape2::acast(df_user_all[,c("sr_name","comment_author_id", "u2d")],
                                           sr_name ~ comment_author_id, value.var = "u2d", fill = NA) # very important no sentiment should be NA not 0
print(dim(forum_author_matrix_org))
net_type <- "co_conflict"
jaccard <- T
source("./script/sr_network/by_conflict_user/net_up-down.R")

# load final graph in 
setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/sr_network/analysis")
library(igraph)
library(dplyr)
library(knitr)
library(kableExtra)
library(assortnet)
g <- read_graph(paste0("./final_nets/co_conflict_voting.gml"), format = "gml")

pair1 <- as_edgelist(g)[as_edgelist(g)[,1] %in% V(g)[V(g)$group=="ed"]$name,]
pair2 <- as_edgelist(g)[as_edgelist(g)[,2] %in% V(g)[V(g)$group=="ed"]$name,]
edge_list <- unique( rbind(pair1, pair2) )

# check case by case
conflict_author_to_sr <- list()
conflict_author_from_sr <- list()

for(i in 1:nrow(edge_list)){
  conflict_authors <- colnames(forum_author_matrix)[colSums(!is.na(forum_author_matrix[c(edge_list[i,]),]))>=2]
  
  #--- from sr---
  sr_id <- unique(df$sr_id[which(df$sr_name==edge_list[i,1])])
  sr_comments <- fromJSON(paste0("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/data/",data_type,"/comments/",sr_id,".json"))
  # engineer it into a dataframe
  df_comment_all <- NULL
  for(sm_id in names(sr_comments)){
    df_comment <- sr_comments[[sm_id]]
    df_comment$comment_sr_id <- sr_id
    if(is.null(df_comment_all)){
      df_comment_all <- df_comment
    }
    else{
      df_comment_all <- dplyr::bind_rows(df_comment_all, df_comment)
    }
  }
  conflict_author_from_sr[[i]] <- df_comment_all %>% 
    filter(comment_author_id%in%c(conflict_authors)) %>% 
    select(comment_author_id, comment_score, comment_body, comment_upvotes, comment_downvotes, comment_created_utc, comment_id)
  conflict_author_from_sr[[i]]$forum <- edge_list[i,1]
  
  
  #--- to sr---
  sr_id <- unique(df$sr_id[which(df$sr_name==edge_list[i,2])])
  sr_comments <- fromJSON(paste0("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/data/",data_type,"/comments/",sr_id,".json"))
  # engineer it into a dataframe
  df_comment_all <- NULL
  for(sm_id in names(sr_comments)){
    df_comment <- sr_comments[[sm_id]]
    df_comment$comment_sr_id <- sr_id
    if(is.null(df_comment_all)){
      df_comment_all <- df_comment
    }
    else{
      df_comment_all <- dplyr::bind_rows(df_comment_all, df_comment)
    }
  }
  conflict_author_to_sr[[i]] <- df_comment_all %>%
    filter(comment_author_id%in%c(conflict_authors)) %>% 
    select(comment_author_id, comment_score, comment_body, comment_upvotes, comment_downvotes, comment_created_utc, comment_id)
  conflict_author_to_sr[[i]]$forum <- edge_list[i,2]
  
  print(paste0((edge_list[i,1]), " & ", edge_list[i,2],": ",conflict_authors))
  
}









