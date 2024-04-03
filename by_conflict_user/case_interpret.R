rm(list = ls())
setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network")
top_quantile = 0
data_type <- "new"
library(jsonlite)
source("./script/shiny.R")
source("./script/sr_network/sub_shiny.R")
res_path <- "./script/sr_network/by_conflict_user/commenter/res"
net_type <- "co_conflict"
alpha_troll = 0.25
alpha_random = 0.25
library(Matrix)

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
source("./script/sr_network/by_conflict_user/net_up-down.R")

g <- read_graph(paste0(res_path,"/commenter_",
                       "random_p_",alpha_random,
                       "troll_p_",alpha_troll,
                       net_type,"_parallel.gml"), format = "gml")

sub_v <- V(g)[V(g)$group  %in% c("diet","ed","fitness")]
neighbor_ids <- unique(unlist(lapply(sub_v, function(v) {
  adjacent_vertices <- neighbors(g, v)
  return(as_ids(adjacent_vertices))  # Convert neighbors to vertex ids
})))
sub_v_neighbors <- V(g)[neighbor_ids]
combined_vertices <- unique(c(sub_v, sub_v_neighbors))
g <- induced_subgraph(g, combined_vertices)
edge_list <- as_edgelist(g)


# check case by case
conflict_author_to_sr <- list()
conflict_author_from_sr <- list()

for(i in 1:nrow(edge_list)){
  conflict_authors <- colnames(forum_author_matrix)[colSums(!is.na(forum_author_matrix[c(edge_list[i,]),]))>=2]
  
  #--- from sr---
  sr_id <- unique(df$sr_id[which(df$sr_name==edge_list[i,1])])
  sr_comments <- fromJSON(paste0("./data/",data_type,"/comments/",sr_id,".json"))
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
  
  #--- to sr---
  sr_id <- unique(df$sr_id[which(df$sr_name==edge_list[i,2])])
  sr_comments <- fromJSON(paste0("./data/",data_type,"/comments/",sr_id,".json"))
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
  
  
  print(paste0((edge_list[i,1]), " & ", edge_list[i,2],": ",conflict_authors))
  
}


