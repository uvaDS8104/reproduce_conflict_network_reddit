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
min_n_comments <- 10#2
source("./script/shiny.R")
source("./script/sr_network/sub_shiny.R")
source("./script/sr_network/by_conflict_user/func_rm.R")
res_path <- "./script/sr_network/by_conflict_user/commenter/res"


sr_list <- unique(df$sr_id)
df_user_results <- foreach(sr_id = sr_list, .packages = c("jsonlite", "dplyr")) %dopar% {
  
  tryCatch({
    file_path <- paste0("./data/",data_type,"/comments/",sr_id,".json")
    if(!file.exists(file_path)){
      message(file_path, " --- file not found error! ")
      return(NULL)
    }
    
    sr_comments <- fromJSON(file_path)
    df_comment_all <- do.call(dplyr::bind_rows, lapply(sr_comments, function(df_comment) {
      df_comment$comment_sr_id <- sr_id
      return(df_comment)
    }))
    
    # Data processing here
    # ---- filters on comments ----
    df_comment_all <- df_comment_all %>% filter() %>%
      filter(!is.na(comment_author_id)) %>%
      filter(!comment_author_id %in% c("None", "AutoModerator") ) #%>% # these are reddit moderator and banned/removed account
    # group_by(comment_sr_id, comment_author_id) %>% 
    # filter(n_distinct(comment_id)>10) # remove this inclusion criteria because it's too parsimonious for our dataset a median number of comment from a commenter to a forum is around 2-3 per forum, min of 10 will exclude too much participants
    if(n_distinct(df_comment_all$comment_author_id)>top_n_members){
      # rank super members
      in_member_ids <- df_comment_all %>% group_by(comment_author_id) %>%
        summarise(n_comments = n_distinct(comment_id)) %>%
        filter(n_comments<=50, n_comments>=min_n_comments) %>%
        arrange(desc(n_comments)) %>%
        slice_max(order_by = n_comments, n = top_n_members) # another way is to mimicing ED forum participating behaviors of commenters (in between 0-50)
      in_member_ids <- c(unique(in_member_ids$comment_author_id)) # there could be ties
    }else{
      in_member_ids <- unique(df_comment_all$comment_author_id)
    }
    # ---- label up/down comments & users ----
    df_comments <- df_comment_all %>% 
      filter(comment_author_id %in% in_member_ids) %>%
      mutate(u = ifelse(comment_score>0,1,0), d = ifelse(comment_score<0,1,0)) %>%
      select(comment_sr_id, comment_author_id, comment_id, u, d) # u/d comment
    df_user <- df_comments %>% 
      group_by(comment_sr_id, comment_author_id) %>%
      summarise(nu = sum(u,na.rm=TRUE), nd = sum(d,na.rm=TRUE)) %>% # u/d user
      select(comment_sr_id, comment_author_id, nu, nd) %>%
      as.data.frame()
    # ---- eliminate sanctioning by chance ----
    df_user <- rm_random(df_comments, 
                         df_user, 
                         smp_size=100,  #1000
                         show_hist_plots=F)
    # source("./script/sr_network/by_conflict_user/rm_random.R")
    # Return the final aggregated data
    return(df_user) 
  }, error = function(e) {
    message("Error processing ", sr_id, ": ", e$message)
    return(list(error = e$message, sr_id = sr_id))  # Return a list containing the error and sr_id
  })
}


# Combine all results
df_user_all <- do.call(bind_rows, df_user_results)
saveRDS(df_user_all, paste0(res_path,"/df_user_p_parallel_nc",min_n_comments,".rds"))

# ---- filter df_user_all outside by fine tuning alphas ----
library(Matrix)
alpha_random = 0.25
alpha_troll = 0.25
min_n_comments = 2

df_user_all <- readRDS(paste0(res_path,"/df_user_p_parallel_nc",min_n_comments,".rds"))
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

# ----- create up-down forum network from here -----
net_type <- "co_conflict"
jaccard <- T
source("./script/sr_network/by_conflict_user/net_up-down.R")
write_graph(g, paste0(res_path,"/commenter_",
                      "random_p_",alpha_random,
                      "troll_p_",alpha_troll,
                      net_type,"_jaccard_parallel.gml"), format = "gml")
jaccard <- F
source("./script/sr_network/by_conflict_user/net_up-down.R")
write_graph(g, paste0(res_path,"/commenter_",
                      "random_p_",alpha_random,
                      "troll_p_",alpha_troll,
                      net_type,"_parallel.gml"), format = "gml")

net_type <- "conflict"
source("./script/sr_network/by_conflict_user/net_up-down.R")
write_graph(g, paste0(res_path,"/commenter_",
                      "random_p_",alpha_random,
                      "troll_p_",alpha_troll,
                      net_type,"_parallel.gml"), format = "gml")


# # ---- get subgraphs ----
# for(net_type in c("conflict", "co_conflict")){
#   
#   jaccard <- T
#   source("./script/sr_network/by_conflict_user/net_up-down.R")
#   write_graph(g, paste0(res_path,"/commenter_",
#                         "random_p_",alpha_random,
#                         "troll_p_",alpha_troll,
#                         net_type,"_parallel.gml"), format = "gml")
#   
#   
#   # filter subgraph by ed/diet/fitness
#   # Step 1: Get the vertices where group == 1
#   sub_v <- V(g)[V(g)$group  %in% c("diet","ed","fitness")]
#   
#   # Step 2: Get neighbors of these vertices
#   neighbor_ids <- unique(unlist(lapply(sub_v, function(v) {
#     adjacent_vertices <- neighbors(g, v)
#     return(as_ids(adjacent_vertices))  # Convert neighbors to vertex ids
#   })))
#   # Convert the neighbor ids back to a vertex sequence
#   sub_v_neighbors <- V(g)[neighbor_ids]
#   
#   # Step 3: Combine unique vertices from group 1 and their neighbors
#   combined_vertices <- unique(c(sub_v, sub_v_neighbors))
#   
#   # Step 4: Create the subgraph
#   sub_g <- induced_subgraph(g, combined_vertices)
#   # sub_g <- induced_subgraph(g, which(V(g)$group %in% c("diet","ed","fitness") ))
#   write_graph(sub_g, paste0(res_path,"/subed_commenter_",
#                         "random_p_",alpha_random,
#                         "troll_p_",alpha_troll,
#                         net_type,"_parallel.gml"), format = "gml")
#   
# }
# 
# 
# 
# 
# 
