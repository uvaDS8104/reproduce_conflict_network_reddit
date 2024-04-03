rm(list = ls())
setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network")
top_quantile = 0
data_type <- "new"
alpha_random = 0.1
alpha_troll = 0.05

# ----- prepare environment and data -----
source("./script/shiny.R")
source("./script/sr_network/sub_shiny.R")
res_path <- "./script/sr_network/by_conflict_user/author/res"

for(dcp in c(0.1,0.2,0.25)){
  ucp <- 1-dcp
  
  # get the comments from ed, diet, fit from subreddits
  # enforce a threshold (named signigicant presence) of more than 10 comments in a subreddit to include a author
  df <- df %>% group_by(group, sr_name, author_id) %>% filter( n_distinct(sm_id) > 10)
  
  # We say a comment is downvoted (in aggregate) if the total number of downvotes for the comments exceeds upvotes, and upvoted when upvotes exceed downvotes
  # for each submission/post
  df <- df %>% group_by(sr_name) %>%
    mutate(ur_u = round(quantile(as.numeric(upvote_ratio), ucp, na.rm=TRUE),2),
           ur_d = round(quantile(as.numeric(upvote_ratio), dcp, na.rm=TRUE),2),
           u = ifelse(upvote_ratio>=ur_u,1,0),
           d = ifelse(upvote_ratio<=ur_d,1,0) )
  # mutate(u = ifelse(upvote_ratio>ucp,1,0), d = ifelse(upvote_ratio<dcp,1,0) )
  
  # we say that a user has shown social behavior if they have more upvoted comments (rewarded, norm-compliant) 
  # compared to down- voted ones (sanctioned, norm-violating) within a subreddit.
  df_agg <- df %>% group_by(group, sr_name, author_id) %>% 
    summarise(nu = sum(u,na.rm=TRUE), 
              nd = sum(d,na.rm=TRUE)) %>% as.data.frame()
  
  
  # eliminate sanctioning by chance
  show_hist_plots = T
  smp_size = 100
  # prepare intermediate dataframes to use 
  df_comments <- df[,c("sr_name", "author_id", "sm_id", "u", "d")]
  df_user <- df_agg[,c("sr_name", "author_id", "nu", "nd")]
  source("./script/sr_network/by_conflict_user/rm_random.R")
  # remove random author-forum pairs
  df_agg <- df_user[which(df_user$p_value<alpha_random),]
  colnames(df_agg) <- c("sr_name", "author_id", "nu", "nd", "n", "rd", "p")
  
  df_agg$u2d <- NA
  df_agg$u2d[which(df_agg$nu==df_agg$nd)] <- 0 # neutral author in a forum
  df_agg$u2d[which(df_agg$nu>df_agg$nd)] <- 1 # positive author in a forum
  df_agg$u2d[which(df_agg$nu<df_agg$nd)] <- -1 # negative author in a forum
  df_agg <- df_agg[which(!is.na(df_agg$u2d)),]
  
  forum_author_matrix <- reshape2::acast(df_agg[,c("sr_name","author_id", "u2d")], sr_name ~ author_id, value.var = "u2d", fill = NA) # very important no sentiment should be NA not 0
  saveRDS(forum_author_matrix, paste0("./script/sr_network/by_conflict_user/author/res/forum_author_matrix_",
                                      dcp,"_",
                                      ucp,".rds"))
  
  
  
  # ---- create network from here ----
  for(net_type in c("conflict", "co_conflict")){
    library(Matrix)
    forum_author_matrix_org <- readRDS(paste0(res_path,
                                              "/forum_author_matrix_",
                                              dcp,"_",
                                              ucp,".rds"))
    print(dim(forum_author_matrix_org))
    source("./script/sr_network/by_conflict_user/net_up-down.R")
    write_graph(g, paste0(res_path,
                          "/author_",
                          net_type,"_",
                          dcp,"_",
                          ucp,".gml"), format = "gml")
    
  }
}



