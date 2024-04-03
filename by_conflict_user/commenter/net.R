rm(list = ls())
setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network")
top_quantile = 0
data_type <- "new"
top_n_members <- 500 # downsample subreddits that has super large number of members

# ----- prepare environment and data -----
source("./script/shiny.R")
source("./script/sr_network/sub_shiny.R")
res_path <- "./script/sr_network/by_conflict_user/commenter/res"

if (!requireNamespace("jsonlite", quietly = TRUE)) { install.packages("jsonlite") }
library(jsonlite)

# ----- loop through each subreddit forum -----
# sr_id = "2qh4w"
df_user_all <- NULL
sr_list <- unique(df$sr_id)#[c(which(unique(df$sr_id)==sr_id):length(unique(df$sr_id)))]
for(sr_id in sr_list){
  tryCatch({
    # ---- load a sr in json and convert to dataframe ----
    if(!file.exists(paste0("./data/",data_type,"/comments/",sr_id,".json"))){
      print(paste0("./data/",data_type,"/comments/",sr_id,".json", " --- file not found error! "))
      next
    }
    sr_comments <- fromJSON(paste0("./data/",data_type,"/comments/",sr_id,".json"))
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
        filter(n_comments<=50, n_comments>=2) %>%
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
    show_hist_plots = T
    smp_size = 100 #1000
    source("./script/sr_network/by_conflict_user/rm_random.R")
    if(is.null(df_user_all)){
      df_user_all <- df_user
    }else{
      df_user_all <- bind_rows(df_user_all, df_user)
    }
  }, error=function(e){
    print(e)
    print(paste0("skip -- ",sr_id))
  })
}
saveRDS(df_user_all, paste0(res_path,"/df_user_p_rm_random.rds"))



# ---- filter df_user_all outside by fine tuning alphas ----
library(Matrix)
alpha_random = 0.25
alpha_troll = 0.25

df_user_all <- readRDS(paste0(res_path,"/df_user_p_rm_random.rds"))
df_user_all <- df_user_all[which(df_user_all$p_value<alpha_random),]
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
for(net_type in c("conflict", "co_conflict")){
  source("./script/sr_network/by_conflict_user/net_up-down.R")
  write_graph(g, paste0(res_path,"/commenter_",net_type,
                        "random_p_",alpha_random,
                        "troll_p_",alpha_troll,
                        ".gml"), format = "gml")
}


