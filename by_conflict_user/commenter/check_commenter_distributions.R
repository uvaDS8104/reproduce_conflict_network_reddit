rm(list = ls())
setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network")
top_quantile = 0
data_type <- "new"

source("./script/shiny.R")
source("./script/sr_network/sub_shiny.R")
res_path <- "./script/sr_network/by_conflict_user/commenter/res"

if (!requireNamespace("jsonlite", quietly = TRUE)) { install.packages("jsonlite") }
library(jsonlite)

df_user_all<-NULL
sr_list <- unique(df$sr_id)#[c(which(unique(df$sr_id)==sr_id):length(unique(df$sr_id)))]
for(sr_id in sr_list){
  tryCatch({
    if(!file.exists(paste0("./data/",data_type,"/comments/",sr_id,".json"))){
      print(paste0("./data/",data_type,"/comments/",sr_id,".json", " --- file not found error! "))
      next
    }
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
    
    df_user <- df_comment_all %>% 
      group_by(comment_sr_id) %>% # get up/down voted comments
      mutate(u = ifelse(comment_score>0,1,0), d = ifelse(comment_score<0,1,0)) %>% 
      group_by(comment_sr_id, comment_author_id) %>% # get up/down voted commenter
      summarise(nu = sum(u,na.rm=TRUE), 
                nd = sum(d,na.rm=TRUE),
                n = nu+nd ) %>% as.data.frame()
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

saveRDS(df_user_all, paste0(res_path,"/df_user_full.rds"))



df_user_all <- readRDS(paste0(res_path,"/df_user_full.rds"))
colnames(df_user_all)[which(colnames(df_user_all)=="comment_sr_id")] <- "sr_id"
df_user_all <- merge(df_user_all,distinct(df[,c("sr_id", "sr_name", "group")]),all.x = T)
# check distribution of the number of commenters per forum in each topic 
df_user_all %>% group_by(group, sr_name) %>%
  summarise(n_commenters = n_distinct(comment_author_id)) %>%
  ggplot(aes(x=n_commenters, fill=group)) + 
  geom_histogram(position = "dodge") + 
  geom_vline(aes(xintercept = max(n_commenters[group=="ed"]))) +
  geom_text(aes(x = max(n_commenters[group=="ed"]), 
                label = paste("Max:", max(n_commenters[group=="ed"])),
                y = 40, vjust = -0.5))

# summarize an "average" number of comments a commenter posts to a forum 
df_n_user_comments <- df_user_all %>% 
  group_by(group, sr_name, comment_author_id) %>%
  summarise(ncomments_per_author_median = median(n,na.rm=T),
            ncomments_per_author_mean = median(n,na.rm=T),
            ncomments_per_author_q25 = quantile(n,0.25,na.rm=T),
            ncomments_per_author_q75 = quantile(n,0.75,na.rm=T) )

# ed
df_n_user_comments[which(df_n_user_comments$group=="ed"),] %>% ggplot(aes(x=ncomments_per_author_median, fill=group)) + 
  geom_histogram(position = "dodge") + 
  geom_vline(aes(xintercept = quantile(ncomments_per_author_median,0.75))) +
  geom_text(aes(x = quantile(ncomments_per_author_median,0.75), 
                label = paste("Q75:", quantile(ncomments_per_author_median,0.75)),
                y = 40, vjust = -0.5))+xlim(0,50)

# diet
df_n_user_comments[which(df_n_user_comments$group=="diet"),] %>% ggplot(aes(x=ncomments_per_author_median, fill=group)) + 
  geom_histogram(position = "dodge") + 
  geom_vline(aes(xintercept = quantile(ncomments_per_author_median,0.75))) +
  geom_text(aes(x = quantile(ncomments_per_author_median,0.75), 
                label = paste("Q75:", quantile(ncomments_per_author_median,0.75)),
                y = 40, vjust = -0.5))+xlim(0,50)
# fitness
df_n_user_comments[which(df_n_user_comments$group=="fitness"),] %>% ggplot(aes(x=ncomments_per_author_median, fill=group)) + 
  geom_histogram(position = "dodge") + 
  geom_vline(aes(xintercept = quantile(ncomments_per_author_median,0.75))) +
  geom_text(aes(x = quantile(ncomments_per_author_median,0.75), 
                label = paste("Q75:", quantile(ncomments_per_author_median,0.75)),
                y = 40, vjust = -0.5))+xlim(0,50)

# control
df_n_user_comments[which(df_n_user_comments$group=="control"),] %>%
  ggplot(aes(x=ncomments_per_author_median, fill=group)) + 
  geom_histogram(position = "dodge") + 
  geom_vline(aes(xintercept = quantile(ncomments_per_author_median,0.75))) +
  geom_text(aes(x = quantile(ncomments_per_author_median,0.75), 
                label = paste("Q75:", quantile(ncomments_per_author_median,0.75)),
                y = 40, vjust = -0.5))+xlim(0,50)

# mental
df_n_user_comments[which(df_n_user_comments$group=="mental"),] %>% ggplot(aes(x=ncomments_per_author_median, fill=group)) + 
  geom_histogram(position = "dodge") + 
  geom_vline(aes(xintercept = quantile(ncomments_per_author_median,0.75))) +
  geom_text(aes(x = quantile(ncomments_per_author_median,0.75), 
                label = paste("Q75:", quantile(ncomments_per_author_median,0.75)),
                y = 40, vjust = -0.5))+xlim(0,50)

