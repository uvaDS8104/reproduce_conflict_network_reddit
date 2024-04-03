#' 
#' @param df_comments one comment per row
#' @param df_user one member(participant) (pair of user and forum) per row 


colnames(df_comments) <- c("f", "a", "sm", "u", "d")
stopifnot(nrow(df_comments)==nrow(distinct(df_comments[,c("f", "a", "sm")])) )
colnames(df_user) <- c("f", "a", "nu", "nd")
stopifnot(nrow(df_user)==nrow(distinct(df_user[,c("f","a")])) )
df_user$n <- df_user$nd + df_user$nu
df_user$rd <- df_user$nd/df_user$n

df_user$p_value <- NA
for(a in unique(df_user$a)){
  for(f in unique(df_user$f[which(df_user$a==a)])){
    tryCatch({
      null_rd <- df_user$rd[which(df_user$a==a&df_user$f==f)]
      rd_smp_ls <- c()
      for(i in c(1:smp_size)){
        sm_smp <- sample(df_comments$sm[which(df_comments$f==f)], size=df_user$n[which(df_user$f==f&df_user$a==a)], replace=T)
        rd_smp <- sum(df_comments$d[which(df_comments$sm%in%sm_smp)])/length(df_comments$d[which(df_comments$sm%in%sm_smp)])
        rd_smp_ls <- c(rd_smp_ls, rd_smp)
      }
      # center the test samples
      null_rd <- null_rd - mean(rd_smp_ls)
      rd_smp_ls <- rd_smp_ls - mean(rd_smp_ls)
      # two-tail p_value, if p <0.05, author didn't get downvoted by chance
      p_value <- sum(rd_smp_ls>abs(null_rd) | rd_smp_ls<(-abs(null_rd)) )/length(rd_smp_ls)
      if(show_hist_plots){
        hist(rd_smp_ls,breaks=100, main=paste0(a," in ",f), xlab=paste0("p=",p_value))
        abline(v=quantile(rd_smp_ls,c(0.025,0.975)), col=c("red","blue"), lwd=3, lty=2)
        abline(v=null_rd, col="black", lwd=3, lty=2 )
      }
      df_user$p_value[which(df_user$a==a&df_user$f==f)] <- p_value
    },error=function(e){
      print(e)
      print(paste0("error -- ",a,"in",f))
    })
  }
}
rm(df_comments)
