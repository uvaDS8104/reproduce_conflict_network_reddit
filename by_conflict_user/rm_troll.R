
#'To identify `troll's from potentially misbehaved users, 
#'we test if an observed user $j$ who was sanctioned by $P_j$ forums out of $N_j$ ($N_j\geq5$) 
#'forums they participated in. We sampled (with replacement) $N_{j}$ users from these forums, 
#'one per each, and calculated the ratio of users being sanctioned. 
#'We repeated the experiment 1000 time to get the sampling distribution of 
#'the ratio to calculate a one-tail $p$-value for the observed user.
#' One-tail test was used because a troll is more likely to misbehave in a 
#' larger number of forums they participate in by definition.
#' 
#' @param forum_author_matrix_org original matrix for all pairwise (forum, common_author)=score
#' @param forum_author_matrix filtered matrix for pairwise (forum, conflict_author)=score


p_ls <- list()
for( a in colnames(forum_author_matrix) ){
  parti_f_ls <- names(forum_author_matrix[,a][which(!is.na(forum_author_matrix[,a])) ]) # name of forums this user participated in
  if(length(parti_f_ls)>=5){
    confl_f_ls <- names(forum_author_matrix[,a][which(forum_author_matrix[,a]<0 ) ])
    null_ratio <- length(confl_f_ls)/length(parti_f_ls)
    
    sample_members <- function(row){
      row <- row[!is.na(row)]
      # Sample 'num_samples' times with replacement
      sample_indices <- sample(seq_along(row), size = 1000, replace = TRUE)
      # Subset the row vector with these indices
      sampled_values <- row[sample_indices]
      return(sampled_values)
    }
    forum_author_matrix_smp <- t(apply(forum_author_matrix_org[parti_f_ls,], MARGIN = 1, FUN=sample_members))
    sampled_ratio <- colSums(forum_author_matrix_smp<0)/nrow(forum_author_matrix_smp)
    hist(sampled_ratio, breaks = 100)
    abline(v = null_ratio, col="red", lwd=3, lty=2)
    
    # use empirical percentages to get p_values, 
    # (no) one-tail test because aggressors may intentionally attack more forums by definition
    null_ratio <- null_ratio - mean(sampled_ratio)
    sampled_ratio <- sampled_ratio-mean(sampled_ratio)
    # two-tail test
    p_value <- sum( sampled_ratio>abs(null_ratio)|sampled_ratio<(-abs(null_ratio)) )/length(sampled_ratio)
    p_ls[[a]] <- p_value
    # if p < 0.05, this user is an attacker/ troll who misbehaves
  }else{
    p_ls[[a]] <- 1
  }
}
forum_author_matrix_misbehave <- forum_author_matrix[,names(p_ls[p_ls<alpha_troll])]
forum_author_matrix <- as.matrix(forum_author_matrix[,names(p_ls[p_ls>=alpha_troll])])
colnames(forum_author_matrix) <- names(p_ls[p_ls>=alpha_troll])
