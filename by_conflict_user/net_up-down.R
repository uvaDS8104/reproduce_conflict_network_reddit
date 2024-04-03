
# ---- Prepare the forum(row) to author(column) matrix ---- 
# Filter authors who have at least 1 positive value and 1 negative value to two forums
author_pos_counts <- apply(forum_author_matrix_org, 2, function(col) sum(col>0, na.rm=TRUE))
author_neg_counts <- apply(forum_author_matrix_org, 2, function(col) sum(col<0, na.rm=TRUE))
forum_author_matrix <- as.matrix(forum_author_matrix_org[, author_pos_counts >= 1 & author_neg_counts >= 1])
colnames(forum_author_matrix) <- colnames(forum_author_matrix_org)[author_pos_counts >= 1 & author_neg_counts >= 1]
print(dim(forum_author_matrix))
# positive value means "positive sentiment" / "being rewarded / upvoted" in a forum
# nagetive value means "negative sentiment" / "being sanctioned / downvoted" in a forum


# ---- identify and remove misbehaved users ----
source("./script/sr_network/by_conflict_user/rm_troll.R")
print(dim(forum_author_matrix))

# ---- Calculate forum to forum "differential-drop" per author (faster) ----
library(parallel)
library(tictoc)
tic()
process_author <- function(a) {
  # # --- uncomment to test ---
  # # fake example, this author is deemed positive by forum 1 and forum 4, negative by forum 3, and neutral by forum 5.
  # forum_vector <- c("forum1"=1, "forum2"=NA, "forum3"=-1,"forum4"=1, "forum5" = 0) 
  # # step 1. create individual adjacency matrix
  # repeated_matrix <- matrix(rep(forum_vector, each = length(forum_vector)), nrow = length(forum_vector), byrow = TRUE)
  # adj_matrix_individual <- repeated_matrix - t(repeated_matrix) # only non-na will return values
  # adj_matrix_individual[is.na(adj_matrix_individual)] <- 0 # replace NA with 0 for final sum-up (means no up/down voting change)
  # adj_matrix_individual[adj_matrix_individual<0] <- 0
  # rownames(adj_matrix_individual) <- names(forum_vector)
  # colnames(adj_matrix_individual) <- names(forum_vector)
  # # print(adj_matrix_individual)
  # # in adj_matrix_individual, from row to column, how much does the measure of connection drop,
  # # i.e. row 1 col 3 means from forum 1 to forum 3, the score (sentiment / upvotes) drop by 2.
  # # step 2 (optional). if conflict should only be from positive to negative, zero-out neutral forum related cells
  # neutrals <- names(forum_vector)[forum_vector==0&!is.na(forum_vector)]
  # adj_matrix_individual[,neutrals] <- 0
  # adj_matrix_individual[neutrals,] <- 0
  
  forum_vector <- forum_author_matrix[, a]
  # step 1. create individual adjacency matrix
  repeated_matrix <- matrix(rep(forum_vector, each = length(forum_vector)), nrow = length(forum_vector), byrow = TRUE)
  adj_matrix_individual <- repeated_matrix - t(repeated_matrix) # only non-na will return values
  adj_matrix_individual[is.na(adj_matrix_individual)] <- 0 # replace NA with 0 for final sum-up (means no up/down voting change)
  adj_matrix_individual[adj_matrix_individual<0] <- 0
  rownames(adj_matrix_individual) <- names(forum_vector)
  colnames(adj_matrix_individual) <- names(forum_vector)
  # step 2 (optional). if conflict should only be from positive to negative, zero-out neutral forum related cells
  neutrals <- names(forum_vector)[forum_vector==0&!is.na(forum_vector)]
  if(length(neutrals)>0){
    adj_matrix_individual[,neutrals] <- 0
    adj_matrix_individual[neutrals,] <- 0
  }
  return(adj_matrix_individual)
}
# Detect the number of available cores
no_cores <- detectCores() - 1
# Apply the function over each column (author) in parallel and aggregate the results
adj_matrix_individual_list <- mclapply(colnames(forum_author_matrix), process_author, mc.cores = no_cores)
names(adj_matrix_individual_list) <- colnames(forum_author_matrix)
toc() # 6.71 sec elapsed


# ---- Individual-level rescaling / normalization (adj_matrix_individual) if needed ----
# case 1: if the absolute "drop" value should be kept, such as the sentiment score, no need to change each adj_matrix_individual in the adj_matrix_individual_list 
# case 2: if non-zero edge should be count as 1 connection, make each adj_matrix_individual binary
binary_indi <- function(m){
  m[m>0]<-1 
  return(m)
}
adj_matrix_individual_list <- lapply(adj_matrix_individual_list, binary_indi)

# ---- Aggregate across individuals ----
agg_adj_matrix<- Reduce(`+`, adj_matrix_individual_list)
print(dim(agg_adj_matrix))
colnames(agg_adj_matrix) <- names(forum_author_matrix[,1])
rownames(agg_adj_matrix) <- names(forum_author_matrix[,1]) # maintain names
# remove disconnected nodes 
in_deg = colSums(agg_adj_matrix) # in degrees
out_deg = rowSums(agg_adj_matrix) # out degrees
disconnected_nodes <- intersect(names(in_deg[in_deg==0]), names(out_deg[out_deg==0])) # remove both zeroed in "in and out degrees" 
connected_nodes <- setdiff(colnames(agg_adj_matrix), disconnected_nodes)
agg_adj_matrix <- agg_adj_matrix[connected_nodes, connected_nodes]
print(dim(agg_adj_matrix))


# ---- Forum-level rescaling / normalization if needed -----
if(net_type=="co_conflict"){
  # --- case 1: use "jaccard coefficient" + undirected co-conflict network  ---
  # normalize common controversial author counts by the number of distinct sanctioned authors in either subreddits.
  # for each forum, get the list of "negative" authors
  neg_author_list <- list()
  for(f in rownames(agg_adj_matrix)){
    neg_author_list[[f]] <- colnames(forum_author_matrix)[forum_author_matrix[f,]<0&!is.na(forum_author_matrix[f,])]
  }
  union_neg_matrix <- matrix(0, nrow=length(neg_author_list), ncol=length(neg_author_list))
  union_neg_list <- list()
  rownames(union_neg_matrix) <- names(neg_author_list)
  colnames(union_neg_matrix) <- names(neg_author_list)
  for(f1 in names(neg_author_list)){
    union_neg_list[[f1]] <- list()
    for(f2 in names(neg_author_list)){
      union_neg_list[[f1]][[f2]] <- unique(c(neg_author_list[[f1]], neg_author_list[[f2]])) # collect union distinct neg authors
      union_neg_matrix[f1,f2] <- n_distinct(union_neg_list[[f1]][[f2]])# count union distinct neg authors
    }
  }
  # remove direction
  adj_matrix <- agg_adj_matrix + t(agg_adj_matrix)
  if(jaccard){
    adj_matrix <- adj_matrix/union_neg_matrix
  }
  adj_matrix[is.na(adj_matrix)] <- 0 # fix nan introduced by deviding by zero problem
}

if(net_type=="conflict"){
  # --- case 2: use "common-author-normalization" + directed conflict network  ---
  # normalize the controversial author counts by the number of common authors in both subreddits.
  # find the common authors
  bipartite_forum_author_matrix <- forum_author_matrix[rownames(agg_adj_matrix),] 
  bipartite_forum_author_matrix[!is.na(bipartite_forum_author_matrix)] <- 1
  bipartite_forum_author_matrix[is.na(bipartite_forum_author_matrix)] <- 0
  common_author_adj_matrix <- bipartite_forum_author_matrix %*% t(bipartite_forum_author_matrix)
  adj_matrix <- agg_adj_matrix / common_author_adj_matrix
  adj_matrix[is.na(adj_matrix)] <- 0
}

adj_matrix <- clean_adj_matrix(adj_matrix)
# ---- make the igraph object ----
if(identical(adj_matrix, t(adj_matrix))){
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)
}else{
  g <- graph_from_adjacency_matrix(adj_matrix, weighted = TRUE)
}
summary(g)
attr_df <- merge(data.frame("sr_name" = V(g)$name), dplyr::distinct(df[,c("sr_name","public_description", "group")]), all.x=TRUE)# add description
name_to_group <- setNames(attr_df$group, attr_df$sr_name)
V(g)$group <- name_to_group[V(g)$name]# Assign group to g_proj using vectorized indexing
name_to_descr <- setNames(attr_df$public_description, attr_df$sr_name)
V(g)$description <- name_to_descr[V(g)$name]# Assign group to g_proj using vectorized indexing
g <- sanity_check_g(g)
# Plot the graph using the Fruchterman-Reingold layout
plot(g, layout = layout_with_fr(g), 
     vertex.size = 3, 
     vertex.label = V(g)$name,
     vertex.label.cex = 0.5,  # Adjust label size
     vertex.label.degree = 0, # This centers the label on the vertex
     vertex.label.dist = 0,   # Increase label distance
     edge.arrow.size = 0.5, 
     vertex.color = "skyblue", 
     edge.color = "grey")


# # --- test code ---
# forum_vector <- c("a"=1, "b"=NA,"c"=-1, "d"=NA)
# repeated_matrix <- matrix(rep(forum_vector, each = length(forum_vector)), nrow = length(forum_vector), byrow = TRUE)
# adj_matrix_individual <- repeated_matrix - t(repeated_matrix) # only non-na will return values
# adj_matrix_individual[is.na(adj_matrix_individual)] <- 0 # replace NA with 0 for final sum-up (means no up/down voting change)
# adj_matrix_individual[adj_matrix_individual<0] <- 0
# adj_matrix_individual
# sum(adj_matrix_individual[upper.tri(adj_matrix_individual)])

