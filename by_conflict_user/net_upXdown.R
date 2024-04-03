rm(list = ls())
setwd("/Users/joyqiu/Documents/Research/ED Media/network")
source("./script/sr_network/by_conflict_author/net.R")


matrix_u <- reshape2::acast(df_agg[,c("sr_name","author_id", "u")], sr_name ~ author_id, value.var = "u", fill = 0)
matrix_d <- reshape2::acast(df_agg[,c("sr_name","author_id", "l")], sr_name ~ author_id, value.var = "l", fill = 0)
adj_matrix <- matrix_u%*%t(matrix_d)
diag(adj_matrix) <- 0# remove diagonal self-connection
g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
summary(g)
attr_df <- merge(data.frame("sr_name" = V(g)$name), dplyr::distinct(df[,c("sr_name","public_description", "group")]), all.x=TRUE)# add description
name_to_group <- setNames(attr_df$group, attr_df$sr_name)
V(g)$group <- name_to_group[V(g)$name]# Assign group to g_proj using vectorized indexing
name_to_descr <- setNames(attr_df$public_description, attr_df$sr_name)
V(g)$description <- name_to_descr[V(g)$name]# Assign group to g_proj using vectorized indexing
g <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
write_graph(g, paste0("./script/sr_network/by_conflict_author/res/conflict_",round(ucp*100),"_",round(lcp*100),".gml"), format = "gml")



# remove control group
# df <- df %>% filter(group%in%c("ed", "diet", "body", "cognative") ) %>% as.data.frame()

# lcp <- 0.2
# ucp <- 0.8
# df$author <- df$author_id
# # group by sr calculate average upvote_ratio as the baseline upvote_ratio cotoff
# 
# sr_score <- df %>% group_by(sr_name) %>%
#   summarise(sr_ur_l = round(quantile(as.numeric(upvote_ratio), lcp, na.rm=TRUE),2),
#             sr_ur_u = round(quantile(as.numeric(upvote_ratio), ucp,na.rm=TRUE),2) ) %>%
#   as.data.frame()
# # find authors that co-post at each pair of sr_name
# down_score <- merge(df,sr_score,all.x=TRUE) %>% group_by(author) %>% 
#   filter(upvote_ratio<sr_ur_l) %>%
#   mutate(sr_name_l = sr_name,
#          ur_l = upvote_ratio,
#          sr_l_ur = paste0(sr_name, "=", sr_ur_l),
#          sr_ur = paste0(sr_name, "=", upvote_ratio)) %>%
#   select(author, sr_name, sr_l_ur, ur_l) %>%
#   as.data.frame()
# up_score <- merge(df,sr_score,all.x=TRUE) %>% group_by(author) %>% 
#   filter(upvote_ratio>sr_ur_u) %>%
#   mutate(sr_name_u = sr_name,
#          ur_u = upvote_ratio,
#          sr_u_ur = paste0(sr_name, "=", sr_ur_u),
#          sr_ur = paste0(sr_name, "=", upvote_ratio)) %>%
#   select(author, sr_name, sr_u_ur, ur_u) %>%
#   as.data.frame()
# cp <- merge(down_score, up_score, all=TRUE)
# 
# 
# # at least one up sr and one down sr
# cfl_authors <- cp %>% group_by(author) %>%
#   summarise(nu = sum(!is.na(ur_u)),
#             nl = sum(!is.na(ur_l)) ) %>%
#   filter(nu>0&nl>0) %>%
#   as.data.frame()
# 
# cfl <- cp[which(cp$author%in%cfl_authors$author),] %>%
#   # filter(!( (!is.na(ur_l))&(!is.na(ur_u)) ) ) %>%
#   as.data.frame()
# 
# cfl_df <- distinct(cfl[,c("author", "sr_name")])
# 
# edge_list <- as.matrix(cfl_df[,c("sr_name", "author")])
# # Create a graph from the edge list
# g <- graph_from_edgelist(edge_list, directed = FALSE)
# # Add bipartite attribute
# V(g)$type <- bipartite_mapping(g)$type
# proj <- bipartite_projection(g) # proj is a list containing two projections: proj$proj1 for the first set (column1) and proj$proj2 for the second set (column2)
# g_proj <- proj$proj1 # first column
# summary(g_proj)
# attr_df <- merge(data.frame("sr_name" = V(g_proj)$name), dplyr::distinct(df[,c("sr_name","public_description", "group")]), all.x=TRUE, by="sr_name")# add description
# name_to_group <- setNames(attr_df$group, attr_df$sr_name)
# V(g_proj)$group <- name_to_group[V(g_proj)$name]# Assign group to g_proj using vectorized indexing
# name_to_descr <- setNames(attr_df$public_description, attr_df$sr_name)
# V(g_proj)$description <- name_to_descr[V(g_proj)$name]# Assign group to g_proj using vectorized indexing
# g_proj <- simplify(g_proj, remove.multiple = F, remove.loops = T)
# summary(g_proj)
# 
# write_graph(g_proj, paste0("./script/sr_network/by_conflict_author/res/conflict_",round(ucp*100),"_",round(lcp*100),".gml"), format = "gml")
# 
