packages <- c("dplyr", "ggplot2", "statnet", "igraph", "Matrix", "ndtv", "treemap", "readxl", "fastDummies")

#load/Install required packages
for (i in packages){
  if (!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
  } else {
    library(i, character.only = TRUE)
  }
}


#These codes are the initial preparation for the datasets
prepare_grad_history <- function(dir){
  grad_history <- read_xlsx(dir)
  colnames(grad_history) <- c("STD_ID",
                              "year",
                              "semester",
                              "class_name",
                              "class_number",
                              "class_sep",
                              "type",
                              "student_campus",
                              "student_division",
                              "student_major",
                              "student_department",
                              "class_campus",
                              "class_division",
                              "class_department",
                              "content_korean",
                              "content_english")
  return(grad_history)
}
prepare_grad_info <- function(dir){
  grad_info <- read_xlsx(dir)
  colnames(grad_info) <- c("STD_ID",
                           "gender",
                           "entrance_type",
                           "entrance_year",
                           "entrance_semester",
                           "re_enter_year",
                           "re_enter_semester",
                           "completion_year",
                           "completion_semester",
                           "register",
                           "course",
                           "nationality")
  return(grad_info)
}
merge_grad_datasets <- function(history, info){
  grad_merge <- merge(history, info, by = "STD_ID")
  return(grad_merge)
}

#functions used in EDA.R
#some repeated functions
show_student_department <- function(dataset, time){
  dataset %>% 
    filter(year == time) %>% 
    ggplot(aes(student_department, n, fill = student_department)) + geom_bar(stat='identity') + ggtitle(paste("N of classes by department, ", time))
}

show_class_department <- function(dataset, time){
  dataset %>% 
    filter(year == time) %>% 
    ggplot(aes(class_department, n, fill = class_department)) + geom_bar(stat='identity') + ggtitle(paste("N of classes by department, ", time))
}


#functions used in graph.R
history_year <- function(dataset, time){
  grad_history_year <- dataset %>% filter(year==time)
  return(grad_history_year)
}

edge_list <- function(dataset){
  division_edge_list <- as.data.frame(cbind(dataset$class_number, dataset$student_division))
  colnames(division_edge_list) <- c("class_number", "student_division")
  return(division_edge_list)
}

edge_list_class <- function(dataset){
  division_edge_list <- as.data.frame(cbind(dataset$student_division, dataset$class_division))
  colnames(division_edge_list) <- c("student_division", "class_division")
  return(division_edge_list)
}

create_onemode_byDivison <- function(dataset) {
  A <- spMatrix(nrow=length(unique(dataset$class_number)),
                ncol=length(unique(dataset$student_division)),
                i = as.numeric(factor(dataset$class_number)),
                j = as.numeric(factor(dataset$student_division)),
                x = rep(1, length(as.numeric(dataset$class_number))) )
  row.names(A) <- levels(factor(dataset$class_number))
  colnames(A) <- levels(factor(dataset$student_division))
  A #bipartite matrix object
  matrix_adj <- tcrossprod(t(A)) #cross-product of transpose(A), rows are student_division
  matrix_adj <- as.matrix(matrix_adj)
  print(matrix_adj)
  return(matrix_adj)
}

create_onemode_byClass <- function(dataset) {
  A <- spMatrix(nrow=length(unique(dataset$student_division)),
                ncol=length(unique(dataset$class_division)),
                i = as.numeric(factor(dataset$student_division)),
                j = as.numeric(factor(dataset$class_division)),
                x = rep(1, length(as.numeric(dataset$student_division))) )
  row.names(A) <- levels(factor(dataset$student_division))
  colnames(A) <- levels(factor(dataset$class_division))
  A #bipartite matrix object
  matrix_adj <- tcrossprod(t(A)) #cross-product of transpose(A), rows are class_division
  matrix_adj <- as.matrix(matrix_adj)
  print(matrix_adj)
  return(matrix_adj)
}

create_onemode_byStudent <- function(dataset) {
  A <- spMatrix(nrow=length(unique(dataset$class_division)),
                ncol=length(unique(dataset$student_division)),
                i = as.numeric(factor(dataset$class_division)),
                j = as.numeric(factor(dataset$student_division)),
                x = rep(1, length(as.numeric(dataset$class_division))) )
  row.names(A) <- levels(factor(dataset$class_division))
  colnames(A) <- levels(factor(dataset$student_division))
  A #bipartite matrix object
  matrix_adj <- tcrossprod(t(A)) #cross-product of transpose(A), rows are class_division
  matrix_adj <- as.matrix(matrix_adj)
  print(matrix_adj)
  return(matrix_adj)
}

create_net <- function(adj){
  temp <- as.network(adj, directed=FALSE)
  return(temp)
}

plot_net <- function(net){
  gplot(net, 
        gmode = "graph", 
        usearrows = FALSE, 
        displaylabels = TRUE, 
        edge.col = "gray",
        label.cex = 0.5,
        vertex.border = 0,
        label.pos = 5)
}

division_list_year <- function(network){
  temp <- c()
  len <- length(network[["val"]])
  for (i in 1:len){
    print(network[["val"]][[i]][["vertex.names"]])
    temp <- c(temp, network[["val"]][[i]][["vertex.names"]])
  }
  temp <- as.data.frame(temp)
  return(temp)
}

division_centrality_year <- function(netw, list){
  deg <- c()
  betw <- c()
  clo <- c()
  eigen <- c()
  deg <- sna::degree(netw, gmode="graph")
  betw <- sna::betweenness(netw, gmode="graph")
  clo <- sna::closeness(netw, gmode="graph", cmode="gil-schmidt")
  eigen <- sna::evcent(netw, gmode="graph")
  temp <- cbind(list, deg, betw, clo, eigen)
  colnames(temp) <- c("student_division", "degree", "betweenness", "closeness", "eigen")
  return(temp)
}

plot_net_kc <- function(net, kc){
  gplot(net, 
        gmode = "graph", 
        usearrows = FALSE, 
        displaylabels = TRUE, 
        edge.col = "gray",
        label.cex = 0.2,
        label.pos = 2,
        vertex.border = 0,
        vertex.col=kc)
}

#dynamic
#please consult https://statnet.org/Workshops/ndtv_workshop.html
set_square_matrix <- function(year_list, year_adj){
  common <- matrix(0, nrow=nrow(year_list), ncol=nrow(year_list))
  for (i in 1:nrow(common)){
    for (j in 1:ncol(common)){
      if (i==j){
        common[i,j] = 1
      } else {
        common[i,j] = 0
      }
    }
  }
  
  ccc <- c()
  for (i in 1:nrow(division_list_all)){
    ccc <- c(ccc, division_list_all[i,])
  }
  
  cccc <- c()
  for (i in 1:nrow(year_list)){
    cccc <- c(cccc, year_list[i,])
  }
  
  colnames(common) <- cccc
  row.names(common) <- cccc
  
  common_temp <- as.data.frame(year_adj)
  common_temp[ccc[!(ccc %in% colnames(common_temp))]] = 0
  common_temp_transpose <- t(common_temp)
  common_temp_transpose <- common_temp_transpose %*% common
  
  for (i in 1:nrow(common_temp_transpose)){
    for (j in 1:ncol(common_temp_transpose)){
      if (i==j){
        common_temp_transpose[i, j] = 1
      } else {
        common_temp_transpose[i,j] = 0
      }
    }
  }
  mid_temp <- as.matrix(common_temp_transpose) %*% as.matrix(common_temp)
  
  un1 <- sort(union(rownames(mid_temp), colnames(mid_temp)))
  
  final_temp <- matrix(0, dimnames = list(un1, un1), ncol=length(un1), nrow=length(un1))
  i1 <- match(rownames(final_temp), rownames(mid_temp), nomatch = 0)
  j1 <- match(colnames(final_temp), colnames(mid_temp), nomatch = 0)
  i2 <- match(rownames(mid_temp), rownames(final_temp))
  j2 <- match(colnames(mid_temp), colnames(final_temp))
  i = 1:nrow(final_temp)
  j = 1:ncol(final_temp)
  
  final_temp[i, j] = mid_temp[i1, j1]
  final_temp <- as.matrix(final_temp)
  return(final_temp)
}

class_set_square_matrix<- function(year_list, year_adj){
  common <- matrix(0, nrow=nrow(year_list), ncol=nrow(year_list))
  for (i in 1:nrow(common)){
    for (j in 1:ncol(common)){
      if (i==j){
        common[i,j] = 1
      } else {
        common[i,j] = 0
      }
    }
  }
  
  ccc <- c()
  for (i in 1:nrow(class_list_all)){
    ccc <- c(ccc, class_list_all[i,])
  }
  
  cccc <- c()
  for (i in 1:nrow(year_list)){
    cccc <- c(cccc, year_list[i,])
  }
  
  colnames(common) <- cccc
  row.names(common) <- cccc
  
  common_temp <- as.data.frame(year_adj)
  common_temp[ccc[!(ccc %in% colnames(common_temp))]] = 0
  common_temp_transpose <- t(common_temp)
  common_temp_transpose <- common_temp_transpose %*% common
  
  for (i in 1:nrow(common_temp_transpose)){
    for (j in 1:ncol(common_temp_transpose)){
      if (i==j){
        common_temp_transpose[i, j] = 1
      } else {
        common_temp_transpose[i,j] = 0
      }
    }
  }
  mid_temp <- as.matrix(common_temp_transpose) %*% as.matrix(common_temp)
  
  un1 <- sort(union(rownames(mid_temp), colnames(mid_temp)))
  
  final_temp <- matrix(0, dimnames = list(un1, un1), ncol=length(un1), nrow=length(un1))
  i1 <- match(rownames(final_temp), rownames(mid_temp), nomatch = 0)
  j1 <- match(colnames(final_temp), colnames(mid_temp), nomatch = 0)
  i2 <- match(rownames(mid_temp), rownames(final_temp))
  j2 <- match(colnames(mid_temp), colnames(final_temp))
  i = 1:nrow(final_temp)
  j = 1:ncol(final_temp)
  
  final_temp[i, j] = mid_temp[i1, j1]
  final_temp <- as.matrix(final_temp)
  return(final_temp)
}

#in ergm.R
student_year <- function(dataset, time){
  grad_student_year <- dataset %>% filter(year==time)
  return(grad_student_year)
}

student_edge_list <- function(dataset){
  student_edge_list <- as.data.frame(cbind(dataset$class_number, dataset$STD_ID))
  colnames(student_edge_list) <- c("class_number", "STD_ID")
  return(student_edge_list)
}

create_onemode_byStudent <- function(dataset) {
  A <- spMatrix(nrow=length(unique(dataset$class_number)),
                ncol=length(unique(dataset$STD_ID)),
                i = as.numeric(factor(dataset$class_number)),
                j = as.numeric(factor(dataset$STD_ID)),
                x = rep(1, length(as.numeric(dataset$class_number))) )
  row.names(A) <- levels(factor(dataset$class_number))
  colnames(A) <- levels(factor(dataset$STD_ID))
  A #bipartite matrix object
  matrix_adj <- tcrossprod(t(A)) #cross-product of transpose(A), rows are student_division
  matrix_adj <- as.matrix(matrix_adj)
  print(matrix_adj)
  return(matrix_adj)
}

tie_prob = function(netw){
  possible_tie = (nrow(netw)*(nrow(netw)-1))/2
  summ = summary(netw ~ edges)
  summary(netw ~ edges)
  summ/possible_tie
}

