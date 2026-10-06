################################################################################
##################### Functions #####################

### Compute the nuclear norm ###
nuclear_norm <- function(X) {
  sum(svd(X)$d)
}

### Compute the rank num ###
rank_num <- function(X,etol = 0.003) {
  sum((svd(X)$d > etol))
}

### Compute the weight matrix by maximum-degree rule ###
weight_matrix <- function(W) {
  m <- nrow(W)
  degree <- rowSums(W) - 1
  d_max <- max(degree)
  
  weight_matrix <- matrix(0, m, m)
  
  edge_indices <- which(W == 1 & upper.tri(W), arr.ind = TRUE)
  for(k in 1:nrow(edge_indices)) {
    i <- edge_indices[k, 1]
    j <- edge_indices[k, 2]
    weight_matrix[i,j] <- weight_matrix[j,i] <- 1 / d_max
  }
  
  diag(weight_matrix) <- 1 - degree / d_max
  return(weight_matrix)
}

### Compute the loss function with one penalty ###
loss_function_1 <- function(y,x,theta,lambda){
  prod_d <- dim(x)[2]
  m <- length(theta) / prod_d
  n <- dim(x)[1] / m
  
  loss_fun <- 0
  for(k in 1:m){
    mode1_mat <- k_unfold(as.tensor(array(theta[((k-1)*prod_d+1):(k*prod_d)], dim = d)), m = 1)@data
    
    svd_result1 <- svd(mode1_mat)
    
    nuclear_norm <- (sum(svd_result1$d))/3
    
    loss_fun <- loss_fun + mean(pmax(1 - y[((k-1)*n+1):(k*n)]*(x[((k-1)*n+1):(k*n),] %*% theta[((k-1)*prod_d+1):(k*prod_d)]),0)) + lambda*nuclear_norm
  }
  return(loss_fun/m)
}

### Compute the loss function without penalty ###
loss_function_0 <- function(y,x,theta){
  prod_d <- dim(x)[2]
  m <- length(theta) / prod_d
  n <- dim(x)[1] / m
  
  loss_fun <- 0
  for(k in 1:m){
    loss_fun <- loss_fun + mean(pmax(1 - y[((k-1)*n+1):(k*n)]*(x[((k-1)*n+1):(k*n),] %*% theta[((k-1)*prod_d+1):(k*prod_d)]),0))
  }
  return(loss_fun/m)
}