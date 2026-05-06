################################################################################
##################### Functions #####################

### Generate the adjacency matrix of the connected circular lattice graph ###
lattice_graph <- function(m, deg) {
  W <- 0 * diag(m)
  for (i in 1:deg) {
    diag(W[-(1:i), -((m - i + 1):m)]) <- 1
    W[i, (m - deg + i):m] <- 1
  }
  return(W + t(W) + diag(m))
}

### Generate Laplacian matrix based on the adjacency matrix ###
Laplacian_matrix <- function(A){
  diag(A) <- 0
  degree <- rowSums(A)
  D <- diag(degree)
  L <- D - A
  
  return(L)
}

### Generate a diagonal tensor and return the vector and diagonal positions ###
## dims: diagonal tensor dimension (dims*dims*dims)
create_diag_tensor <- function(dims, diag_values = seq_len(min(dims))) {
  ## Create a diagonal tensor
  tensor <- array(0, dim = dims)
  n <- min(dims)
  for(i in 1:n) {
    indices <- as.list(rep(i, length(dims)))
    tensor <- do.call(`[<-`, c(list(tensor), indices, diag_values[i]))
  }
  
  ## Convert to a vector
  vec <- as.vector(tensor)
  
  ## Find the diagonal position
  positions <- 1
  stride <- 1
  for(k in 1:length(dims)) {
    stride <- stride * dims[k]
    positions <- positions + (0:(n-1)) * (stride / dims[k])
  }
  
  ## Return results
  list(
    tensor = tensor,
    vector = vec,
    diag_positions = positions,
    diag_values = vec[positions]
  )
}

### Generating matrix via outer products to construct mean tensor coefficients with tensor normal distribution ###
matrix_power <- function(t, s) {
  outer(t, 0:(s-1), "^")
}

### Compute the loss function ###
loss_function <- function(y,x,theta,lambda){
  prod_d <- dim(x)[2]
  m <- length(theta) / prod_d
  n <- dim(x)[1] / m
  
  loss_fun <- 0
  for(k in 1:m){
    mode1_mat <- k_unfold(as.tensor(array(theta[((k-1)*prod_d+1):(k*prod_d)], dim = d)), m = 1)@data
    mode2_mat <- k_unfold(as.tensor(array(theta[((k-1)*prod_d+1):(k*prod_d)], dim = d)), m = 2)@data
    mode3_mat <- k_unfold(as.tensor(array(theta[((k-1)*prod_d+1):(k*prod_d)], dim = d)), m = 3)@data
    
    svd_result1 <- svd(mode1_mat)
    svd_result2 <- svd(mode2_mat)
    svd_result3 <- svd(mode3_mat)
    
    nuclear_norm <- (sum(svd_result1$d) + sum(svd_result2$d) + sum(svd_result3$d))/3
    
    loss_fun <- loss_fun + mean(pmax(1 - y[((k-1)*n+1):(k*n)]*(x[((k-1)*n+1):(k*n),] %*% theta[((k-1)*prod_d+1):(k*prod_d)]),0)) + lambda*nuclear_norm
  }
  return(loss_fun/m)
}

### Compute the gradient ###
gradient_loss <- function(y,x,theta){
  prod_d <- dim(x)[2]
  m <- length(theta) / prod_d
  n <- dim(x)[1] / m
  
  gd_loss <- rep(0,m*prod_d)
  for(k in 1:m){
    gd_loss[((k-1)*prod_d+1):(k*prod_d)] <- - colMeans(x[((k-1)*n+1):(k*n),]*as.vector(y[((k-1)*n+1):(k*n)]*((y[((k-1)*n+1):(k*n)]*x[((k-1)*n+1):(k*n),]%*%theta[((k-1)*prod_d+1):(k*prod_d)]) <= 1)))
  }
  return(gd_loss)
}
