##### Packages #################################################################
library(glmnet)
library(MASS)
library(openxlsx)
library(rTensor)

setwd("C:/Users/USTC/Desktop/ScienceWork/CityU/SVM/Tensor ADMM/code_github")
source('src/functions.R')

##### initialization ###########################################################
repeat_time <- 1 # Number of repetitions

# Sample size set for simulations
n_min <- 2 
n_max <- 5
n_interval <- 1
n_all <- floor(10^seq(n_min,n_max,n_interval))

d <- c(5,5,5) # Tensor dimension
lambda <- 0.1 # Regularization parameter

prod_d <- prod(d) # The corresponding dimension after converting a tensor into a vector
pi1 <- 0.5 # the ratio y = 1
pi2 <- 1 - pi1 # the ratio y = -1

m <- 20 # Number of nodes
deg <- 5 # Connectivity
W <- lattice_graph(m,deg) # circular lattice graph
LpW <- Laplacian_matrix(W) # Laplacian matrix for networks

# Step size and iteration number for our STM algorithm ##
rho <- 0.5 
c <- 10
Maxiter <- 2000

##### coefficients initialization with tensor normal distribution ##############
s <- c(3,3,3)

# Variance for tensor normal distribution ###
Sigma1 <- diag(1,d[1],d[1])
Sigma1[1:s[1],1:s[1]] <- diag(0.7,s[1],s[1]) + matrix(0.3,s[1],s[1])

Sigma2 <- diag(1,d[2],d[2])
Sigma2[1:s[2],1:s[2]] <- diag(0.7,s[2],s[2]) + matrix(0.3,s[2],s[2])

Sigma3 <- diag(1,d[3],d[3])
Sigma3[1:s[3],1:s[3]] <- diag(0.7,s[3],s[3]) + matrix(0.3,s[3],s[3])

Sigma <- kronecker(kronecker(Sigma3, Sigma2), Sigma1)

# Mean for tensor normal distribution ###
muf_tensor <- create_diag_tensor(s, c(0.5*rep(1,min(s))))
diag_positions <- (muf_tensor$diag_positions)[1:min(s)]

B <- muf_tensor$tensor
A <- matrix_power(0.5*(1:d[1]),s[1])
C <- matrix_power(0.3*(1:d[2]),s[2])
D <- matrix_power(0.1*(1:d[3]),s[3])

QA <- qr.Q(qr(A))
QC <- qr.Q(qr(C))
QD <- qr.Q(qr(D))

M <- ttl(as.tensor(B), list(QA, QC, QD), ms = c(1, 2, 3))@data
muf <- as.vector(M)
mug <- - muf

##### Calculate the true values of the intercept and tensor coefficients #######
inv_Sigma <- solve(Sigma)
Mahalanobis_distance <- as.numeric(sqrt(t(muf - mug) %*% inv_Sigma %*% (muf - mug)))
Bayes_error_rate <- pi1 * pnorm(-Mahalanobis_distance / 2 - 1/Mahalanobis_distance*log(pi1/pi2)) +
  pi2*pnorm(-Mahalanobis_distance / 2 + 1/Mahalanobis_distance*log(pi1/pi2))

Gamma <- function(a) {dnorm(a,0,1)/pnorm(a,0,1) - Mahalanobis_distance/2}
Gamma_a <- uniroot(Gamma, interval = c(-max(Mahalanobis_distance,3), max(Mahalanobis_distance,3)),tol = 1e-10)
a_star <- Gamma_a$root

# True intercept
beta0_star <- - as.numeric(t(muf - mug) %*% inv_Sigma %*% (muf + mug)) / (2*a_star*Mahalanobis_distance+Mahalanobis_distance^2)
# True tensor coefficients represented as vector
beta_plus_star <- 2*inv_Sigma %*% (muf - mug) / (2*a_star*Mahalanobis_distance+Mahalanobis_distance^2)
beta_plus_star <- as.vector(beta_plus_star)

norm2_beta_star <- norm(beta_plus_star,"2")

##### simulations ##############################################################
# Record
repeat_n <- length(n_all)
theta_new_sum <- theta_mean <- matrix(0,repeat_n,prod_d*m)
error_sum <- Nerror_sum <- matrix(0,repeat_n,repeat_time)

for(re in 1:repeat_n){
  # Sample size
  n <- n_all[re]
  for(ret in 1:repeat_time){
    set.seed(ret) # Set the random seed
    
    # Generate random data
    y <- sample(c(1, -1), size = n*m, replace = TRUE, prob = c(pi1, 1-pi1))
    x <- matrix(NA, nrow = n*m, ncol = prod_d)
    x[y == 1,] <- mvrnorm(sum(y == 1), mu = muf, Sigma = Sigma)
    x[y == -1,] <- mvrnorm(sum(y == -1), mu = mug, Sigma = Sigma)
    
    ##### Global Part ##############################################################
    # Record
    error_all <- Q_loss_all <- numeric(Maxiter)
    theta_new_all <- theta_old_all <- u1_new_all <- v2_new_all <- w3_new_all <- 
      lambda_u1_new_all <- lambda_v2_new_all <- lambda_w3_new_all <- rep(0,prod_d)
    
    # Algorithm Flow
    for(tgd in 1:Maxiter){
      # Update theta
      theta_new_all <- (lambda_u1_new_all + lambda_v2_new_all + lambda_w3_new_all + 
                          rho*(u1_new_all + v2_new_all + w3_new_all) + (c*theta_old_all) - 
                          gradient_loss(y,x,theta_old_all)) / (c+3*rho)
      
      
      # Mode-K Expand
      mode1_mat <- k_unfold(as.tensor(array(theta_new_all - lambda_u1_new_all / rho, dim = d)), m = 1)@data
      mode2_mat <- k_unfold(as.tensor(array(theta_new_all - lambda_v2_new_all / rho, dim = d)), m = 2)@data
      mode3_mat <- k_unfold(as.tensor(array(theta_new_all - lambda_w3_new_all / rho, dim = d)), m = 3)@data
      
      svd_A1 <- svd(mode1_mat)
      r1 <- ((svd_A1$d > 1e-10))
      if(sum(r1) <= 1){
        b1_mat <- as.matrix(svd_A1$u[,r1],1) %*% pmax(svd_A1$d[r1] - lambda / (3*rho),0) %*% t(as.matrix(svd_A1$v[,r1],1))
      }else{
        b1_mat <- svd_A1$u[,r1] %*% diag(pmax(svd_A1$d[r1] - lambda / (3*rho),0)) %*% t(svd_A1$v[,r1])
      }
      
      svd_A2 <- svd(mode2_mat)
      r2 <- ((svd_A2$d > 1e-10))
      if(sum(r2) <= 1){
        b2_mat <- as.matrix(svd_A2$u[,r2],1) %*% (pmax(svd_A2$d[r2] - lambda / (3*rho),0)) %*% t(as.matrix(svd_A2$v[,r2],1))
      }else{
        b2_mat <- svd_A2$u[,r2] %*% diag(pmax(svd_A2$d[r2] - lambda / (3*rho),0)) %*% t(svd_A2$v[,r2])
      }
      
      svd_A3 <- svd(mode3_mat)
      r3 <- ((svd_A3$d > 1e-10))
      if(sum(r3) <= 1){
        b3_mat <- as.matrix(svd_A3$u[,r3],1) %*% (pmax(svd_A3$d[r3] - lambda / (3*rho),0)) %*% t(as.matrix(svd_A3$v[,r3],1))
      }else{
        b3_mat <- svd_A3$u[,r3] %*% diag(pmax(svd_A3$d[r3] - lambda / (3*rho),0)) %*% t(svd_A3$v[,r3])
      }
      
      # Update u, v, and w
      u1_new_all <- as.vector(k_fold(as.tensor(b1_mat), m = 1, modes=d)@data)
      v2_new_all <- as.vector(k_fold(as.tensor(b2_mat), m = 2, modes=d)@data)
      w3_new_all <- as.vector(k_fold(as.tensor(b3_mat), m = 3, modes=d)@data)
      
      # Update lambda_u, lambda_v and lambda_w
      lambda_u1_new_all <- lambda_u1_new_all - rho*(theta_new_all - u1_new_all)
      lambda_v2_new_all <- lambda_v2_new_all - rho*(theta_new_all - v2_new_all)
      lambda_w3_new_all <- lambda_w3_new_all - rho*(theta_new_all - w3_new_all)
      
      # Update theta
      theta_old_all <- theta_new_all
      
      # Record the error and loss for each iteration
      error_all[tgd] <- norm(theta_new_all-beta_plus_star,"2")^2
      Q_loss_all[tgd] <- loss_function(y,x,theta_new_all,lambda)
    }
    
    ##### Decentralized part ##############################################################
    # Record
    error <- Q_loss <- error_to_tall <- Q_loss_to_tall <- numeric(Maxiter)
    theta_new_tall <- theta_new <- theta_old <- u1_new <- v2_new <- w3_new <- 
      lambda_u1_new <- lambda_v2_new <- lambda_w3_new <- alphaM_new <- rep(0,m*prod_d)
    
    # Algorithm Flow
    for(tgd in 1:Maxiter){
      # Update theta
      theta_new <- (lambda_u1_new + lambda_v2_new + lambda_w3_new + alphaM_new + 
                      rho*(u1_new + v2_new + w3_new) + (c*theta_old - rho/2*2*as.vector(matrix(theta_old, nrow = prod_d, ncol = m)%*%LpW)) - 
                      gradient_loss(y,x,theta_old)) / (c+3*rho)
      
      for(k in 1:m){
        # Mode-K Expand
        mode1_mat <- k_unfold(as.tensor(array(theta_new[((k-1)*prod_d+1):(k*prod_d)] - lambda_u1_new[((k-1)*prod_d+1):(k*prod_d)] / rho, dim = d)), m = 1)@data
        mode2_mat <- k_unfold(as.tensor(array(theta_new[((k-1)*prod_d+1):(k*prod_d)] - lambda_v2_new[((k-1)*prod_d+1):(k*prod_d)] / rho, dim = d)), m = 2)@data
        mode3_mat <- k_unfold(as.tensor(array(theta_new[((k-1)*prod_d+1):(k*prod_d)] - lambda_w3_new[((k-1)*prod_d+1):(k*prod_d)] / rho, dim = d)), m = 3)@data
        
        svd_A1 <- svd(mode1_mat)
        r1 <- ((svd_A1$d > 1e-10))
        if(sum(r1) <= 1){
          b1_mat <- as.matrix(svd_A1$u[,r1],1) %*% pmax(svd_A1$d[r1] - lambda / (3*rho),0) %*% t(as.matrix(svd_A1$v[,r1],1))
        }else{
          b1_mat <- svd_A1$u[,r1] %*% diag(pmax(svd_A1$d[r1] - lambda / (3*rho),0)) %*% t(svd_A1$v[,r1])
        }
        
        svd_A2 <- svd(mode2_mat)
        r2 <- ((svd_A2$d > 1e-10))
        if(sum(r2) <= 1){
          b2_mat <- as.matrix(svd_A2$u[,r2],1) %*% (pmax(svd_A2$d[r2] - lambda / (3*rho),0)) %*% t(as.matrix(svd_A2$v[,r2],1))
        }else{
          b2_mat <- svd_A2$u[,r2] %*% diag(pmax(svd_A2$d[r2] - lambda / (3*rho),0)) %*% t(svd_A2$v[,r2])
        }
        
        svd_A3 <- svd(mode3_mat)
        r3 <- ((svd_A3$d > 1e-10))
        if(sum(r3) <= 1){
          b3_mat <- as.matrix(svd_A3$u[,r3],1) %*% (pmax(svd_A3$d[r3] - lambda / (3*rho),0)) %*% t(as.matrix(svd_A3$v[,r3],1))
        }else{
          b3_mat <- svd_A3$u[,r3] %*% diag(pmax(svd_A3$d[r3] - lambda / (3*rho),0)) %*% t(svd_A3$v[,r3])
        }
        
        # Update u, v, and w
        u1_new[((k-1)*prod_d+1):(k*prod_d)] <- as.vector(k_fold(as.tensor(b1_mat), m = 1, modes=d)@data)
        v2_new[((k-1)*prod_d+1):(k*prod_d)] <- as.vector(k_fold(as.tensor(b2_mat), m = 2, modes=d)@data)
        w3_new[((k-1)*prod_d+1):(k*prod_d)] <- as.vector(k_fold(as.tensor(b3_mat), m = 3, modes=d)@data)
      }
      
      # Update lambda_u, lambda_v and lambda_w
      lambda_u1_new <- lambda_u1_new - rho*(theta_new - u1_new)
      lambda_v2_new <- lambda_v2_new - rho*(theta_new - v2_new)
      lambda_w3_new <- lambda_w3_new - rho*(theta_new - w3_new)
      
      # Update alpha
      alphaM_new <- alphaM_new - rho/2*2*as.vector(matrix(theta_new, nrow = prod_d, ncol = m)%*%LpW)
      theta_old <- theta_new
      theta_new_tall <- theta_new_tall + theta_new
      
      # Record the error and loss for each iteration
      error[tgd] <- mean(apply(matrix(theta_new,prod_d,m)-beta_plus_star,2,norm,"2")^2)
      Q_loss[tgd] <- loss_function(y,x,theta_new,lambda)
      
      # Record the error and loss for each iteration
      error_to_tall[tgd] <- mean(apply(matrix(theta_new_tall / tgd,prod_d,m) - theta_new_all,2,norm,"2")^2)
      Q_loss_to_tall[tgd] <- loss_function(y,x,theta_new_tall / tgd,lambda) - Q_loss_all[Maxiter]
    }
    
    # Recording the convergence process of algorithm
    result_ret <- cbind(error,Q_loss,error_all,Q_loss_all,error_to_tall,Q_loss_to_tall)
    filename_ret <- paste0("results/process/STM_tall_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,"_d",prod_d,"_lam",lambda,"_ret",ret,"_rho",rho,"_c",c,"_Mxi",Maxiter,".csv")
    write.csv(result_ret,filename_ret)
    
    # Record the coefficient estimates and errors for each iteration
    theta_new_sum[re,] <- theta_new_sum[re,] + theta_new
    error_sum[re,ret] <- error[tgd]
    Nerror_sum[re,ret] <- error[tgd] / norm2_beta_star
  }
  # Calculate the mean of the estimated coefficients from repeated experiments
  theta_mean[re,] <- theta_new_sum[re,] / repeat_time
}

