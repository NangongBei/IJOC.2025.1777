##### Packages #################################################################
library(glmnet)
library(MASS)
library(openxlsx)
library(rTensor)

setwd("C:/Users/USTC/Desktop/ScienceWork/CityU/SVM/Tensor ADMM/code_github")
source('src/functions.R')
source('src/functions_compare.R')

##### initialization ###########################################################
repeat_time <- 1 # Number of repetitions

# Sample size for simulations
n <- 10^3

d <- c(5,5,5) # Tensor dimension
# lambda <- 0.1 # Regularization parameter

prod_d <- prod(d) # The corresponding dimension after converting a tensor into a vector
pi1 <- 0.5 # the ratio y = 1
pi2 <- 1 - pi1 # the ratio y = -1

m <- 20 # Number of nodes
deg <- 5 # Connectivity
W <- lattice_graph(m,deg) # circular lattice graph
LpW <- Laplacian_matrix(W) # Laplacian matrix for networks

degree <- rowSums(W) - 1

# Step size and iteration number for our STM algorithm ##
rho <- 0.5 
c <- 3
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
    theta_new_all <- theta_old_all <- rep(0,prod_d)
    
    # DLM Algorithm Flow
    for(tgd in 1:Maxiter){
      # Update theta
      theta_new_all <- theta_old_all - (gradient_loss(y,x,theta_old_all)) / c
      
      # Update theta
      theta_old_all <- theta_new_all
      
      # Record the error and loss for each iteration
      error_all[tgd] <- norm(theta_new_all-beta_plus_star,"2")^2
      Q_loss_all[tgd] <- loss_function_0(y,x,theta_new_all)
    }
    
    ##### Decentralized part ##############################################################
    # Record
    error <- Q_loss <- error_to_tall <- Q_loss_to_tall <- 
      mat1_nuclear_norm <- mat2_nuclear_norm <- mat3_nuclear_norm <- 
      mat1_rank_num <- mat2_rank_num <- mat3_rank_num <- numeric(Maxiter)
    theta_new_tall <- theta_new <- theta_old <- alphaM_new <- rep(0,m*prod_d)
    
    # DLM Algorithm Flow
    for(tgd in 1:Maxiter){
      # Update theta
      theta_new <- theta_old - (rho*as.vector(matrix(theta_old, nrow = prod_d, ncol = m)%*%LpW) - 
                                  alphaM_new + gradient_loss(y,x,theta_old))/(c+2*rho*rep(degree,each = prod_d))
      
      
      # Update alpha
      alphaM_new <- alphaM_new - rho/2*2*as.vector(matrix(theta_new, nrow = prod_d, ncol = m)%*%LpW)
      theta_old <- theta_new
      theta_new_tall <- theta_new_tall + theta_new
      
      # Record the error and loss for each iteration
      error[tgd] <- mean(apply(matrix(theta_new,prod_d,m)-beta_plus_star,2,norm,"2")^2)
      Q_loss[tgd] <- loss_function_0(y,x,theta_new)
      
      # Record the error and loss for each iteration
      error_to_tall[tgd] <- mean(apply(matrix(theta_new_tall / tgd,prod_d,m) - theta_new_all,2,norm,"2")^2)
      Q_loss_to_tall[tgd] <- loss_function_0(y,x,theta_new_tall / tgd) - Q_loss_all[Maxiter]
      
      # Record the nuclear norm and rank number for each iteration
      mat1_nn <- mat2_nn <- mat3_nn <- 0
      mat1_rn <- mat2_rn <- mat3_rn <- 0
      for(k in 1:m){
        mode1_mat <- k_unfold(as.tensor(array(theta_new[((k-1)*prod_d+1):(k*prod_d)], dim = d)), m = 1)@data
        mode2_mat <- k_unfold(as.tensor(array(theta_new[((k-1)*prod_d+1):(k*prod_d)], dim = d)), m = 2)@data
        mode3_mat <- k_unfold(as.tensor(array(theta_new[((k-1)*prod_d+1):(k*prod_d)], dim = d)), m = 3)@data
        
        mat1_nn <- mat1_nn + nuclear_norm(mode1_mat)
        mat2_nn <- mat2_nn + nuclear_norm(mode2_mat)
        mat3_nn <- mat3_nn + nuclear_norm(mode3_mat)
        
        mat1_rn <- mat1_rn + rank_num(mode1_mat)
        mat2_rn <- mat2_rn + rank_num(mode2_mat)
        mat3_rn <- mat3_rn + rank_num(mode3_mat)
      }
      
      mat1_nuclear_norm[tgd] <- mat1_nn / m
      mat2_nuclear_norm[tgd] <- mat2_nn / m
      mat3_nuclear_norm[tgd] <- mat3_nn / m
      
      mat1_rank_num[tgd] <- mat1_rn / m
      mat2_rank_num[tgd] <- mat2_rn / m
      mat3_rank_num[tgd] <- mat3_rn / m
    }
    
    # Recording the convergence process of algorithm
    result_ret <- cbind(error,Q_loss,error_all,Q_loss_all,error_to_tall,Q_loss_to_tall,
                        mat1_nuclear_norm,mat2_nuclear_norm,mat3_nuclear_norm,
                        mat1_rank_num,mat2_rank_num,mat3_rank_num)
    filename_ret <- paste0("results/compare/DLM_0_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,"_d",prod_d,"_ret",ret,"_rho",rho,"_c",c,"_Mxi",Maxiter,".csv")
    write.csv(result_ret,filename_ret)
    
  }


