# Lalith Devireddy
# Stat 534 HW1

# Problem 1 Solution
logdet <- function(R) # computes logarithm of determinant of a square matrix (assuming nondegenerate)
{
  eigenvals = eigen(R,only.values=T)$values
  eigen_sum = sum(log(eigenvals))
  return (eigen_sum)
}

# Problem 2 Solution
logmarglik <- function(data,A) # computes logarithm of marginal likelihood given data frame and vector of columns
{
  A_len = length(A)
  D_1 = as.matrix(data[,1])
  D_A = as.matrix(data[,A])
  n = length(D_1)
  M_A = diag(A_len) + t(D_A)%*%D_A
  log_gamma_result = lgamma((n+A_len+2)/2)-lgamma((A_len+2)/2)
  log_det_result = -logdet(M_A)/2
  matrix_result = 1 + t(D_1)%*%D_1 - t(D_1)%*%D_A%*%solve(M_A)%*%t(D_A)%*%D_1
  log_matrix_result = -(n+A_len+2)*log(matrix_result)/2
  marg_lik = log_gamma_result + log_det_result + log_matrix_result
  return (marg_lik)
}

load_data <- function() # reads in erdata.txt file
{
  table = read.table("erdata.txt",sep="\t",header=F)
  return (table)
}

data = load_data()
logmarglik(data,c(2,5,10)) # result: -59.97893