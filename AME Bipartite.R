if (!requireNamespace("amen", quietly = TRUE)) {
  install.packages("amen")
}
library(amen)

# Set seed for reproducibility
set.seed(123)

# Define the number of sender and receiver nodes
n_s <- 5
n_r <- 10

# Create covariates for sender nodes
#X_s <- rnorm(n_s, mean = 0, sd = 1)
X_s_dim <- 2
X_s <- matrix(rnorm(n_s * X_s_dim), nrow = n_s, ncol = X_s_dim)

# Create covariates for receiver nodes
#X_r <- rnorm(n_r, mean = 0, sd = 1)
X_r_dim <- 3
X_r <- matrix(rnorm(n_r * X_r_dim), nrow = n_r, ncol = X_r_dim)


# Create dyadic variable from noramal distribution with dim = n_s * n_r
X_d_dim <- 1
X_d <- array(rnorm(n_s * n_r * X_d_dim), dim = c(n_s, n_r, X_d_dim))

# Create a binary adjacency matrix with dim = n_s * n_r
Y <- matrix(rbinom(n_s * n_r, size = 1, prob = 0.5), nrow = n_s, ncol = n_r)

# Design Matrix with dimension n_s * n_r * (1 + X_s_dim + X_r_dim + X_d_dim)
X <- array(NA, dim = c(n_s, n_r, 1 + X_s_dim + X_r_dim + X_d_dim))

# Set first dim to 1 for intercept
X[, , 1:1] <- 1

# Fill in X_s values for dimension 2 to X_s_dim + 1
dim_start <- 2
for (i in 1:X_s_dim) {
  X[, , dim_start + i - 1] <- X_s[, i]
}

# Fill in X_r values for dimension X_s_dim + 2 to X_s_dim + X_r_dim + 1
dim_start <- dim_start + X_s_dim
for (i in 1:X_r_dim) {
  X[, , dim_start + i - 1] <- array(t(rep(X_r[,i], each = n_s)), dim= c(n_s,n_r))
}


# Fill in X_d values for dimension X_s_dim + X_r_dim + 2 to X_s_dim + X_r_dim + X_d_dim + 1
dim_start <- dim_start + X_r_dim
for (i in 1:X_d_dim) {
  X[, , dim_start + i - 1] <- X_d[, , i]
}


