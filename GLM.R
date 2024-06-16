# Set seed for reproducibility
set.seed(123)

# Define the number of sender and receiver nodes
n_s <- 5
n_r <- 10

# Create covariates for sender nodes
#X_s <- rnorm(n_s, mean = 0, sd = 1)
X_s_dim <- 2
X_s <- matrix(rnorm(n_s * X_s_dim), nrow = n_s, ncol = X_s_dim)
X_s_1 <- X_s[, 1]
X_s_2 <- X_s[, 2]

# Create covariates for receiver nodes
#X_r <- rnorm(n_r, mean = 0, sd = 1)
X_r_dim <- 3
X_r <- matrix(rnorm(n_r * X_r_dim), nrow = n_r, ncol = X_r_dim)
X_r_1 <- X_r[, 1]
X_r_2 <- X_r[, 2]
X_r_3 <- X_r[, 3]


# Create dyadic variable from noramal distribution with dim = n_s * n_r
X_d_dim <- 1
X_d <- array(rnorm(n_s * n_r * X_d_dim), dim = c(n_s, n_r, X_d_dim))
X_d_1 <- X_d[, , 1]

# Create a binary adjacency matrix with dim = n_s * n_r
Y <- matrix(rbinom(n_s * n_r, size = 1, prob = 0.5), nrow = n_s, ncol = n_r)


##### Transform the data into tabular format
# empty data frame
df <- data.frame()

# loop over all cells in the adjacency matrix
for (i in 1:n_s) {
  for (j in 1:n_r) {
    # create a new row in the data frame
    df <- rbind(df, data.frame(sender = i, receiver = j, Y = Y[i, j], X_s_1 = X_s_1[i], X_s_2 = X_s_2[i], X_r_1 = X_r_1[j], X_r_2 = X_r_2[j], X_r_3 = X_r_3[j], X_d_1 = X_d_1[i, j]))
  }
}