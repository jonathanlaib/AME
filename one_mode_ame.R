library("amen")
library("dplyr")

bipartite_dataset <- read.csv("bipartite_dataset.csv")

length(unique(bipartite_dataset$company_id))
length(unique(bipartite_dataset$director_id))

# Get all entries where edge = 1 to see how many connections there are between director and company
edge_connection <- bipartite_dataset %>%
  filter(edge == 1) %>%
  select(director_id, company_id, edge)

# Select all the directors that have more than one connection to a company, group by director and sum the number of connections
directors_with_edges <- bipartite_dataset %>%
  group_by(director_id) %>%
  summarise(edge = sum(edge)) %>%
  filter(edge > 1) %>%
  select(director_id)


# Filter the original dataset to keep only those directors with edges > 1
specific_director_ids <- directors_with_edges$director_id

# Filter data for specified director_ids with edge = 1
connected_companies <- bipartite_dataset %>%
  filter(director_id %in% specific_director_ids & edge == 1) %>%
  select(director_id, company_id, company)

unique_companies <- unique(bipartite_dataset$company)
n <- length(unique_companies)

# Create an n x n matrix with all elements set to 0
matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(unique_companies, unique_companies))


# Loop through each director and update the matrix for each pair of companies
for (director in unique(connected_companies$director_id)) {
  # Get the companies associated with the current director
  companies <- connected_companies$company[connected_companies$director_id == director]
  # Update the matrix for each pair of companies
  for (i in seq_along(companies)) {
    for (j in seq_along(companies)) {
      if (i != j) {

        company1 <- companies[i]
        company2 <- companies[j]

        print(company1)
        print(company2)

        # Ensure the company names exist in the matrix dimensions
        if (company1 %in% rownames(matrix) && company2 %in% colnames(matrix)) {
          # Set the connection to 1 for both [i, j] and [j, i]
          matrix[company1, company2] <- 1
          matrix[company2, company1] <- 1
        }
      }
    }
  }
}

diag(matrix) <- NA
# Get the covariates for the companies
covariates_company <- read.csv("companies_covariates.csv")

covariates_company_df <- data.frame(
  company_id = covariates_company$company_id,
  sector = as.factor(covariates_company$sector),
  revenue = covariates_company$revenue
)


unique_companies <- bipartite_dataset %>% select(company_id, company) %>% distinct()

# Merge the datasets on company_id to get company names in companies_covariates
merged_data <- covariates_company_df %>%
  left_join(unique_companies, by = "company_id")

#Set company as rownames
rownames(merged_data) <- merged_data$company

# Drop the company column as it is now used as row names
merged_data <- merged_data %>% select(-company)

# Delete the company_id column as it is no longer needed
merged_data <- merged_data %>% select(-company_id)

# Ensure the sector column is a factor
merged_data$sector <- as.factor(merged_data$sector)
merged_data$revenue <- as.numeric(merged_data$revenue)

#adj_matrix <- as.matrix(matrix)
adj_matrix <- matrix

# Sort column names
column_names <- c(rownames(merged_data))
adj_matrix <- adj_matrix[, column_names]

# Sort row names
row_names <- c(rownames(merged_data))
adj_matrix <- adj_matrix[row_names, ]


# Create a one-hot encoding of the sector variable
sector_one_hot <- model.matrix(~ sector - 1, data = merged_data)
# Delete last column of sector_one_hot to avoid multicollinearity
sector_one_hot <- sector_one_hot[, -ncol(sector_one_hot)]

merged_data_one_hot <- cbind(merged_data, sector_one_hot)


# Ensure adj_matrix has company names as row and column names
rownames(adj_matrix) <- colnames(adj_matrix) <- rownames(merged_data_one_hot)

#remove sector column
merged_data_one_hot <- merged_data_one_hot %>% select(-sector)
x_cov <- as.matrix(merged_data_one_hot)



# Fit the AME model
set.seed(123)
ame_fit <- ame(Y = adj_matrix, Xdyad = NULL, Xrow = x_cov, Xcol = x_cov, family = "bin", nscan=20000, R=2, symmetric = TRUE, nvar = TRUE)


#sink("outputfile_AME_irish.txt")

# Summarize the model results
summary(ame_fit)

#sink()


# Plot the network
plot(ame_fit)

#netplot(adj_matrix, plotnames= TRUE, lcol="blue", cex=0.6, pch=30)
netplot(adj_matrix,X=NULL,xaxt="n",yaxt="n",xlab="",ylab="",
                  lcol="gray",ncol="black",lwd=1,lty=1,pch=16,
                  bty="n",plotnames=FALSE,seed=1,plot.iso=TRUE,
                  directed=NULL,add=FALSE)


