#install.packages("IrishDirectorates")
#install.packages("dplyr")
library("dplyr")
library("IrishDirectorates")
data("IrishDirectoratesData")
library("mgcv")
if (!require(gender)) {
  install.packages("gender")
}
library("gender")
#install.packages("genderdata")
library("genderdata")

# Extract components from the dataset
edgelist <- IrishDirectoratesData$edgelist
years <- IrishDirectoratesData$years
directors_names <- IrishDirectoratesData$directors_names
companies_names <- IrishDirectoratesData$companies_names

# Create df (will be easier to work with later on )
companies <- data.frame(
  company_id = seq_along(companies_names),
  company = companies_names,
  stringsAsFactors = FALSE
)

directors <- data.frame(
  director_id = seq_along(directors_names),
  director = directors_names,
  stringsAsFactors = FALSE
)

# Identify the time frame corresponding to the year 2013
time_frame_2013 <- which(years == 2013)

# Filter the edgelist for the year 2013
edgelist_2013 <- edgelist[edgelist[, 1] == time_frame_2013, ]

directors_2013 <- directors[directors$director_id %in% unique(edgelist_2013$director), ]
companies_2013 <- companies[companies$company_id %in% unique(edgelist_2013$board), ]

# Reset index for directors_2013 and companies_2013
directors_2013_df <- data.frame(
  director_id = directors_2013$director_id,
  director = directors_2013$director,
  stringsAsFactors = FALSE
)

companies_2013_df <- data.frame(
  company_id = companies_2013$company_id,
  company = companies_2013$company
)

unique_directors_2013 <- directors_2013$director

# Gender Prediction
directors_2013_with_gender <- data.frame(unique_directors_2013, stringsAsFactors = FALSE)

# Function to predict gender
predict_gender <- function(name) {
  first_name <- unlist(strsplit(name, " "))[1]  # Get the first name
  gender_prediction <- gender(first_name)

  if (length(gender_prediction) > 0) {
    return(gender_prediction$gender[1])
  } else {
    return(NA)
  }
}

# Apply the function to the data frame
directors_2013_with_gender$gender <- sapply(unique_directors_2013, predict_gender)

# Get rows where gender is NA
na_rows <- directors_2013_with_gender[is.na(directors_2013_with_gender$gender), ]

# Fill in the NA values for gender in the data frame
directors_2013_with_gender$gender[1] <- "male"       # 1   Abdul-Jaleel Al-Khalifa
directors_2013_with_gender$gender[26] <- "female"    # 26  Breege O'Donoghue
directors_2013_with_gender$gender[27] <- "male"      # 27  Breffni Byrne
directors_2013_with_gender$gender[39] <- "female"    # 39  Brid Horan
directors_2013_with_gender$gender[88] <- "male"      # 88  Donard Patrick Thomas Gaynor
directors_2013_with_gender$gender[107] <- "male"     # 107 Frits Beurskens
directors_2013_with_gender$gender[114] <- "male"     # 114 Gearoid O'Dea
directors_2013_with_gender$gender[129] <- "female"   # 129 Heather-Ann Mcsharry
directors_2013_with_gender$gender[141] <- "male"     # 141 Irial Finan
directors_2013_with_gender$gender[297] <- "female"   # 297 Sorca Caitriona Conroy
directors_2013_with_gender$gender[324] <- "male"     # 324 Utz-Hellmuth Felcht
directors_2013_with_gender$gender[325] <- "male"     # 325 Vakha Alvievich Sobraliev

## change gender for index 351 Yuri Ivanovich Radchenko
directors_2013_with_gender$gender[335] <- "male"

# Check again if there are any NA values
na_rows <- directors_2013_with_gender[is.na(directors_2013_with_gender$gender), ]
na_rows
directors_2013_with_gender

# Add gender to directors_2013_df
directors_2013_df["gender"] <- directors_2013_with_gender$gender

#Create dummy variables Revenue and Sector for companies
companies_2013_df["revenue"] <- sample(1000:10000, nrow(companies_2013_df), replace = TRUE)
companies_2013_df["sector"] <- sample(c("Technology", "Entertainment", "Media", "Sports"), nrow(companies_2013_df), replace = TRUE)

# Map indices to actual names
director_map <- setNames(directors$director, directors$director_id)
company_map <- setNames(companies$company, companies$company_id)

bipartite_df_2013 <- expand.grid(
  director_id = unique(edgelist_2013$director),
  company_id = unique(edgelist_2013$board),
  stringsAsFactors = FALSE
)

bipartite_df_2013 <- bipartite_df_2013 %>%
  left_join(directors_2013_df, by = "director_id") %>%
  left_join(companies_2013_df, by = "company_id")

bipartite_df_2013$edge <- 0

for (i in seq_len(nrow(edgelist_2013))) {
  director_name <- director_map[as.character(edgelist_2013$director[i])]
  company_name <- company_map[as.character(edgelist_2013$board[i])]
  bipartite_df_2013$edge[bipartite_df_2013$director == director_name & bipartite_df_2013$company == company_name] <- 1
}

# Reset the index for bipartite_df_2013
bipartite_dataset <- data.frame(
  director_id = bipartite_df_2013$director_id,
  company_id = bipartite_df_2013$company_id,
  director = bipartite_df_2013$director,
  gender = bipartite_df_2013$gender,
  company = bipartite_df_2013$company,
  revenue = bipartite_df_2013$revenue,
  sector = bipartite_df_2013$sector,
  edge = bipartite_df_2013$edge
)

# Turn categorical variables into factors
bipartite_dataset$gender <- as.factor(bipartite_dataset$gender)
bipartite_dataset$sector <- as.factor(bipartite_dataset$sector)
bipartite_dataset$edge <- as.factor(bipartite_dataset$edge)
bipartite_dataset$director <- as.factor(bipartite_dataset$director)
bipartite_dataset$company <- as.factor(bipartite_dataset$company)

# Fit the GAM model
# Here, we assume edge is a binary response variable (0 or 1)
# Using s() for smooth terms and factor() for categorical variables
gam_model <- gam(edge ~ s(director_id, bs = "re") + s(company_id, bs = "re") + gender + sector + s(revenue),
  data = bipartite_dataset,
  family = binomial(link = "logit")  # Assuming a binary response
)

# Summarize the model
summary(gam_model)

# Include multiplicative effects

gam_model <- gam(edge ~ s(director_id, bs = "re") + s(company_id, bs = "re") + gender + sector + revenue +
  te(director_id, company_id, bs = c("re","re")), data = bipartite_dataset, family = binomial(link = "logit"))


summary(gam_model)

