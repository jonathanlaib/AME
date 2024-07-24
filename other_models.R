library("Matrix")
library("lme4")
library("mgcv")
library("gamm4")
library("brms")
library("galamm")

bipartite_dataset <- read.csv("bipartite_dataset.csv")
# Turn categorical variables into factors
length(unique(bipartite_dataset$company_id)) # 283
bipartite_dataset$director_id <- as.factor(bipartite_dataset$director_id)
bipartite_dataset$company_id <- as.factor(bipartite_dataset$company_id)
bipartite_dataset$gender <- as.factor(bipartite_dataset$gender)
bipartite_dataset$sector <- as.factor(bipartite_dataset$sector)
bipartite_dataset$director <- as.factor(bipartite_dataset$director)
bipartite_dataset$company <- as.factor(bipartite_dataset$company)

# Create subset from the bipartite datset that only includes specific companies (to reduce computational complexity)
subset <- bipartite_dataset[bipartite_dataset$company_id %in% c(48, 84, 68, 10, 44, 2), ]

# Find directors with at least one edge to a target company
directors_with_edges <- subset$director_id[subset$edge > 0]
length(unique(directors_with_edges)) # 283

# Filter the original dataset to keep only those directors with edges to target companies
subset_reduced <- subset[subset$director_id %in% directors_with_edges, ]

subset_df <- data.frame(
  director_id = subset_reduced$director_id,
  company_id = subset_reduced$company,
  director = subset_reduced$director,
  company = subset_reduced$company,
  gender = subset_reduced$gender,
  sector = subset_reduced$sector,
  revenue = subset_reduced$revenue,
  edge = subset_reduced$edge
)
subset_df$director_id <- as.factor(subset_df$director_id)
subset_df$company_id <- as.factor(subset_df$company_id)
subset_df$gender <- as.factor(subset_df$gender)
subset_df$sector <- as.factor(subset_df$sector)
subset_df$director <- as.factor(subset_df$director)
subset_df$company <- as.factor(subset_df$company)

# Standardize the numeric predictor
subset_df$revenue <- log(subset_df$revenue + 1)


#Try different models
# SRRM - GAm with random additive effects

model_1 <- gam(edge ~ gender + sector+ revenue + s(director_id, bs="re") + s(company_id, bs="re"), random =~(1 | director:company),
             family=binomial(link="logit"), data=subset_df)


model_2 <- gamm4(edge ~ gender  + revenue + sector +
                 s(director_id, bs = "re") + s(company_id, bs = "re") + t2(director, company, bs=c("re","re")),
                 data = subset_df, family = binomial(link = "logit"))

model_3 <- gamm4(edge ~ gender  + revenue +
                 s(director_id, bs = "re") + s(company_id, bs = "re") , random =~(1 | director:company),
                 data = subset_df, family = binomial(link = "logit"))

model_4 <- gamm4(edge ~ gender  + revenue + sector +
                 s(director_id, bs = "re") + s(company_id, bs = "re"),
                 data = subset_df, family = binomial(link = "logit"))


# Fit the GLMM using glmer
model_5 <- glmer(edge ~ gender + log(revenue) + sector +
                    (1 | director_id) + (1 | company_id) + (1 | director:company),
                    data = subset_df, family = binomial(link = "logit"))

# Summarize the GLMM
summary(model_5)

# Simplify the random effects structure
model_6 <- glmer(edge ~ gender + log(revenue) + sector +
                            (1 | director_id) + (1 | company_id),
                            data = subset_df, family = binomial(link = "logit"))

# Define the model formula
formula <- bf(edge ~ gender + revenue + sector +
              (1 | director_id) + (1 | company_id), sparse = TRUE)

# Fit the model using brms
model_brms <- brm(formula, data = subset_df, family = bernoulli(link = "logit"))

model_galamm <- galamm(formula = edge ~ gender + revenue + sector +  (1 | director_id) + (1 | company_id), family = binomial, data = subset_df)
#model_galamm <- galamm(formula = edge ~ gender + revenue + sector +  (1 | director_id) + (1 | company_id) + t2(director, company, bs = c("re", "re")), family = binomial, data = subset_df)