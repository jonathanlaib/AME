library("mgcv")
library("dplyr")

bipartite_dataset <- read.csv("bipartite_dataset.csv")

bipartite_dataset$director_id <- as.factor(bipartite_dataset$director_id)
bipartite_dataset$company_id <- as.factor(bipartite_dataset$company_id)
bipartite_dataset$gender <- as.factor(bipartite_dataset$gender)
bipartite_dataset$sector <- as.factor(bipartite_dataset$sector)

# Delete director and company columns as it is redundant
bipartite_dataset <- bipartite_dataset %>% select(-director, -company)

# SRRM - with random additive effects

srrm <- gam(edge ~ gender + revenue + sector + s(director_id, bs="re") + s(company_id, bs="re"),
             family=binomial(link="logit"), data=bipartite_dataset)


sink("outputfile_SRRM.txt")

summary(srrm)

sink()
