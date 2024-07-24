library(igraph)

bipartite_dataset <- read.csv("bipartite_dataset.csv")

length(unique(bipartite_dataset$company_id))
length(unique(bipartite_dataset$director_id))

# Get number of entries that have an edge value of 1
sum(bipartite_dataset$edge == 1)

edgelist <- bipartite_dataset[bipartite_dataset$edge == 1, c("director", "company")]

edgelist_df <- data.frame(
  director = edgelist$director,
  company = edgelist$company
)

edgelist_df$director <- as.factor(edgelist_df$director)
edgelist_df$company <- as.factor(edgelist_df$company)

# Create the bipartite graph
net <- graph_from_data_frame(edgelist_df, directed = FALSE)

# Identify bipartite structure and set vertex types
bipartite.mapping(net)
V(net)$type <- bipartite_mapping(net)$type

# Customize node appearance
V(net)$color <- ifelse(V(net)$type, "lightblue", "salmon")
#V(net)$shape <- ifelse(V(net)$type, "circle", "square")
V(net)$shape <- ifelse(V(net)$type, "circle", "circle")
V(net)$size <- 3  # Adjust node size as needed

# Customize label properties
V(net)$label <- NA
V(net)$label.font <- 4
V(net)$label.cex <- 0
V(net)$label.dist <- 0.8 # Distance of labels from nodes

# Set larger plot margins
par(mar = c(1, 1, 1, 1))  # Adjust as needed (bottom, left, top, right)

# Plot the network with bipartite layout
plot(net, layout = layout_as_bipartite(net))
plot(net)

png("bipartite_network.png", width = 800, height = 600)
plot(net)
dev.off()  # Close the PNG device


################################################################
# Plot for subset

# Preselcetd the companies of interest
subset <- bipartite_dataset[bipartite_dataset$company_id %in% c(48, 84, 68, 10, 44, 2), ]
directors_with_edges <- subset$director_id[subset$edge > 0]
length(unique(directors_with_edges)) # 283

# Filter the original dataset to keep only those directors with edges to target companies
subset_reduced <- subset[subset$director_id %in% directors_with_edges, ]

edgelist_subset <- subset_reduced[subset_reduced$edge == 1, c("director_id", "company")]

edgelist_subset_df <- data.frame(
  director_id = edgelist_subset$director_id,
  company_id = edgelist_subset$company
)

edgelist_subset_df$director <- as.factor(edgelist_subset_df$director)
edgelist_subset_df$company_id <- as.factor(edgelist_subset_df$company)


net <- graph_from_data_frame(edgelist_subset_df, directed = FALSE)

# Identify bipartite structure and set vertex types
bipartite.mapping(net)
V(net)$type <- bipartite_mapping(net)$type

# Customize node appearance
V(net)$color <- ifelse(V(net)$type, "lightblue", "salmon")
#V(net)$shape <- ifelse(V(net)$type, "circle", "square")
V(net)$shape <- ifelse(V(net)$type, "circle", "circle")
V(net)$size <- 3  # Adjust node size as needed

# Customize label properties
V(net)$label <- ifelse(V(net)$type, V(net)$name, NA)
V(net)$label.font <- 4
V(net)$label.cex <- 0.8
V(net)$label.dist <- 0.5  # Distance of labels from nodes

# Set larger plot margins
par(mar = c(1, 1, 1, 1))  # Adjust as needed (bottom, left, top, right)

# Plot the network with bipartite layout
plot(net, layout = layout_as_bipartite(net))
plot(net)

