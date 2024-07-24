# Load necessary libraries
library(ggplot2)

# Load the data
data <- read.csv("bipartite_dataset.csv")


data_filtered <- subset(data, edge == 1)

# Get the number of connections for each director
director_connections <- table(data_filtered$director)
director_connections_df <- data.frame(Director = names(director_connections), Connections = as.integer(director_connections))

# Get the number of connections for each company
company_connections <- table(data_filtered$company)
company_connections_df <- data.frame(Company = names(company_connections), Connections = as.integer(company_connections))

hist_directors <- ggplot(director_connections_df, aes(x = as.factor(Connections))) +
  geom_bar(color = "black", fill = "lightblue", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_x_discrete(breaks = seq(1, max(company_connections_df$Connections))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Number of Connections",
       y = "Count") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 16, margin = margin(t = 23)),
        axis.title.y = element_text(size = 16, margin = margin(r = 23)),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("distribution_of_director_connections.png", plot = hist_directors, width = 5, height = 5, dpi=300)


hist_company <- ggplot(company_connections_df, aes(x = as.factor(Connections))) +
  geom_bar(color = "black", fill = "lightblue", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_x_discrete(limits = as.character(1:max(company_connections_df$Connections))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Number of Connections",
       y = "Count") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, margin = margin(r = 23)),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"))


ggsave("distribution_of_company_connections.png", plot = hist_company, width = 8, height = 5, dpi= 300)
