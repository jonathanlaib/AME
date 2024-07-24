## Dataset Setup
The dataset is prepared in `set_up_bipartite_dataset.R`. It modifies the Irish Directorates Dataset by incorporating the covariates: gender, sector, and company.

## Bipartite Network Analysis
The main analysis of the bipartite network is performed in `srrm_bipartite.R`. In this script, we fit a Generalized Additive Model (GAM) to our dataset to represent the properties of an SRRM.

## One-Mode Network Analysis

We also transformed our dataset into a one-mode dataset to fit an AME in `one_mode_ame.R`. 

## Model Results

Results from both models (GAM and AME) can be found in their respective R scripts and are also exported to text files for easier access and review.

## Network Plots
The plots of the network of the datasets are created in the `plot.R` file. These plots provide a visual representation of the dataset, showing the connections between directors and companies.

## Degree Distribution Plots
The plots for the degree distributions for both directors and companies are done in `plot_histogram.R`.

## File Summary

- `set_upbipartite_dataset.R`: Script for setting up and modifying the dataset
- `srrm_bipartite.R`: Script for fitting a GAM to the bipartite network
- `one_mode_ame.R`: Script for transforming the dataset to one-mode and fitting an AME
- `plot.R`: Script for generating netwoek plots of the dataset
- `plot_histogram.R`: Script for plotting the degree distributions of directors and companies
