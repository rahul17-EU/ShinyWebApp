#0 Libraries and Packages
if (!require("tidyverse")) {
    install.packages("tidyverse")
}
# Load necessary libraries
library(ggplot2)
library(gridExtra)

#1 Load the data
data  <- read.csv("G:/My Drive/Studies/Term 2/R/Final Project/DATA/winequality-red.csv")

#2 Data Exploration

str(data)
summary(data)

# Check NAs
map_dbl(.x = data, .f = function (x) {sum(is.na(x))}) # Checking Data missing values

# Plot histograms for all columns
# Get column names
column_names <- colnames(data)

# Create an empty list to store plots
plot_list <- list()

# Loop over each column
for (i in seq_along(column_names)) {
    # Calculate percentiles for each column
    percentiles <- quantile(data[[column_names[i]]], probs = seq(0, 1, 0.01))
    
    # Create box plot for each column's percentiles
    p <- ggplot(data.frame(percentiles), aes(x = "", y = percentiles)) + 
        geom_boxplot(fill = "lightblue", color = "black") +
        labs(title = paste("Box plot of percentiles of", column_names[i]), x = "", y = "Percentiles")
    
    # Add the plot to the list
    plot_list[[i]] <- p
}

# Plot all box plots on a shared plot
do.call(grid.arrange, plot_list)


# We conclude that the data is clean, that there are no missing values, and that the data is ready for analysis.





