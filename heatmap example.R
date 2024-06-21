# Load the necessary library
library(dplyr)

# Set the seed for reproducibility
set.seed(123)

# Create a data frame
df <- data.frame(
  Column1 = rnorm(100),  # 100 random normal variables
  Column2 = runif(100),  # 100 random uniform variables
  Column3 = rpois(100, lambda = 10),  # 100 random Poisson variables
  Column4 = rbinom(100, size = 10, prob = 0.5),  # 100 random binomial variables
  Column5 = rexp(100, rate = 0.1)  # 100 random exponential variables
)

# Print the first few rows of the data frame
head(df)


library(ggplot2)
library(reshape2)

# Compute the correlation matrix of the data
correlation_matrix <- cor(df)

# Melt the correlation matrix
melted_correlation_matrix <- melt(correlation_matrix)

# Create a heatmap
ggplot(data = melted_correlation_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  coord_fixed()

# Load the necessary libraries
library(gplots)

# Compute the correlation matrix of the data
correlation_matrix <- cor(df)

# Create a heatmap
heatmap.2(correlation_matrix, 
          main = "Heatmap of the Data", 
          trace = "none", 
          margins = c(12, 12))












# Load the necessary libraries
library(ComplexHeatmap)
library(circlize)

# Compute the correlation matrix of the data
correlation_matrix <- cor(df)

# Create a heatmap
Heatmap(correlation_matrix, 
        name = "correlation", 
        col = circlize::colorRamp2(c(-1, 0, 1), c("blue", "white", "red")))


