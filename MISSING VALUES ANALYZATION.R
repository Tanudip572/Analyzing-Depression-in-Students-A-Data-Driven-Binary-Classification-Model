data=read.csv("C:\\Users\\TANUDIP GHOSH\\OneDrive\\Desktop\\Depression.csv")
# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggpubr)
library(readr)
str(data)
summary(data)
library(DataExplorer)
plot_missing(data)
dim(data)
colSums(is.na(data))
data=na.omit(data)
colSums(is.na(data))
dim(data)
plot_missing(data)
# Convert relevant columns to factors if necessary
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Correlation matrix (numeric variables only)
corr_matrix <- cor(data[, num_cols], use="complete.obs")
corrplot(corr_matrix, method="number", type="upper")





