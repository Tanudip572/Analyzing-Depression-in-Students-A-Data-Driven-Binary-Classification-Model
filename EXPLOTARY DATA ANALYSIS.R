data=read.csv("C:\\Users\\TANUDIP GHOSH\\OneDrive\\Desktop\\Student Depression Dataset 2.csv")
length(unique(data$Gender))
library(dplyr)
data%>%count(Gender)
data%>%count(Age)
data%>%count(Academic.Pressure)
data%>%count(CGPA)
data%>%count(Depression)
data%>%count(Work.Study.Hours)
data%>%count(Financial.Stress)
data%>%count(Sleep.Duration)
data=na.omit(data)
# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(ggthemes)
library(reshape2)
library(plotly)


##############

# Load necessary libraries
library(ggplot2)
library(ggridges)
library(ggcorrplot)
library(viridis)
library(patchwork)
library(tidyverse)
# Load libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Read data
data <- read.csv("C:\\Users\\TANUDIP GHOSH\\OneDrive\\Desktop\\Student Depression Dataset 2.csv")
data=na.omit(data)
##########################################################################################
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(ggthemes)
library(reshape2)

# Convert relevant columns to factorsdata
data$Gender <- as.factor(data$Gender) 
data$Have.you.ever.had.suicidal.thoughts.. <- as.factor(data$Have.you.ever.had.suicidal.thoughts..)
data$Family.History.of.Mental.Illness <- as.factor(data$Family.History.of.Mental.Illness)
data$Sleep.Duration <- factor(data$Sleep.Duration, levels = c("Less than 5 hours", "5-6 hours", "7-8 hours", "More than 8 hours"))
data$Depression <- as.factor(data$Depression)

# 1. Bar plot: Depression count by Gender
ggplot(data, aes(x = Gender, fill = Depression)) +
  geom_bar(position = "dodge") +
  labs(title = "Depression Count by Gender", y = "Count") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(readr)


# Suicidal thoughts vs Depression (X factor plot)
ggplot(data, aes(x =Have.you.ever.had.suicidal.thoughts.., fill = Depression)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Suicidal Thoughts vs Depression (Proportion)",
       x = "Suicidal Thoughts", y = "Percentage") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 14)



# Read the data
depression_data <- read.csv("C:\\Users\\TANUDIP GHOSH\\OneDrive\\Desktop\\Depression.csv")
depression_data=na.omit(depression_data)
# Convert Depression to a factor for better visualization
depression_data$Depression <- factor(depression_data$Depression, 
                                     levels = c(0, 1),
                                     labels = c("No Depression", "Depression"))

# 1. Correlation Heatmap with Depression Highlight
numeric_vars <- depression_data %>% 
  select(where(is.numeric), -Age) %>% 
  select(-matches("^Have you ever had suicidal thoughts")) # Exclude non-numeric binary

cor_matrix <- cor(numeric_vars, use = "complete.obs")

ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlation Between Numeric Factors") +
  theme(plot.title = element_text(hjust = 0.5))
library(ggridges)
# 2. Ridgeline Plot of CGPA by Depression Status
ggplot(depression_data, aes(x = CGPA, y = Depression, fill = Depression)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  labs(title = "Distribution of CGPA by Depression Status",
       x = "CGPA", y = "") +
  theme_minimal() +
  theme(legend.position = "none")



# 4. Stacked Bar Plot of Dietary Habits by Depression
diet_data <- depression_data %>%
  count(Dietary.Habits, Depression) %>%
  group_by(Dietary.Habits) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(diet_data, aes(x = Dietary.Habits, y = percentage, fill = Depression)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  labs(title = "Dietary Habits Distribution by Depression Status",
       x = "Dietary Habits", 
       y = "Percentage") +
  theme_minimal() +
  coord_flip()

# 7. Faceted Histogram of Age by Depression Status
ggplot(depression_data, aes(x = Age, fill = Depression)) +
  geom_histogram(binwidth = 2, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  labs(title = "Age Distribution by Depression Status",
       x = "Age", 
       y = "Count") +
  theme_minimal() +
  facet_wrap(~Depression, ncol = 1)




  # Load required libraries
  library(ggplot2)
  library(ggridges)
  library(readr)
  
  # Load your data
  data=read.csv("C:\\Users\\TANUDIP GHOSH\\OneDrive\\Desktop\\Student Depression Dataset 2.csv")
  # Clean column names
  colnames(data) <- make.names(colnames(data))
  
  # Convert Depression to factor
  data$Depression <- factor(data$Depression, levels = c("No Depression", "Depression"))
  library(ggplot2)
  library(readr)
  
  # Load and clean data
  data=read.csv("C:\\Users\\TANUDIP GHOSH\\OneDrive\\Desktop\\Student Depression Dataset 2.csv")
  data=na.omit(data)
  library(ggplot2)
  library(readr)
  
  # Load and clean data
  # Convert Depression to factor (if not already)
  data$Depression <- as.factor(data$Depression)
  levels(data$Depression) = c("No Depression", "Depression")
  # Strip plot
  ggplot(data, aes(x = Depression, y = Academic.Pressure, color = Depression)) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    theme_minimal() +
    labs(
      title = "Academic Pressure by Depression Status (Strip Plot)",
      x = "Depression Status",
      y = "Academic Pressure"
    ) +
    theme(legend.position = "none")
  
  
  library(ggplot2)
  library(readr)
  library(dplyr)
  
  # Convert variables
  data$Study.Satisfaction <- as.factor(data$Study.Satisfaction)  # assuming it's categorical
  
  # Create plot data: proportion table
  plot_data <- data %>%
    group_by(Depression, Study.Satisfaction) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(Depression) %>%
    mutate(prop = count / sum(count))
  
  # Stacked proportional bar chart
  ggplot(plot_data, aes(x = Depression, y = prop, fill = Study.Satisfaction)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Study Satisfaction Distribution by Depression Status",
      x = "Depression Status",
      y = "Proportion",
      fill = "Study Satisfaction Level"
    ) +
    theme_minimal()
  
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(gridExtra)  # for arranging plots
  

  data$Sleep.Duration <- as.factor(data$Sleep.Duration)
  
  # Split data by Depression group
  data_no <- data %>% filter(Depression == "No Depression")
  data_yes <- data %>% filter(Depression == "Depression")
  
  # Create pie chart function
  make_pie <- function(data, group_label) {
    plot_data <- data %>%
      count(Sleep.Duration) %>%
      mutate(perc = round(n / sum(n) * 100, 1),
             label = paste0(Sleep.Duration, "\n", perc, "%"))
    
    ggplot(plot_data, aes(x = "", y = n, fill = Sleep.Duration)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = paste("Sleep Duration -", group_label)) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
      theme(legend.position = "none")
  }
  
  # Generate both plots
  pie_no <- make_pie(data_no, "No Depression")
  pie_yes <- make_pie(data_yes, "Depression")
  
  # Display side by side
  grid.arrange(pie_no, pie_yes, ncol = 2)
  library(ggplot2)
  library(readr)
  library(dplyr)
  # Ensure relevant columns are factors
  data$Degree <- as.factor(data$Degree)
  
  # Calculate depression percentages per degree
  plot_data <- data %>%
    group_by(Degree) %>%
    summarise(
      Total = n(),
      Depressed = sum(Depression == "Depression", na.rm = TRUE)
    ) %>%
    mutate(DepressionPercent = round((Depressed / Total) * 100, 1))
  
  # Bar plot
  ggplot(plot_data, aes(x = Degree, y = DepressionPercent, fill = Degree)) +
    geom_col(width = 0.6, alpha = 0.8) +
    geom_text(aes(label = paste0(DepressionPercent, "%")), vjust = -0.5, size = 3.5) +
    labs(
      title = "Percentage of Students with Depression by Degree",
      x = "Degree",
      y = "Depression Percentage"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  library(ggplot2)
  library(readr)
  
  # Violin Plot
  ggplot(data, aes(x = Depression, y = Work.Study.Hours, fill = Depression)) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    labs(
      title = "Study Hours by Depression Status",
      x = "Depression Status",
      y = "Study Hours"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  library(ggplot2)
  library(readr)
  library(dplyr)
  # Ensure factors
  data$Financial.Stress <- factor(data$Financial.Stress, ordered = TRUE)
  
  # Count and percent by group
  plot_data <- data %>%
    group_by(Financial.Stress, Depression) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(Financial.Stress) %>%
    mutate(perc = 100 * n / sum(n),
           perc = ifelse(Depression == "Depression", -perc, perc))  # invert Depression values
  
  # Plot
  ggplot(plot_data, aes(x = Financial.Stress, y = perc, fill = Depression)) +
    geom_col(width = 0.6) +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(
      title = "Diverging Bar Chart: Financial Stress by Depression Status",
      x = "Financial Stress Level",
      y = "Percentage",
      fill = "Depression Status"
    ) +
    theme_minimal()
  
  library(ggplot2)
  library(readr)
  library(dplyr)
  
  # Load data
  colnames(data) <- make.names(colnames(data))
  
  # Ensure categorical
  data$Depression <- as.factor(data$Depression)
  data$Family.History.of.Mental.Illness <- as.factor(data$Family.History.of.Mental.Illness)
  
  # Calculate percentages
  slope_data <- data %>%
    group_by(Family.History.of.Mental.Illness, Depression) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Family.History.of.Mental.Illness) %>%
    mutate(perc = 100 * n / sum(n)) %>%
    ungroup()
  
  # Create slope chart
  ggplot(slope_data, aes(x = Family.History.of.Mental.Illness, y = perc, group = Depression, color = Depression)) +
    geom_line(size = 1.5) +
    geom_point(size = 4) +
    geom_text(aes(label = paste0(round(perc, 1), "%")), vjust = -0.8, size = 3.5) +
    labs(
      title = "Change in Depression Rate by Family Mental Illness History",
      x = "Family History of Mental Illness",
      y = "Depression Percentage",
      color = "Depression Status"
    ) +
    theme_minimal()
  
  