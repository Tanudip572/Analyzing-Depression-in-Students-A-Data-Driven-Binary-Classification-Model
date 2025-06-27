library(tidyverse)
library(broom)
library(ggplot2)
library(car)
data= read.csv("D:\\SEM 6 PROJECT\\Student Depression Dataset.csv")
data=na.omit(data)

data=data%>%select(-id,-Profession,-Work.Pressure,-Job.Satisfaction,-City,-Gender,-Family.History.of.Mental.Illness,-Sleep.Duration,-Dietary.Habits,-Have.you.ever.had.suicidal.thoughts..,-Degree)
model=glm(Depression~.,data=data,family = "binomial")
vif(model) #no multi-colinearity

model <- lm(Depression ~ ., data =data)
summary(model)

count(data)
repeat {
  # Calculate Cook's distance
  cooks_dist <- cooks.distance(model)
  
  # Set the threshold
  threshold <- 4 / length(cooks_dist)
  
  # Find influential points
  influential_points <- which(cooks_dist > threshold)
  
  # Print influential points
  print(influential_points)
  
  # Plot Cook's distance
  plot(cooks_dist, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index")
  abline(h = threshold, col = "red", lty = 2)
  
  # Check if there are any influential points
  if (length(influential_points) == 0) {
    cat("No more influential points. Exiting loop.\n")
    break
  }
  
  # Remove influential points
  data <- data[-influential_points, ]
  
  # Print updated number of rows
  print(paste("Remaining observations:", nrow(data)))
  
  # Refit the model
  model <- lm(Depression ~ ., data = data)
}
count(data)


data$Depression=as.factor(data$Depression)
levels(data$Depression)=c("No","Yes")
# Add 1 to all numeric columns
data <- data %>% 
  mutate(across(where(is.numeric), ~ . + 1))
model <- glm(Depression ~., data = data, 
             family = binomial)
summary(model)
data$Depression <- ifelse(data$Depression == "Yes", 1, 0)
boxTidwell(Depression~.,data = data)

#
library(splines)
library(caTools)
set.seed(123)


split=sample.split(data,SplitRatio = 0.8)
train_data=subset(data,split == TRUE)
test_data=subset(data,split==FALSE)
# Fit logistic regression with spline for continuous predictors
spline_model <- glm(
  Depression ~ ns(Age, df = 3) + ns(Academic.Pressure, df = 3) +ns(Financial.Stress, df = 3)+CGPA+Study.Satisfaction+Work.Study.Hours ,
  data = train_data,
  family = binomial
)
options(scipen = 999)
summary(spline_model)
pscl::pR2(spline_model)["McFadden"]
caret::varImp(spline_model)
vif(spline_model)
# Visualize the spline effect
termplot(spline_model, partial.resid = TRUE, smooth = panel.smooth, se = TRUE)
pred_probs <- predict(spline_model, newdata = test_data, type = "response")
# 2. Convert probabilities to class predictions (default threshold = 0.5)
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)
# 3. Ensure both actual and predicted are factors with matching levels
head(test_data$Depression)
head(pred_classes)
test_data$Depression <- factor(test_data$Depression, levels = c(0, 1), labels = c("No", "Yes"))
pred_classes <- unname(pred_classes)  # Remove row number labels
pred_classes <- factor(pred_classes, levels = c(0, 1), labels = c("No", "Yes"))
# Now generate the confusion matrix
library(caret)
conf_matrix <- confusionMatrix(pred_classes, test_data$Depression)
print(conf_matrix)

# Install pROC package if not already installed
# install.packages("pROC")

library(pROC)

# Convert actual labels to numeric (1 = Yes, 0 = No)
actual <- ifelse(test_data$Depression == "Yes", 1, 0)

# Compute ROC
roc_obj <- roc(actual, pred_probs)
roc_obj
# Plot ROC curve
plot(roc_obj, col = "blue", lwd = 2,
     main = "ROC Curve for Depression Prediction")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal line for reference
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 3)), col = "blue", lwd = 2)


###########################
library(ggplot2)

# Create a data frame from the varImp output
importance_data <- data.frame(
  Variable = c("ns(Age, df = 3)1", "ns(Age, df = 3)2", "ns(Age, df = 3)3",
               "ns(Academic.Pressure, df = 3)1", "ns(Academic.Pressure, df = 3)2", "ns(Academic.Pressure, df = 3)3",
               "ns(Financial.Stress, df = 3)1", "ns(Financial.Stress, df = 3)2", "ns(Financial.Stress, df = 3)3",
               "CGPA", "Study.Satisfaction", "Work.Study.Hours"),
  Importance = c(5.7072282, 9.0647539, 7.0550125,
                 0.2612792, 0.1054245, 0.1055608,
                 19.3144287, 29.4087875, 34.7333456,
                 4.7530864, 18.9077898, 24.9606531)
)


# Extract base variable names
importance_data$BaseVar <- gsub("ns\\((.*?), df = 3\\).*", "\\1", importance_data$Variable)
importance_data$BaseVar <- ifelse(importance_data$BaseVar == importance_data$Variable,
                                  importance_data$Variable,
                                  importance_data$BaseVar)

# Calculate total importance for each base variable
total_importance <- aggregate(Importance ~ BaseVar, data = importance_data, sum)

# Order base variables by total importance
total_importance <- total_importance[order(total_importance$Importance), ]
importance_data$BaseVar <- factor(importance_data$BaseVar,
                                  levels = total_importance$BaseVar)

# Plot with facets for spline terms
ggplot(importance_data, aes(x = Importance, y = BaseVar)) +
  geom_col(fill = "steelblue") +
  labs(title = "Variable Importance from Spline Model",
       x = "Relative Importance",
       y = "Variable") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
library(ggplot2)

# Prepare the VIF data
vif_data <- data.frame(
  Variable = c("ns(Age, df = 3)", "ns(Academic.Pressure, df = 3)", 
               "ns(Financial.Stress, df = 3)", "CGPA", 
               "Study.Satisfaction", "Work.Study.Hours"),
  GVIF = c(1.194207, 1.591951, 1.577319, 1.011726, 1.059813, 1.136757),
  GVIF_adj = c(1.030022, 1.080575, 1.078913, 1.005846, 1.029472, 1.066188)
)

# Plot with reference line at VIF = 5
ggplot(vif_data, aes(x = reorder(Variable, GVIF), y = GVIF)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed", linewidth = 1) +  # Threshold line
  geom_text(aes(label = round(GVIF, 2)), 
            hjust = -0.2, color = "black", size = 3.5) +  # Add VIF values
  labs(
    title = "VIF Values (GVIF)",
    subtitle = "Red dashed line indicates VIF = 5 (common multicollinearity threshold)",
    x = "Predictor Variables",
    y = "VIF"
  ) +
  coord_flip() +  # Horizontal bars
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40")
  ) +
  ylim(0, max(vif_data$GVIF) * 1.1)  # Add padding to y-axis

library(ggplot2)

# 1. Get the model's training data (automatically excludes NAs)
model_data <- model.frame(spline_model)  # Ensure rows align
outcome_name <- names(attr(terms(spline_model), "dataClasses"))[1]  # Extract outcome name

# 2. Get predicted probabilities
predicted_probs <- predict(spline_model, type = "response")

# 3. Create plotting dataframe
plot_data <- data.frame(
  Actual = model_data[[outcome_name]],
  Predicted = predicted_probs
)

# Ensure Actual is a factor with "Yes"/"No" labels if it is 0/1
plot_data$Actual <- factor(plot_data$Actual, levels = c(0, 1), labels = c("No", "Yes"))

# 4. Jitter plot with colored outcome
ggplot(plot_data, aes(x = Predicted, y = jitter(as.numeric(Actual == "Yes"), factor = 0.3), color = Actual)) +
  geom_jitter(alpha = 0.6, width = 0, height = 0.1, size = 2) +
  geom_smooth(method = "loess", color = "black", se = FALSE) +
  labs(
    title = "Actual vs. Predicted Probabilities",
    x = "Predicted Probability", 
    y = "Actual Outcome (jittered)",
    color = "Outcome"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  scale_color_manual(values = c("No" = "tomato", "Yes" = "steelblue"))



library(caret)
library(ggplot2)
library(dplyr)

# 1. Compute confusion matrix
conf_matrix <- confusionMatrix(pred_classes, test_data$Depression)

# 2. Extract table and convert to dataframe
cm_table <- as.data.frame(conf_matrix$table)
colnames(cm_table) <- c("Predicted", "Actual", "Freq")

# 3. Assign TP, TN, FP, FN labels
cm_table <- cm_table %>%
  mutate(Label = case_when(
    Predicted == "Yes" & Actual == "Yes" ~ "True Positive",
    Predicted == "No" & Actual == "No" ~ "True Negative",
    Predicted == "Yes" & Actual == "No" ~ "False Positive",
    Predicted == "No" & Actual == "Yes" ~ "False Negative"
  ))

# 4. Plot confusion matrix with labels
ggplot(cm_table, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(Label, "\n", Freq)), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "orange", high = "lightgreen") +
  labs(
    title = "Confusion Matrix with TP/TN/FP/FN Labels",
    x = "Actual Outcome",
    y = "Predicted Outcome"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "none"
  )







