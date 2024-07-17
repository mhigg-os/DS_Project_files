---
title: "Predicting Treatment Completion for Substance Abuse"
author: "Shady & Mohammad"
date: "2024-07-02"
output:
  pdf_document: default
  html_document: default
---

#setup chunk  
```{r }
knitr::opts_chunk$set(echo = TRUE)
```





#Loading Data and Libraries:
```{r}
# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(schrute)
library(lubridate)
library(dplyr)
library(caret)
library(caTools)
library(ROCR)
library(reshape2)
library(corrplot)
library(broom)
```


# Load data
```{r}

# df for the 2015-2019
load("~/שנה ב/סמסטר ד/מבוא לניתוח נתונים(ננ)/project(ננ)/tedsa_puf_2015_2019.RData")

```





#Glimpsing the Data:
```{r}

# Glimpse the datasets:
glimpse(df)

```



# data cleaning:

## columns names change:
```{r}

df <- df %>%
  janitor::clean_names()

```

## replaceing -9 with nulls:
```{r}

library(dplyr)
library(haven)

df <- df %>% mutate_all(~replace(., . == -9, NA))

# here we replace the -9 value with a null value, and that's because the -9 represent a null value based on the documentation of the dataset. so we replace now to ease on our self to handle the null's during the project.

```


## filling nulls:
```{r}


df <- df %>%
  mutate(
    gender  = ifelse(is.na(gender) , as.factor('Unknown'), gender) ,
    race    = ifelse(is.na(race)   , as.factor('Unknown'), race)   ,
    ethnic  = ifelse(is.na(ethnic) , as.factor('Unknown'), ethnic) ,
    employ  = ifelse(is.na(employ) , as.factor('Unknown'), employ) ,
    hlthins = ifelse(is.na(hlthins), as.factor('Unknown'), hlthins),
    psyprob = ifelse(is.na(psyprob), as.factor('Unknown'), psyprob),
    freq1   = ifelse(is.na(freq1)  , 0, freq1)                     ,
    freq2   = ifelse(is.na(freq2)  , 0, freq2)                     ,
    freq3   = ifelse(is.na(freq3)  , 0, freq3)
  )

```


## Remove duplicate rows
```{r}

df <- df %>% distinct()


```



# data formating:
```{r}

df <- df %>%
  mutate(
    gender = recode(gender, `1` = 'Male', `2` = 'Female'),
    employ = recode(employ, `1` = 'Employed', `2` = 'Unemployed', `3` = 'Not in Labor Force')
  )
```



# visualization:

```{r}
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(colour = "lightgray", linetype = "dashed"),
    panel.background = element_rect(fill = "white")
  )
```


## Plot 1: Gender Distribution
```{r}

ggplot(df, aes(x = gender, fill = gender)) +
  geom_bar(color = "black") +
  labs(title = "Gender Distribution in 2015-2019", 
       x = "Gender", 
       y = "Count") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  my_theme
```



## Plot 2: Age Distribution
```{r}

ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Age Distribution in 2015-2019", 
       x = "Age", 
       y = "Count") +
  my_theme

```


## Plot 3: Ethnicity Distribution
```{r}

ggplot(df, aes(x = ethnic, fill = ethnic)) +
  geom_bar(color = "black") +
  labs(title = "Ethnicity Distribution in 2015-2019",
       x = "Ethnicity",
       y = "Count") +
  my_theme

```


## Plot 4: Employment Status Distribution
```{r}

ggplot(df, aes(x = employ, fill = employ)) +
  geom_bar(color = "black") +
  labs(title = "Employment Status Distribution in 2015-2019",
       x = "Employment Status",
       y = "Count") +
  my_theme

```


## Plot 5: Substance 1 (Sub1) Distribution
```{r}

library(ggplot2)
library(dplyr)

# Assuming df is your dataset
# Calculate percentages
df_percent <- df %>%
  group_by(sub1) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) / 100)

# Plot with percentage on y-axis
ggplot(df_percent, aes(x = factor(sub1), y = percentage, fill = factor(sub1))) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Substance 1 Distribution in 2015-2019",
       x = "Substance 1",
       y = "Percentage") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis labels to percentage
  theme_minimal()  # Use a minimal theme or replace with your custom theme (my_theme)


```


## Plot 6: Substance 2 (Sub2) Distribution
```{r}

# Plot Substance 2 Distribution
ggplot(df, aes(x = factor(sub2), fill = factor(sub2))) +
  geom_bar(color = "black") +
  labs(title = "Substance 2 Distribution in 2015-2019",
       x = "Substance 2",
       y = "Count") +
  scale_x_discrete(drop = FALSE) +  # Ensure all factor levels are shown
  my_theme


```


## Plot 7: Substance 3 (Sub3) Distribution
```{r}


# Plot Substance 3 Distribution
ggplot(df, aes(x = factor(sub3), fill = factor(sub3))) +
  geom_bar(color = "black") +
  labs(title = "Substance 3 Distribution in 2015-2019",
       x = "Substance 3",
       y = "Count") +
  scale_x_discrete(drop = FALSE) +  # Ensure all factor levels are shown
  my_theme

```





## Plot 8: Education Level Distribution
```{r}

ggplot(df, aes(x = educ, fill = educ)) +
  geom_bar(color = "black") +
  labs(title = "Education Level Distribution in 2015-2019",
       x = "Education Level",
       y = "Count") +
  my_theme

```

## Plot 9: Health Insurance Status
```{r}

library(ggplot2)
library(dplyr)

# Assuming df is your dataset
df_percents <- df %>%
  group_by(hlthins) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) / 100)

# Plotting with percentage on y-axis and labels on bars
ggplot(df_percents, aes(x = factor(hlthins), y = percentage)) +
  geom_bar(stat = "identity", fill = "salmon", color = "black") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  labs(x = "Health Insurance Status", y = "Percentage (%)", 
       title = "Distribution of Health Insurance Status (hlthins) in 2015-2019") +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis labels to percentage
  theme_minimal()  # Use a minimal theme or replace with your custom theme (my_theme)



```



# Create Additional Features:

## age group column:
```{r}

df <- df %>%
  mutate(
    age_group = case_when(
      age <= 18 ~ 'Under 18',
      age > 18 & age <= 25 ~ '19-25',
      age > 25 & age <= 35 ~ '26-35',
      age > 35 ~ '36+'
    )
  )
```


## Completion Status new column:
```{r}

# Function to create completion status based on subsequent years
create_completion_status <- function(df, years) {
  # Initialize an empty vector to store completion status
  completion_status <- rep(0, nrow(df))
  
  # Loop through each year in the list of years
  for (year in years) {
    # Check if caseid is present in subsequent year data
    completion_status <- completion_status | (df$caseid %in% df$caseid[df$admyr == year + 1])
  }
  
  # Return the completion status as 0 or 1
  return(completion_status)
}

# Assuming df is your dataset
# List of subsequent years to check
years_to_check <- c(2015, 2016, 2017, 2018, 2019, 2020)

# Create completion_status column for df dataset
df <- df %>%
  mutate(completion_status = create_completion_status(., years_to_check))

# Count and print completion status for df
cat("\nCompletion Status Counts in df:\n")
completion_status_counts <- table(df$completion_status)
print(completion_status_counts)


```


# the model:

## spliting the data:
```{r}


# Load necessary libraries
library(tidyverse)
library(caret)

# Ensure df is correctly defined and not masked by any other object
print(class(df))

# Splitting the Data
set.seed(123)
# Split the data into training and testing sets
train_index <- createDataPartition(df$completion_status, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Check the distribution in the training and test sets
train_distribution <- table(train_data$completion_status)
test_distribution <- table(test_data$completion_status)

# Print the distribution in the training and test sets
cat("Training dataset distribution:\n")
print(train_distribution)

cat("Test dataset distribution:\n")
print(test_distribution)


# Number of rows in the training set
train_rows <- nrow(train_data)

# Number of rows in the testing set
test_rows <- nrow(test_data)

# Print the number of rows
cat("Number of rows in the training set:", train_rows, "\n")
cat("Number of rows in the testing set:", test_rows, "\n")

```


## running the model:
```{r}

# Define the formula for the logistic regression model
formula <- completion_status ~ sub1 + age + gender + ethnic + race + employ + educ + hlthins + psyprob + freq1 + freq2 + freq3

# Fit the logistic regression model
model <- glm(formula, data = train_data, family = binomial)

# Summarize the model
summary(model)


```


## model Visualizing Logistic Regression Coefficients:
```{r}

# Extract coefficients and their names
coef_data <- coef(summary(model))
coef_data <- as.data.frame(coef_data)

# Remove the intercept row
coef_data <- coef_data[-1, ]

# Order coefficients by absolute value for better visualization
coef_data <- coef_data[order(abs(coef_data[, "Estimate"]), decreasing = TRUE), ]

# Plot coefficients
library(ggplot2)
ggplot(coef_data, aes(x = reorder(row.names(coef_data), Estimate), y = Estimate, fill = row.names(coef_data))) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  labs(x = "Predictors", y = "Coefficient Estimate", title = "Logistic Regression Coefficients") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  geom_text(aes(label = sprintf("%.2f", Estimate), y = Estimate), hjust = -0.3, size = 3, color = "black")


```


# model evaluation:
Evaluating the model using accuracy, sensitivity, specificity .
```{r}

# Ensure predicted_classes and test_data$completion_status are factors with levels 0 and 1
predicted_classes <- factor(ifelse(predictions > 0.5, 1, 0), levels = c(0, 1))
test_data$completion_status <- factor(test_data$completion_status, levels = c(0, 1))

# Compute confusion matrix
conf_matrix <- confusionMatrix(predicted_classes, test_data$completion_status)

# Extract evaluation metrics
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']


# Print evaluation metrics
cat("\nModel Evaluation Metrics:\n")
cat(paste("Accuracy    : ", round(accuracy, 3), "\n", sep = ""))
cat(paste("Sensitivity : ", round(sensitivity, 3), "\n", sep = ""))
cat(paste("Specificity : ", round(specificity, 3), "\n", sep = ""))




```

##  Plotting Coefficients with Positive/Negative Highlight:
This part enhances the previous coefficient plot by visually distinguishing between positive and negative coefficients.
```{r}

# Extract coefficients and their names
coef_data <- coef(summary(model))
coef_data <- as.data.frame(coef_data)

# Remove the intercept row
coef_data <- coef_data[-1, ]

# Order coefficients by absolute value for better visualization
coef_data <- coef_data[order(abs(coef_data[, "Estimate"]), decreasing = TRUE), ]

# Determine positive and negative coefficients
coef_data$sign <- ifelse(coef_data$Estimate >= 0, "Positive", "Negative")

# Plot coefficients with positive/negative highlight
ggplot(coef_data, aes(x = reorder(row.names(coef_data), Estimate), y = Estimate, fill = sign)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  labs(x = "Predictors", y = "Coefficient Estimate", title = "Logistic Regression Coefficients with Positive/Negative Highlight") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  geom_text(aes(label = sprintf("%.2f", Estimate), y = Estimate), hjust = -0.3, size = 3, color = "black") +
  scale_fill_manual(values = c("Positive" = "skyblue", "Negative" = "salmon"))


```


## ROC Curve Plot:
This part generates an ROC curve for the logistic regression model to evaluate its performance and calculates the Area Under the Curve (AUC).
```{r}

# Load necessary library
library(pROC)

# Assuming 'model' is your fitted logistic regression model
# Predict probabilities for the test set
predictions <- predict(model, newdata = test_data, type = "response")

# Generate ROC curve
roc_curve <- roc(test_data$completion_status, predictions)

# Calculate AUC
roc_auc <- auc(roc_curve)

# Print AUC
cat("AUC:", round(roc_auc, 3), "\n")

# Plot ROC curve
plot(roc_curve, col = "steelblue", main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(roc_auc, 3)), col = "steelblue", lwd = 2)


```








