#rm(list= ls()) # clear the r environment - use cautiously for clean work
# Load dataset
dataset1 <- read.table(file.choose(), sep= ",", header= TRUE,
                       stringsAsFactors= FALSE)

# Load required libraries
library(dplyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(corrplot)
library(tidyr)
library(psych)
library(caTools)
library(caret)
library(pROC)


# Data structure overview
str(dataset1)

# Check for missing values
sum(is.na(dataset1))

# Task 1: Descriptive statistics ----
# Select numerical variables and compute descriptive statistics
desc_stats <- dataset1 %>%
  select_if(is.numeric) %>%
  psych::describe() %>%
  select(n, mean, sd, median, min, max, range, skew, kurtosis)

desc_stats %>%
  knitr::kable(
    align = "c",
    caption = "Descriptive Statistics for Numerical Variables",
    digits = 2
  ) %>%
  kableExtra::kable_classic_2(
    html_font = "Calibri",
    position = "center",
    font_size = 12
  ) %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#E8F4F8")

# Response variable analysis
revenue_dist <- dataset1 %>%
  count(Revenue) %>%
  mutate(Percentage = n/sum(n) * 100)

revenue_dist %>%
  knitr::kable(
    caption = "Distribution of Purchase Conversion (Revenue)",
    digits = 2,
    col.names = c("Revenue (Purchase Made)", "Count", "Percentage (%)")
  ) %>%
  kableExtra::kable_classic_2(
    html_font = "Calibri",
    position = "center",
    font_size = 13,
    full_width = FALSE
  ) %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#E8F4F8")

# Visualise Revenue distribution
ggplot(dataset1, aes(x = Revenue, fill = Revenue)) +
  geom_bar(width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  labs(
    title = "Distribution of Purchase Conversion",
    x = "Revenue Status",
    y = "Number of Sessions"
  ) +
  scale_fill_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Categorical variables frequency tables
# Function to create frequency table
create_freq_table <- function(data, var_name) {
  data %>%
    count(!!sym(var_name)) %>%
    mutate(Percentage = n/sum(n) * 100) %>%
    arrange(desc(n))
}

# VisitorType frequency table
visitor_freq <- create_freq_table(dataset1, "VisitorType")

visitor_freq %>%
  knitr::kable(
    caption = "Distribution of Visitor Type",
    digits = 2,
    col.names = c("Visitor Type", "Count", "Percentage (%)")
  ) %>%
  kableExtra::kable_classic_2(
    html_font = "Calibri",
    position = "center",
    font_size = 12,
    full_width = FALSE
  )%>%
  kableExtra::row_spec(0, bold = TRUE, background = "#E8F4F8")


# Visualise key categorical variables 
# VisitorType Distribution
ggplot(dataset1, aes(x = VisitorType, fill = VisitorType)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(
    title = "Distribution of Visitor Types",
    x = "Visitor Type",
    y = "Number of Sessions"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


# Create boxplots for key behavioral variables 
key_vars <- c("Administrative_Duration", "Informational_Duration", 
              "ProductRelated_Duration", "BounceRates", "ExitRates", "PageValues")

dataset1 %>%
  select(Revenue, all_of(key_vars)) %>%
  pivot_longer(-Revenue, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Revenue, y = Value, fill = Revenue)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  labs(
    title = "Distribution of Key Behavioral Variables by Revenue Status",
    subtitle = "Comparing converting vs non-converting sessions",
    x = "Revenue (Purchase Made)",
    y = "Value"
  ) +
  scale_fill_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60")) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "#E8F4F8"),
    strip.text = element_text(face = "bold")
  )

# Task 2: Train-test split ----
# Initialise randomise
set.seed(25)  # for reproducibility

# Create new column in dataset1 and asign T/F randomly using 80/20 split
dataset1$splt<- sample.split(dataset1$Revenue, SplitRatio= 0.8)

# Get training data which include 80% of split column equal to True
trainData<- subset(dataset1, dataset1$splt == TRUE)

# Get test data 
testData<- subset(dataset1, dataset1$splt == FALSE)

# Verify split
cat("Train:", nrow(trainData), "| Test;", nrow(testData),
    "| Split:", floor(nrow(trainData)/nrow(dataset1)*100), "/",
    ceiling(nrow(testData)/nrow(dataset1)*100)
    )

# Task 3: Model fitting ----
# Fit Logistic regression model
logitModel<- glm(Revenue ~ PageValues + ProductRelated_Duration +
                   ExitRates + BounceRates + ProductRelated,
                 data= trainData,
                 family= binomial(link= "logit"))

# model summary
summary(logitModel)

# refit the model without the statistically insignificant predictors
logitModel1<- glm(Revenue ~ PageValues +
                   ExitRates + ProductRelated,
                 data= trainData,
                 family= binomial(link= "logit"))

# model summary
summary(logitModel1)

# Display coefficients in a table
coeffTable<- data.frame(
  Predictor= names(coef(logitModel1)),
  Coefficient= coef(logitModel1),
  OddsRatio= exp(coef(logitModel1))
)

rownames(coeffTable)<- NULL # removes row names

# Display coefficients table
coeffTable %>%
  knitr::kable(
    caption = "Model Coefficients and Odds Ratios") %>%
  kableExtra::kable_classic_2(
    html_font = "Calibri",
    position = "center",
    font_size = 12,
    full_width = FALSE
  )%>%
  kableExtra::row_spec(0, bold = TRUE, background = "#E8F4F8")

# Task4: Confusion Matrix for Training set ----
# Predict on training data
trainPredProb <- predict(logitModel1, type = "response")
trainPredClass <- ifelse(trainPredProb >= 0.5, "TRUE", "FALSE")
trainPredClass <- factor(trainPredClass, levels = c("FALSE", "TRUE"))

# Convert Revenue from logical to factor with same levels
trainData$Revenue <- factor(trainData$Revenue, levels = c(FALSE, TRUE))

# Create a confusion matrix
trainConfusionMatrix <- confusionMatrix(trainPredClass, trainData$Revenue, positive = "TRUE")
print(trainConfusionMatrix)

# Task 5: Confusion Matrix for test Set ----
# Predict on Test Data
testPredProb <- predict(logitModel1, newdata = testData, type = "response")
testPredClass <- ifelse(testPredProb >= 0.5, "TRUE", "FALSE")
testPredClass <- factor(testPredClass, levels = c("FALSE", "TRUE"))

# Convert Revenue from logical to factor with same levels
testData$Revenue <- factor(testData$Revenue, levels = c(FALSE, TRUE))

# Create a confusion matrix
testConfusionMatrix <- confusionMatrix(testPredClass, testData$Revenue, positive = "TRUE")
print(testConfusionMatrix)

# Task 6: ROC Curve ----
# Create ROC object
rocObject <- roc(testData$Revenue, testPredProb)

# Plot ROC curve
plot(rocObject,
     main = "ROC: Classifying Revenue-Generating Sessions",
     col = "blue",
     lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)"
)

# Task 7: Calculate AUC ----
aucValue <- auc(rocObject)

# Show AUC  
aucValue


# Task8: Hypothesis Test (Chi-Square Test of Independence) ----
# Testing whether Revenue generation is independent of Month
# H0: Revenue generation is independent of Month
# H1: Revenue generation is dependent on Month

# Create contingency table from dataset
contingencyTable <- table(dataset1$Month, dataset1$Revenue)

# Display the contingency table 
contingencyTable %>%
  knitr::kable(
    caption = "Contingency Table: Month vs Revenue Generation",
    col.names = c("No Purchase", "Purchase Made")
  ) %>%
  kableExtra::kable_classic_2(
    html_font = "Calibri",
    position = "center",
    font_size = 12,
    full_width = FALSE
  ) %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#E8F4F8")

# Convert table to matrix for calculations
observed <- as.matrix(contingencyTable)

# Compute marginal totals needed for calculating expected frequencies
rowTotal <- rowSums(observed)  # total sessions for each month
colTotal <- colSums(observed)  # total sessions for each revenue category
grandTotal <- sum(observed)    # total sessions

alpha <- 0.05  # significance level
r <- nrow(observed)  # number of rows (months)
c <- ncol(observed)  # number of columns (revenue categories)
df <- (r - 1) * (c - 1)  # degrees of freedom
df

# Compute expected frequencies assuming independence
expected <- outer(rowTotal, colTotal) / grandTotal

# Critical Value
criticalValue <- qchisq(1 - alpha, df)  # right tailed test CV
criticalValue

# Compute chi-square test statistic
chiStat <- sum((observed - expected)^2 / expected)
chiStat

# Decision
ifelse(chiStat > criticalValue, "Reject H0", "Fail to reject H0")