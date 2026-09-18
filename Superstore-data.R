install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("car")

# Superstore Statistical Analysis

# Purpose: Import and initially inspect the Superstore dataset


# Load required packages
library(tidyverse)
library(janitor)
library(skimr)
library(car)
# Import the dataset 
superstore <- read_csv("sample_superstore.csv")
head(superstore)
dim(superstore)
#Sturucture of dataset
str(superstore)
#summary of data
summary(superstore)
#overview of dataset
skim(superstore)
names(superstore)
#Data Cleaning and Pre-processing.
#Check for missing values
colSums(is.na(superstore))

# Check for duplicated rows
sum(duplicated(superstore))

# Check unique discount values
sort(unique(superstore$Discount))

# Check unique shipping modes
unique(superstore$Ship.Mode)

# Check unique customer segments
unique(superstore$Segment)

# Check unique product categories
unique(superstore$Category)
# Convert date variables to Date format
superstore$Order.Date <- as.Date(superstore$Order.Date, format = "%m/%d/%Y")
superstore$Ship.Date <- as.Date(superstore$Ship.Date, format = "%m/%d/%Y")

# Check the new variable types
str(superstore$Order.Date)
str(superstore$Ship.Date)
# Calculate shipping time in days
superstore$Shipping.Days <- as.numeric(
  superstore$Ship.Date - superstore$Order.Date
)

# Check shipping time
summary(superstore$Shipping.Days)

# Check for Invalid Values
sum(superstore$Sales < 0)
sum(superstore$Quantity < 0)
sum(superstore$Discount < 0 | superstore$Discount > 1)
sum(superstore$Shipping.Days < 0)
sum(superstore$Sales == 0)
sum(superstore$Profit == 0)

# Step 7: Missing Values and Duplicate Records

colSums(is.na(superstore))

sum(duplicated(superstore))

# Descriptive Statistics


summary(superstore$Profit)
summary(superstore$Discount)

cor(
  superstore$Discount,
  superstore$Profit,
  use = "complete.obs"
)

# Scatter Plot - Discount vs Profit

plot(
  superstore$Discount,
  superstore$Profit,
  main = "Relationship Between Discount and Profit",
  xlab = "Discount",
  ylab = "Profit",
  pch = 19,
  cex = 0.5
)


# Profit by Discount Level

aggregate(
  Profit ~ Discount,
  data = superstore,
  FUN = mean
)
#average profit for each discount level.
aggregate(
  Profit ~ Discount,
  data = superstore,
  FUN = median
)
#Box plot
boxplot(
  Profit ~ Discount,
  data = superstore,
  main = "Profit Distribution by Discount Level",
  xlab = "Discount",
  ylab = "Profit"
)
par(mar = c(4, 4, 2, 1))

# Multiple Linear Regression

model <- lm(
  Profit ~ Discount + Sales + Quantity,
  data = superstore
)

summary(model)
model <- lm(Profit ~ Discount + Sales + Quantity, data = superstore)
summary(model)

# Regression Diagnostics

par(mfrow = c(2, 2))
plot(model)
shapiro.test(sample(residuals(model),5000))
#Breusch-Pgan test
install.packages("lmtest")
library(lmtest)
bptest(model)
#Calculate Robust standard error
install.packages("sandwich")
library(sandwich)
library(lmtest)
coeftest(model, vcov = vcovHC(model, type = "HC3"))
vif(model)
install.packages("car")
library(car)
vif(model)
# Robust regression
library(sandwich)
library(lmtest)

robust_results <- coeftest(
  model,
  vcov = vcovHC(model, type = "HC3")
)

robust_results
# Confidence Interval(0.95)
coefci(model, vcov = vcovHC(model, type = "HC3"), level = 0.95)
library(car)
vif(model)
#structure of data
str(superstore)
#check for missing values
colSums(is.na(superstore))
# Descriptive Statisctics
summary(superstore[, c("Sales", "Quantity", "Discount", "Profit")])
#Scatter plot for Sales vs Profit
plot(superstore$Sales, superstore$Profit,
     main = "Sales vs Profit",
     xlab = "Sales",
     ylab = "Profit")
par(mar = c(4,4,2,1))
plot(superstore$Sales, superstore$Profit,
     main = "Sales vs Profit",
     xlab = "Sales",
     ylab = "Profit")
#Scatter Plot for Discount vs Profit
plot(superstore$Discount, superstore$Profit,
     main = "Discount vs Profit",
     xlab = "Discount",
     ylab = "Profit")
#Scatter plot for Quantity vs Profit
plot(superstore$Quantity, superstore$Profit,
     main = "Quantity vs Profit",
     xlab = "Quantity",
     ylab = "Profit")
#Correlation
cor(superstore[, c("Sales", "Discount", "Quantity", "Profit")],
    use = "complete.obs")
boxplot(superstore$Profit,
        main = "Boxplot of Profit",
        ylab = "Profit")
#summary
summary(superstore$Profit)
table(superstore$Category)
table(superstore$Region)
table(superstore$Segment)
#Average profits by categories
aggregate(Profit ~ Category, data = superstore, FUN = mean)
aggregate(Profit ~ Region, data = superstore, FUN = mean)
aggregate(Profit ~ Segment, data = superstore, FUN = mean)
#Total Average
aggregate(Profit ~ Category, data = superstore, FUN = sum)
#Total profit by Region
aggregate(Profit ~ Region, data = superstore, FUN = sum)
#Total profit by Customers' segment
aggregate(Profit ~ Segment, data = superstore, FUN = sum)
#comparing sales and profit using the categories
aggregate(cbind(Sales, Profit) ~ Category,
          data = superstore,
          FUN = sum)
#Profit_margin
category_summary <- aggregate(cbind(Sales, Profit) ~ Category,
                              data = superstore,
                              FUN = sum)

category_summary$Profit_Margin <- 
  (category_summary$Profit / category_summary$Sales) * 100

category_summary
#Finding large losses
head(superstore[order(superstore$Profit),
                c("Order.ID", "Product.Name", "Sales", "Discount", "Quantity", "Profit")], 10)
#Finding large profits
head(superstore[order(-superstore$Profit),
                c("Order.ID", "Product.Name", "Sales", "Discount", "Quantity", "Profit")], 10)
summary(model)
