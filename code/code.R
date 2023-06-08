# import library
library(regclass)

# importing the dataset from regclass
data("ACCOUNT")

# Summarizing the data
data <- ACCOUNT[sample(nrow(ACCOUNT), size=500), ]
write.csv(data, file = 'data.csv', row.names = TRUE)

# Quick summary of the range of age in the dataset
summary(data$Age)

# We want to see the relationship between purchase and Area.Classification
# Change the level names
levels(data$Area.Classification) <- c("Rural", "Suburban", "Urban")

# Create a frequency table
table(data$Purchase, data$Area.Classification)
table(data$Area.Classification, data$Purchase)

# Create a relative frequency table
table(data$Area.Classification, data$Purchase)/nrow(data)

# Create a contingency table
tab <- table(data$Area.Classification, data$Purchase)
addmargins(tab)



# Create a bar plot for the categorical variables
plot(data$Purchase, main="Purchase", col=c('gray', 'green'),)
plot(data$Area.Classification, main="Area Classification", col=c('green', 'gray', 'orange'))

# Create a segmented bar chart
segmented_barchart(ACCOUNT$Area.Classification)
segmented_barchart(ACCOUNT$Purchase)

# Create a vector of colors for each bar
colors <- c("green", "gray", "orange")

# Create a stacked bar chart of Area Classification and Purchase
barplot(table(data$Area.Classification, data$Purchase), col = colors,
        main = "Stacked Bar Chart of Purchase",
        legend = rownames(table(data$Area.Classification, data$Purchase)))

# Create a mosaic plot
purchase <- data$Purchase
areas <- data$Area.Classification
plot(x=purchase, y=areas, xlab="Purchase",
     ylab="Area Classifiers", main="Mosaic Plot", col=rainbow(4))


# The expected table will be the sum of row * sum of column / total sample size
result <- chisq.test(tab)
result$observed
result$expected
result

# P-Value Chi-Squared Test
1-pchisq(3.9353, 2)

associate(data$Purchase~data$Area.Classification, data = data, permutations = 5000, seed = 42)

# Check nulls
ACCOUNT[is.na(ACCOUNT)]
ACCOUNT

# The next testing for categorical and quantitative data

# Independent Variable
table(data$Purchase)

# Dependent Variable
table(data$Income)

# Histogram
?hist()
hist(data$Income, xlab = "Income", ylab = "Frequency", main = "Income Distribution", col = "skyblue")

# QQ-Plot
?qq()
qq(data$Income, ax = "Income")
qqnorm(data$Income, col="skyblue")
qqline(data$Income, col = "red", lwd = 1)


# Box plot
?boxplot
boxplot(Income~Purchase, data = data, xlab = "Purchase", ylab = "Income", main = "Income and Purchase", col=c("skyblue", "green"))

# Associate
associate(Income~Purchase, data=data, permutations = 1000)

# Median test
install.packages("RVAideMemoire")
library(RVAideMemoire)

?mood.medtest()
mood.medtest(Income~Purchase, data=data)

med <- c(1:nrow(data))
for (i in 1:nrow(data)) {
  if (data$Income[i] > median(data$Income) )
  {med[i] ="Above"}
  else
  {med[i] ="Below"}
}
new_data <- cbind(data,med)
new_tab <- table(new_data$Purchase,new_data$med)
new_tab
addmargins(new_tab)
chisq.test(new_data$Purchase,new_data$med)

1-pchisq(0.13912, 1)

associate(Income~Purchase, data = data)

# Analysis 3
# Distribution of income
hist(data$Income*1000, col="green", xlab = "Income", main="Distribution of Income")

# Association between Checking and Saving accounts
plot(log10(data$Income), log10(data$SavingBalance), 
     xlab="Income", ylab="Saving Balance", main = "Income & Savings Balance (Log)", col="red")
  
plot(data$Income*1000, data$SavingBalance, 
     xlab="Income", ylab="Saving Balance", main = "Income & Savings Balance", col="red")

# Correlation
cor_matrix(data)

?associate()
associate(log10(SavingBalance)~log10(Income), data=data, permutations = 1000)

# Simple linear regression
