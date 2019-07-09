data <- read.csv("advertising.csv", header=T)

df <- data.frame(data)
summary(df)

# Drop index column
df <- subset(df, select = -X)

# Exploratory plots
pairs(~.,data=df, main="Scatterplot Matrix")
# plots of tv/radio/newspaper show random scatter -> no correlation
# sales vs TV/radio/newspaper shows linear relationship
# sales vs newspaper shows fan-shape -> indicates heteroskedasticity 

# Create train and test sets (60/40 split)
sample_size <- floor(0.6 * nrow(df))
set.seed(100)
train_index <- sample(seq_len(nrow(df)), size = sample_size)

train <- df[train_index, ]
test <- df[-train_index, ]

# Choose a linear model using backward elimination
# Fit model with all explanatory variables                      
fullmodel <- lm(sales~.,data=df)
summary(fullmodel)

# Drop newspaper
updatedmodel <- update(fullmodel, .~.-newspaper)
summary(updatedmodel)

# Choose a linear model using all subsets regression
library(leaps)
best.subset <-regsubsets(sales~., data=df, nbest=1)
summary(best.subset)

# Compare C_p values
summary(best.subset)$cp
which.max(summary(best.subset)$cp)

# Compare adjusted R^2 values
summary(best.subset)$adjr2
which.max(summary(best.subset)$adjr2)

# Plot C_p and adjusted R^2 values
par(mfrow=c(1,2))
plot(2:4, summary(best.subset)$cp, xlab="No. of Parameters",
     ylab="Cp statistic", col="blue")
abline(0,1)
plot(2:4, summary(best.subset)$adjr2, xlab="No. of Parameters",
     ylab="Adjusted R-squared", col="blue")

# From Cp values and adjusted R2 values, the 2 explanatory variable model looks best (TV and radio)

# Make predictions for test data
predicted <- predict(updatedmodel, test)
prediction_results <- data.frame(cbind(actual=test$sales, predicted=predicted))  
correlation_accuracy <- cor(prediction_results) # 93.5%
