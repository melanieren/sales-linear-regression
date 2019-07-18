data <- read.csv("advertising.csv", header=T)

df <- data.frame(data)
summary(df)

# Drop index column
df <- subset(df, select = -X)

# Create train and test sets (70/30 split)
sample_size <- floor(0.7 * nrow(df))
set.seed(100)
train_index <- sample(seq_len(nrow(df)), size = sample_size)

train <- df[train_index, ]
test <- df[-train_index, ]

# Exploratory plots
pairs(~.,data=train, main="Scatterplot Matrix")
# plots of tv/radio/newspaper show random scatter -> no correlation
# sales vs TV/radio/newspaper shows linear relationship -> let's try using a linear model
# sales vs newspaper and sales vs TV show fan-shape -> indicates heteroskedasticity 

# Selecting a linear model
# Choose a linear model using backward elimination
# Fit model with all explanatory variables                      
fullmodel <- lm(sales~.,data=df)
summary(fullmodel)

# Drop newspaper
updatedmodel <- update(fullmodel, .~.-newspaper)
summary(updatedmodel)

# All explanatory variables have p-value > alpha, so we stop backward elimination here

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
# This matches the linear model chosen using backwards elimination

# Visually assess how well the linear model fits the data
plot(updatedmodel)
# Residuals vs fitted plots shows quadratic relationship -> indicates non-linearity (need higher order polynomial terms)
#   or other explanatory variables are not captured in the model
# QQ-plot is S-shaped -> indicates residuals are not normally distributed 

# Make predictions for test data
predicted <- predict(updatedmodel, test)
prediction_results <- data.frame(cbind(actual=test$sales, predicted=predicted))  
correlation_accuracy <- cor(prediction_results) # 93.5%

# Adding interaction 
# The Residuals vs fitted plot had a quadratic relationship -> could be result of explanatory variables
#   not captured in the model
# Try adding interaction between the two explanatory variables and see if it is significant
model_with_interaction <- lm(sales ~ TV + radio + TV * radio, data = train)
summary(model_with_interaction)
# p-value of TV:radio interaction is less than alpha -> interaction is significant given other variables
#   in the model

# Visually assess how well the linear model with interaction fits the data
plot(model_with_interaction)
# Residuals vs fitted plot shows less of a quadratic relation than model without interaction
# QQ-plot is U-shaped -> transformation of response may help with normality of residuals

# Make new predictions
predicted2 <- predict(model_with_interaction, test)
prediction_results2 <- data.frame(cbind(actual=test$sales, predicted=predicted2))  
correlation_accuracy2 <- cor(prediction_results2) # 97.8%




