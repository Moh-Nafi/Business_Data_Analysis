library(rpart)
library(rpart.plot)


library(caret)

ebay.df <- read.csv("eBayAuctions.csv", stringsAsFactors = TRUE)

View(ebay.df)

# Answering quetion 1
set.seed(29)  
train.index <- sample(1:nrow(ebay.df), nrow(ebay.df)*0.65)  
train.df <- ebay.df[train.index, ]
valid.df <- ebay.df[-train.index, ]

# Answering quetion 2
# Default classification tree, optimized by pruning
default.ct <- rpart(Competitive ~ ., data = train.df, method = "class")

rpart.plot(default.ct, extra = 1)

# confusion matrix for validation
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred, factor(valid.df$Competitive))

# Answering to question 3
# confusion matrix for training
# First find the prediction for training data points
default.train.pred <- predict(default.ct, train.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(default.train.pred, factor(train.df$Competitive))


# QUESTION 4 : Is this model practical for predicting the outcome of a new auction?
# Answer: No, closeing price is not available at the time of prediction. Close price is only avalaible after the auction is closed, at which time it is already known whether the auction involved more than 2 bids.

# QUESTION 5: Fit another default classification tree. This time only use the predictors that can be used for predicting the outcome of a new auction.
# select predictors 
selected.train.df <- train.df[, c(1, 2, 3, 4, 6, 7)]
tr <- rpart(Competitive ~ ., data = selected.train.df, method = "class")


rpart.plot(tr, extra = 1)

# Question 6

# You can go down the tree to manually find the prediction for the new data
# Or you can use the following codes
# In order to make sure that the columns names are exactly the same as those in our
# training data, you can first use names(train.df) before creating the following dataframee
new.df <- data.frame(Category = "Books", currency = "US", sellerRating = 3211,
                     Duration = 7, OpenPrice = 3.99)
pred <- predict(tr, new.df, type = "class")
pred
# the prediction will be competitive

# Question 7
tr$variable.importance
