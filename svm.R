# import svm library (use commandinstall.packages(“e1071”))
library("e1071")

# set random number seed (keeps results consistent every time)
set.seed(888)

# read in existing products
products <- read.csv("./existing product attributes.csv", header=TRUE, sep=",",
                      as.is=TRUE)
colnames(products) <- c("ProductType", "Product", "Price", "5StarReviews",
    "4StarReviews", "3StarReviews", "2StarReviews", "1StarReviews",
    "PositiveServiceReview", "NegativeServiceReview", "WouldConsumerRecommend",
    "BestSellersRank", "ShippingWeight", "ProductDepth", "ProductWidth",
    "ProductHeight", "ProfitMargin", "Volume")

# dropping features due to factor levels mismatch in test/train
products$BestSellersRank <- NULL
products$ProductType <- NULL

# shuffle data
products <- products[sample(nrow(products)),]

# create 10 equally sized folds
folds <- cut(seq(1,nrow(products)),breaks=10,labels=FALSE)

#
# example of how to manually perform cross-validation
#

for(i in 1:10){

    #Segement your data by fold using the which() function
    trainIndexes <- which(folds!=i,arr.ind=TRUE)
    trainData <- products[trainIndexes, ]
    testData <- products[-trainIndexes, ]

    # show run stats
    cat('==== Fold no.', i, '====\n')
    cat('Training set # of rows: ', nrow(trainData), '\n')
    cat('Testing set # of rows: ', nrow(testData), '\n\n')

    # train ze model
    model <- svm(Volume ~ ., data=trainData)
    print(summary(model))

    # make predictions, join the predicted with the actual vals
    predictions <- round(predict(model, testData), digits=0)
    comparison <- cbind(testData$Volume, predictions)
    colnames(comparison) <- c("actual", "predicted")

    # calculate absolute percent error and print to terminal
    comparison <- cbind(comparison,
                round(abs(comparison[,1]-comparison[,2])/comparison[,1]*100.0,
                digits=1))
    colnames(comparison)[3] <- "absPercentError"
    print(comparison)
    cat('\nMean abs percent err:',
      round(sum(comparison[,3]/nrow(comparison)),0.1), 'units\n\n')

}

#
# example of how to tune svm parameters with integrated cross-validation
#

# tune the svm
svm_tuned <- tune.svm(Volume ~ ., data=trainData, cost = 2^(-1:10),
  kernel = "linear", gamma=c(0.5, 1, 2))

cat('\n==== Tuned SVM best settings:\n')
print(svm_tuned)

# taking the output from the tuning step above and creating a final model
svm_tuned_model <- svm(Volume ~ ., data = trainData,
    kernel = "linear",
    gamma = svm_tuned$best.parameters$gamma,
    cost  = svm_tuned$best.parameters$cost);

cat('\n==== Final SVM model with best settings used:\n')
print(svm_tuned_model)

# make predictions, join the predicted with the actual vals
predictions_tuned <- round(predict(svm_tuned_model, testData), digits=0)
comparison_tuned <- cbind(testData$Volume, predictions_tuned)
colnames(comparison_tuned) <- c("actual", "predicted")

# calculate absolute percent error and print to terminal
comparison_tuned <- cbind(comparison_tuned,
            round(abs(comparison_tuned[,1]-comparison_tuned[,2])
            /comparison_tuned[,1]*100.0,
            digits=1))
colnames(comparison_tuned)[3] <- "absPercentError"
print(comparison_tuned)
cat('\nMean abs percent err of tuned model:',
  round(sum(comparison_tuned[,3]/nrow(comparison_tuned)),0.1), 'units\n\n')


#
# example of taking the tuned model and using it to predict new products
#

# read in new products
products_new <- read.csv("./new product attributes.csv", header=TRUE, sep=",",
                      as.is=TRUE)
colnames(products_new) <- c("ProductType", "Product", "Price", "5StarReviews",
    "4StarReviews", "3StarReviews", "2StarReviews", "1StarReviews",
    "PositiveServiceReview", "NegativeServiceReview", "WouldConsumerRecommend",
    "BestSellersRank", "ShippingWeight", "ProductDepth", "ProductWidth",
    "ProductHeight", "ProfitMargin", "Volume")

# dropping features due to factor levels mismatch in test/train
products_new$BestSellersRank <- NULL
products_new$ProductType <- NULL

# setting volume to num from chr (default import type), needed for svm
products_new$Volume <- 0

# make predictions on new products
predictions_tuned_new <- data.frame(round(predict(svm_tuned_model,
  products_new), digits=0.1))

# name the columns for easy reading
colnames(predictions_tuned_new) <- 'projected volume'

# view output
print(predictions_tuned_new)
