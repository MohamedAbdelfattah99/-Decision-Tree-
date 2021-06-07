library("rpart")
library("rpart.plot")
set.seed(102)

##########################################
# Training Data
##########################################
setwd("F:/mohamed/matrials/year(4,2)/ERP/labs/lab4")
cars<- read.table("car-dataset.csv",header=TRUE,sep=",")
##########################################
# dvide data into 80% for train and 20% for testing
##########################################
data_set <- sample.int(n = nrow(cars), size = floor(.80*nrow(cars)), replace = F)
train_cars <- cars[data_set, ]
test_cars  <- cars[-data_set, ]

fit <- rpart(Label ~ Feature1+ Feature2 + Feature3 + Feature4 + Feature5 + Feature6, 
             method="class", 
             data=train_cars,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))

rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)
##########################################
# predict the model 
##########################################
predict(fit,test_cars=test_cars,type=c("class"))

predict_test <-predict(fit, test_cars, type = 'class')
##########################################
# Calacuting model accracucy 
##########################################
accuracy_tabel <- table(test_cars$Label, predict_test)

accuracy_per <- sum(diag(accuracy_tabel)) / sum(accuracy_tabel)

print(paste('Accuracy for test', accuracy_per))
