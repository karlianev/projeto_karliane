library(DMwR)
library(e1071)
data(iris)
idx <- sample(150,100)
tr <- iris[idx,]
ts <- iris[-idx,]
#nb <- naiveBayes(Species ~ ., tr)
#table(predict(nb,ts), ts$Species)
trST <- tr
nas <- sample(100,90)
trST[nas, "Species"] <- NA

# the user should create a function, whose named should be given in the 
#parameter predFunc, that takes care of the classification of the currently 
#unlabelled cases, on each iteration. This function should be written so that
#it receives as FIRST ARGUMENT the learned classification model (with the 
#current training set), and a data frame with test cases in the SECOND ARGUMENT.
#This user-defined function should return a data frame with two columns and
#as many rows as there are rows in the given test set. The FIRST COLUMN of this
#data frame should contain the ASSIGNED CLASS LABELS by the provided 
#classification model, for the respective test case. The SECOND COLUMN should 
#contain the CONFIDENCE (a number between 0 and 1) associated to that classification. 
func <- function(m, d){
  p <- predict(m, d, type = "raw")
  data.frame(c1=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max))
}
nbSTbase <- naiveBayes(Species ~ ., trST[-nas,])
table(predict(nbSTbase, ts), ts$Species)
nbST <- SelfTrain(Species ~ ., trST, learner("naiveBayes", list()), "func")
table(predict(nbST, ts), ts$Species)

#fsvm <- function(m, d){
 # p <- predict(m, d, type="response")
  #data.frame(c1=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max))
#}

#svmSTbase <- svm(Species ~ ., data = trST[-nas,], method="C-classification")
#table(predict(svmSTbase, ts), ts$Species)
#svmST <- SelfTrain(Species ~ ., trST, learner("svm", list(cost=10,gamma=.1)), "fsvm")
#svmST <- SelfTrain(Species ~ ., trST, learner("svm"), "f1")
#table(predict(nbST, ts), ts$Species)

#knnST <- SelfTrain(Species ~ ., trST, learner("knn",list(k=10)), "func")


pred.nb <- function(m, d){
  p <- predict(m, d, type = 'raw')
  data.frame(c1=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max))
}

nb.st <- function(train, test){
  require(e1071, quietly = T)
  train <- train[,c('ID','Prod','Uprice','Insp')]
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp, level=c('ok','fraud'))
  model <- SelfTrain(Insp ~ ., train, learner('naiveBayes', list()),'pred.nb')
  preds <- predict(model, test[,c('ID','Prod','Uprice','Insp')], type='raw')
  return(list(rankOrder=order(preds[,'fraud'], decreasing=T), rankScore=preds[,'fraud'])) 
}

ho.nb.st <- function(form, train, test, ...){
  res <- nb.st(train, test)
  structure(evalOutlierRanking(test, res$rankOrder,...), itInfo=list(preds=res$rankScore, trues=ifelse(test$Insp=='fraud',1,0)))
}

nb.st.res <- holdOut(learner('ho.nb.st',
                             pars=list(Threshold=0.1)), #,statsProds=globalStats
                     dataset(Insp ~ .,sales),
                     hldSettings(3,0.3,1234,T),
                     itsInfo=TRUE)
summary(nb.st.res)



#Example run:
   library(DMwR)
   library(e1071)
   data(iris)
   idx <- sample(150,100)
   tr <- iris[idx,]
   ts <- iris[-idx,]
   nb <- naiveBayes(Species ~ .,tr)
   table(predict(nb,ts),ts$Species)
   trSS <- tr
   nas <- sample(100,50)
 trSS[nas,'Species'] <- NA
 func <- function(m,d) {
    p <- predict(m,d,type='raw')
    data.frame(cl=colnames(p)[apply(p,1,which.max)],p=apply(p,1,max))
 }
 nbSSbase <- naiveBayes(Species ~ .,trSS[-nas,])
 table(predict(nbSSbase,ts),ts$Species)
 nbSS <- SelfTrain(Species ~ .,trSS,learner('naiveBayes',list()),'func')
 table(predict(nbSS,ts),ts$Species)
 