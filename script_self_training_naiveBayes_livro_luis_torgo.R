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
func <- function(m, d){
  p <- predict(m, d, type = "raw")
  data.frame(c1=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max))
}
nbSTbase <- naiveBayes(Species ~ ., trST[-nas,])
table(predict(nbSTbase, ts), ts$Species)
nbST <- SelfTrain(Species ~ ., trST, learner("naiveBayes", list()), "func")
table(predict(nbST, ts), ts$Species)





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
 