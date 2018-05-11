#coBC e/ou coBCG
#https://cran.r-project.org/web/packages/ssc/ssc.pdf
#https://www.rdocumentation.org/packages/ssc/versions/2.0.0/topics/coBCG
#testando o coBC
data(wine)
cls <- which(colnames(wine) == "Wine")
x <- wine[, -cls] # instances without classes
y <- wine[, cls] # the classes
x <- scale(x) # scale the attributes #centraliza ou dimensiona os atributos, não entendi
## Prepare data
set.seed(20)
# Use 50% of instances for training
tra.idx <- sample(x = length(y), size = ceiling(length(y) * 0.5))
xtrain <- x[tra.idx,] # training instances
ytrain <- y[tra.idx] # classes of training instances
# Use 70% of train instances as unlabeled set
tra.na.idx <- sample(x = length(tra.idx), size = ceiling(length(tra.idx) * 0.7))
ytrain[tra.na.idx] <- NA # remove class information of unlabeled instances
# Use the other 50% of instances for inductive testing
tst.idx <- setdiff(1:length(y), tra.idx) #o conjunto de teste pegando os exemplos diferentes dos que estão no treinamento
xitest <- x[tst.idx,] # testing instances
yitest <- y[tst.idx] # classes of testing instances
## Example: Training from a set of instances with 1-NN as base classifier.
set.seed(1)
m1 <- coBC(x = xtrain, y = ytrain,
           #learner('IBk',list(control = Weka_control(K=15, X=TRUE)))
           #learner = ssc::oneNN
           e1071::svm
           #learner = caret::knn3,
           
           #learner.pars = list(k = 1),
           #pred = "predict"
)

pred1 <- predict(m1, xitest)
table(pred1, yitest)


#ALTERADO
setwd("C:\\local_R\\projeto_karliane\\bases")
base_iris <- read.arff("iris.arff")
col <- (ncol(base_iris)-1)/2
xl <- base_iris[,1:ncol(base_iris)-1] #a base dados iris sem os rotulos
yl <- base_iris[-(1:ncol(base_iris)-1)] #rotulos da base iris
view <- partition.matrix(xl, rowsep = nrow(base_iris), colsep = c(col,col))
view1 <- data.frame(view$`1`$`1`, yl)
view2 <- data.frame(view$`1`$`2`,yl)

#xl<-base_iris[,1:4] #a base dados iris sem os rotulos
#Suppose we know the first twenty observations of each class
#and we want to predict the remaining with co-training
# 1 setosa, 2 versicolor, 3 virginica
yl<-rep(1:3,each=20) #armazena em yl os números 1,2,3 (classes) 20 vezes - são os rotulos conhecidos
known.label <-c(1:20,51:70,101:120) # id dos exemplos com rótulos conhecidos
xu<-xl[-known.label,] # exemplos com rotulos desconhecidos
xl<-xl[known.label,] # exemplos com rotulos conhecidos
yu<-sslCoTrain(xl,yl,xu,method1="xgb",nrounds1 = 100,method2="xgb",nrounds2 = 100,n=60)

#ORIGINAL

data(iris) 
xl<-iris[,1:4] #a base dados iris sem os rotulos
#Suppose we know the first twenty observations of each class
#and we want to predict the remaining with co-training
# 1 setosa, 2 versicolor, 3 virginica
yl<-rep(1:3,each=20) #armazena em yl os números 1,2,3 (classes) 20 vezes - são os rotulos conhecidos
known.label <-c(1:20,51:70,101:120) # id dos exemplos com rótulos conhecidos
xu<-xl[-known.label,] # exemplos com rotulos desconhecidos
xl<-xl[known.label,] # exemplos com rotulos conhecidos
yu<-sslCoTrain(xl,yl,xu,method1="xgb",nrounds1 = 100,method2="xgb",nrounds2 = 100,n=60)


