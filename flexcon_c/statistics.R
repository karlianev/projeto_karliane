if (!(grepl("flexcon_c/resultados", getwd(), fixed=TRUE))) {
  setwd("flexcon_c/resultados/")
}
 
#' @description this function do a mean for each row in the matrix and storage it into a vector.
#' 
#' @param vec A vector with all accuracies.
#' 
#' @return a vector where each position contains the mean to the 10 accuracies.
catMedias <- function(vec) {
  med <- matrix(vec, nrow = 10)
  aux <- c()
  for(j in 1:(ncol(med))) {
    aux <- c(aux, mean(med[, j]))
  }
  return (aux)
}

type <- "classifierClass"
typeAgg <- "classifierAgroupClass"

classifierClass <- setClass(
  "classifierClass",
  slots = list(naive = "numeric", rpart = "numeric", JRip = "numeric", IBk = "numeric")
)

classifierAgroupClass <- setClass(
  "classifierAgroupClass",
  representation = list(all = type, mean = type)
  #prototype = list(all = new("classifierClass"), mean = new("classifierClass"))
)

ratesClass <- setClass(
  "ratesClass",
  slots = list(tx5 = typeAgg, tx10 = typeAgg, tx15 = typeAgg, tx20 = typeAgg, tx25 = typeAgg)
)

#' @description Provide a list of the files where each file contains a pattern in the name.
#' 
#' @param pattern a pattern which be used in the search.
#' 
#' @return a list of these files.
getFiles <- function(pattern) {
  return (list.files(pattern=pattern))
}

#' @description 
#' 
#' @param method
#' 
#' @return An object of the classifier class contains the p-values for each classifier
getClassifiersResultAllRates <- function(method) {
  obj1 <- classifierAgroupClass()
  for(i in slotNames(obj1@all)) {
    files <- getFiles(join(c(method, i)))
    aux <- prepareDataAllRates(files)
    for(j in 1:length(slotNames(obj1))) {
      slot(slot(obj1, slotNames(obj1)[j]), i) <- aux[[j]]
    }
  }
  return (obj1)
}

getClassifiersResultOneRate <- function(method) {
  obj <- ratesClass()
  rates <- slotNames(obj)
  mode <- slotNames(obj@tx10)
  classifiers <- slotNames(obj@tx5@all)
    for(i in classifiers) {
      files <- getFiles(join(c(method, i)))
      aux <- prepareDataOneRate(files)
      for(j in 1:length(mode)) {
        for(k in 1:length(rates)) {
          slot(slot(slot(obj, rates[k]), mode[j]), i) <- aux[[j]][k]
        }
      }
    }
  return (obj)
}

main <- function() {
  method <- c("c1_S", "c1_V", "c2")
  
  ## Resultado por classificador utilizando todas as 5 taxas de inicialmente rotulados.
  tx_flexcon_c1_s <<- getClassifiersResultAllRates(method[1])
  tx_flexcon_c1_v <<- getClassifiersResultAllRates(method[2])
  tx_flexcon_c2 <<- getClassifiersResultAllRates(method[3])

  ## Resultados utilizando o mesmo percentual de inicialmente rotulados e variando o parâmetro cr
  cr_flexcon_c1_s <<- getClassifiersResultOneRate(method[1])
  cr_flexcon_c1_v <<- getClassifiersResultOneRate(method[2])
  cr_flexcon_c2 <<- getClassifiersResultOneRate(method[3])
  
  ## 
}

#' @description Provide a way to paste 2 or more words.
#' 
#' @param vec the words to be pasted in the order.
#' 
#' @return a string with the vec words collapsed in _.
join <- function(vec) {
  return (paste(vec[1:length(vec)], collapse = "_"))
}

#' @description read and convert a file to a vector where each column reference a % of the samples with stay with the class atribute
#' 
#' @param file csv file to be read
#' 
#' @return a vector where each possition reference a data of the csv file readed. 
readDataAllRates <- function(file) {
  y <- c()
  read <- read.csv(file)
  for (col in colnames(read)) { 
   y <- c(y, read[, col])
  }
  tam <<- length(y) / length(colnames(read))
  return (y)
}

readDataOneRate <- function(files, rate) {
  y <- c()
  for(file in files) {
    read <- read.csv(file)
    y <- c(y, read[, colnames(read)[rate]])
  }
  tam <<- length(y) / length(files)
  return (y)
}

replie <- function(number, replies) {
  return (rep(number, replies))
}

prepareDataAllRates <- function(files) {
  pvalues <- c()
  pvalues_mean <- c()
  for(file in files) {
    result <- readDataAllRates(file)
    result_mean <- catMedias(result)
    my_anova <- runAnova(result, tam)
    pvalues <- c(pvalues, my_anova$`Pr(>F)`[1])
    my_anova_mean <- runAnova(result_mean, tam/10)
    pvalues_mean <- c(pvalues_mean, my_anova$`Pr(>F)`[1])
  }
  return (list(pvalues, pvalues_mean))
}

prepareDataOneRate <- function(files) {
  pvalues <- c()
  pvalues_mean <- c()
  for (i in 1:5) {
    result <- readDataOneRate(files, i)
    result_mean <- catMedias(result)
    my_anova <- runAnova(result, tam)
    pvalues <- c(pvalues, my_anova$`Pr(>F)`[1])
    my_anova_mean <- runAnova(result_mean, tam/10)
    pvalues_mean <- c(pvalues_mean, my_anova$`Pr(>F)`[1])
  }
  return (list(pvalues, pvalues_mean))
}

runAnova <- function(result, tam) {
  number_replies <- replie(tam, (length(result) / tam))
  groups <- replie(1:length(number_replies), number_replies)
  data <- data.frame(result = result, groups = factor(groups))
  fit <- lm(result ~ groups, data)
  return (anova(fit))
}

main()

# h <- new("classClass") 
# aux2 <- new("classifierClass")
# aux2@naive <- c(1,2,3,4)
# aux2@rpart <- c(5,6,7,8)
# aux3 <- new("classifierClass")
# 
# 
# 
# 
# 
# slot(slot(obj, slotNames(obj)[j]), i)
# 
# 
# 
# 

# tx_05 <- catMedias(y1)
# tx_10 <- catMedias(y2)
# tx_15 <- catMedias(y3)
# tx_20 <- catMedias(y4)
# tx_25 <- catMedias(y5)
# 
# 
# tx <- c(tx_05, tx_10, tx_15, tx_20, tx_25)


# medias <- c(mean(y1), mean(y2), mean(y3), mean(y4))
# max(medias) - min(medias)
# 
# n <- rep(length(y1), (length(y) / length(y1)))
# 
# group <- rep(1:length(n), n)
# 
# data <- data.frame(y = y, group = factor(group))
# 
# fit <- lm(y~group, data)
# 
# anova(fit)
# 
# data.aov <- aov(formula = y ~ group, data = data)
# 
# summary(data$y)
# 
# summary(fit)
# 
# summary.aov(fit)
# 
# boxplot(y1, y2, y3, y4)
# 
# plot(y1, type = "l")
# 
# lines(lowess(y1), col = 4)
# 
# var.test(y1, y4)
# wilcox.test(y1, y4)
# ks.test(y1, y4)
# t.test(y1, y4)
# t.test(y1, y4, var.equal=TRUE)

# z1 <- c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1)
# 
# z2 <- c(17.4, 18.7, 19.1, 16.4, 15.9, 18.4, 17.7)
# 
# z3 <- c(15.2, 18.8, 17.7, 16.5, 15.9, 17.1, 16.7)
# 
# z <- c(z1, z2, z3)
#
# tmp = tapply(y, group, stem)
# tmp = tapply(z, grupos, stem)
#
# tmpfn <- function(x){
#   c(sum = sum(x), mean = mean(x), var = var(x), n = length(x))
# }
#
# tapply(y, group, tmpfn)
#
# tmpfn(y) 
# num <- rep(length(z1), (length(z)/length(z1)))
# 
# grupos <- rep(1:length(num), num)
# 
# dados <- data.frame(z = z, grupos = factor(grupos))
# 
# ajuste <-lm(z ~ grupos, dados)
# 
# minha_anova <- anova(ajuste)
# 
# summary(minha_anova)
# 
# minha_anova$`Pr(>F)`
