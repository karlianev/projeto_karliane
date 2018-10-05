source("utils.R")

if (!(grepl("flexcon_c/resultados", getwd(), fixed=TRUE))) {
  setwd("flexcon_c/resultados/")
}

type <- "Classifier"
typeAgg <- "ClassifierAgroup"

#' @description This class provide an easy way to store the p-values.
#'
#' @slot naive numeric vector.
#' @slot rpart numeric vector.
#' @slot JRip numeric vector.
#' @slot IBk numeric vector.
#'
Classifier <- setClass(
  "Classifier",
  slots = list(naive = "numeric", rpart = "numeric", JRip = "numeric",
               IBk ="numeric")
)

#' @description This class provide 2 objects of the Classifier Class
#'
#' @slot all an object of the Classifier Class
#' @slot mean an object of the Classifier Class
#'
ClassifierAgroup <- setClass(
  "ClassifierAgroup",
  slots = list(all = type, mean = type)
)

#' @description This class provide five objects of the ClassifierAgroup Class
#'
#' @slot tx05 an object of the ClassifierAgroup Class
#' @slot tx10 an object of the ClassifierAgroup Class
#' @slot tx15 an object of the ClassifierAgroup Class
#' @slot tx20 an object of the ClassifierAgroup Class
#' @slot tx25 an object of the ClassifierAgroup Class
#'
Rates <- setClass(
  "Rates",
  slots = list(tx05 = typeAgg, tx10 = typeAgg, tx15 = typeAgg, tx20 = typeAgg,
               tx25 = typeAgg)
)

#' @description Retrieves a list of the files where each file contains a pattern
#' into your name.
#'
#' @param pattern A pattern which must be used in the search.
#'
#' @return A list of these files.
#'
getFiles <- function(pattern) {
  return (list.files(pattern=pattern))
}

#' @description Set the p-values into an object.
#'
#' @param method The method to be used in pattern.
#'
#' @return An object of the ClassifierAgroup class.
#'
getClassifiersResultAllRates <- function(method) {
  obj1 <- ClassifierAgroup()
  classifiers <- slotNames(obj1@all)
  mode <- slotNames(obj1)
  for(i in classifiers) {
    files <- getFiles(join(c(method, i)))
    aux <- prepareDataAllRates(files)
    for(j in 1:length(mode)) {
      slot(slot(obj1, mode[j]), i) <- aux[[j]]
    }
  }
  return (obj1)
}

#' @description Set the p-values into an object.
#'
#' @param method The method to be used in pattern.
#'
#' @return An object of the Rates class.
getClassifiersResultOneRate <- function(method) {
  obj <- Rates()
  rates <- slotNames(obj)
  mode <- slotNames(obj@tx10)
  classifiers <- slotNames(obj@tx05@all)
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

getClassifiersResultTheMean <- function(method) {
  aux <- Classifier()
  classifiers <- slotNames(aux)
  for (i in classifiers) {
    files <- getFiles(join(c(method, i)))
    all <- prepareDataTheMean(files)
    name <- paste(join(c(method, "media", i)), ".csv", sep = "")
    writeArchive(name, all$mean)
    name <- paste(join(c(method, "desvio_padrao", i)), ".csv", sep = "")
    writeArchive(name, all$sd)
  }
}

#' @description This function calculates the mean value for each column in a
#' matrix where the rows are represented by the vector input and the culomns are
#' represented by the number of the folds in the k input and stores it into a
#' new vector.
#'
#' @param vec A vector containing all of the accuracies.
#' @param k An integer reference the number of folds in cross-validation.
#'
#' @return A vector in which every position contains the mean value of the
#' k-folds.
#'
getMeans <- function(vec, k = 10) {
  med <- matrix(vec, nrow = k)
  aux <- c()
  for(j in 1:(ncol(med))) {
    aux <- c(aux, mean(med[, j]))
  }
  return (aux)
}

#' Standard Deviation
getStDev <- function(vec, n) {
  std <- matrix(vec, nrow = n)
  aux <- c()
  for(j in 1:(ncol(std))) {
    aux <- c(aux, sd(std[, j]))
  }
  return (aux)
}

#' @description The main function in this script.
#'
main <- function() {
  method <- c("c1_S", "c1_V", "c2")
  ## Resultado por classificador utilizando todas as 5 taxas de inicialmente
  ## rotulados.
  tx_flexcon_c1_s <<- getClassifiersResultAllRates(method[1])
  tx_flexcon_c1_v <<- getClassifiersResultAllRates(method[2])
  tx_flexcon_c2 <<- getClassifiersResultAllRates(method[3])
  ## Resultados utilizando o mesmo percentual de inicialmente rotulados e
  ## variando o parâmetro cr.
  cr_flexcon_c1_s <<- getClassifiersResultOneRate(method[1])
  cr_flexcon_c1_v <<- getClassifiersResultOneRate(method[2])
  cr_flexcon_c2 <<- getClassifiersResultOneRate(method[3])
  ## Coletando média e desvio padrão
  getClassifiersResultTheMean(method[1])
  getClassifiersResultTheMean(method[2])
  getClassifiersResultTheMean(method[3])
}

#' @description Provide
prepareDataAllRates <- function(files) {
  pvalues <- c()
  pvalues_mean <- c()
  for(file in files) {
    result <- readDataAllRates(file)
    result_mean <- getMeans(result)
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
    result_mean <- getMeans(result)
    my_anova <- runAnova(result, tam)
    pvalues <- c(pvalues, my_anova$`Pr(>F)`[1])
    my_anova_mean <- runAnova(result_mean, tam/10)
    pvalues_mean <- c(pvalues_mean, my_anova$`Pr(>F)`[1])
  }
  return (list(pvalues, pvalues_mean))
}

prepareDataTheMean <- function(files) {
  agroup_means <- c()
  agroup_sd <- c()
  for (file in files) {
    result <- readDataAllRates(file)
    result_mean <- getMeans(result, 310)
    agroup_means <- c(agroup_means, result_mean)
    result_sd <- getStDev(result, 310)
    agroup_sd <- c(agroup_sd, result_sd)
  }
  mean <- matrix(agroup_means, ncol = 5, byrow = T)
  sd <- matrix(agroup_sd, ncol = 5, byrow = T)
  return (list(mean = mean, sd = sd))
}

#' @description Read and convert a file to a vector where each position references an
#' accuracy within the file
#'
#' @param file The csv file to be read.
#'
#' @return A vector where each position references a data of the csv file read.
readDataAllRates <- function(file) {
  y <- c()
  read <- readFile(file)
  for (col in colnames(read)) {
    y <- c(y, read[, col])
  }
  tam <<- length(y) / length(colnames(read))
  return (y)
}

#' @description Read a file and use only one column of it.
#'
#' @param files A vector contains all files names to be read.
#' @param rate The number of the column to be read.
#'
#' @return A vector with all files read in only one column.
readDataOneRate <- function(files, rate) {
  y <- c()
  for(file in files) {
    read <- readFile(file)
    y <- c(y, read[, colnames(read)[rate]])
  }
  tam <<- length(y) / length(files)
  return (y)
}

#' @description Run the anova function in the data
#'
#' @param result A vector data to
#' @param tam
#'
#' @return An anova object
#'
runAnova <- function(result, tam) {
  number_rep <- rep(tam, (length(result) / tam))
  groups <- rep(1:length(number_rep), number_rep)
  data <- data.frame(result = result, groups = factor(groups))
  fit <- lm(result ~ groups, data)
  return (anova(fit))
}

main()
