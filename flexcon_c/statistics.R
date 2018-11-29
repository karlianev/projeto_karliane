if (!(grepl("flexcon_c/teste_estatistico", getwd(), fixed=TRUE))) {
  setwd("flexcon_c/teste_estatistico/")
}

source("../utils.R")

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
#' @param path A path which the files be listed
#' @param pattern A pattern which must be used in the search.
#' @param full.names Relative name for the actual dir
#'
#' @return A list of these files.
#'
getFiles <- function(pattern, path = ".", names = F) {
  return (list.files(path, pattern = pattern, full.names = names))
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
    name <- paste(join(c("../mean_sd/", method, "media", i)), ".csv", sep = "")
    writeArchive(name, round(t(all$mean), 2))
    name <- paste(join(c("../mean_sd/", method, "desvio_padrao", i)), ".csv", sep = "")
    writeArchive(name, round(t(all$sd), 2))
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
  # getClassifiersResultTheMean(method[1])
  # getClassifiersResultTheMean(method[2])
  # getClassifiersResultTheMean(method[3])
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

#' @description Read and convert a file to a vector where each position
#' references an accuracy within the file
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
  data <<- data.frame(result = result, groups = factor(groups))
  fit <- lm(result ~ groups, data)
  return (aov(fit))
}

# main()

method <- c("c1_S", "c1_V", "c2")
classifier <- c("naiveBayes", "rpartXse", "JRip", "IBk")
classificados <- c("Naïve Bayes", "rpartXse", "RIPPER", "k-NN")
metodos <- c("FlexCon-C1(s)", "FlexCon-C1(v)", "FlexCon-C2")
# type <- method[1]
# order <- c(5, 2)
# cr <- 2
# start_rates <- 1
# end_rates <- 5
# tx <- 1
#' Carrega os dados de um arquivo para uma variável para cada % de ini_rot
#' percorra os crs e carregue os dados referentes ao % de ini_rot
creatingDataTTest <- function(classifier, type, start_rates = 1, end_rates = 5,
                              order = c(2, 3, 4, 5, 6, 7, 8)) {
  all_data <- c()
  for (tx in start_rates:end_rates) {
    for (cr in order) {
      file <- getFiles(join(c(type, classifier, cr)), "../dados", names = T)
      all_data <- c(all_data, readFile(file)[, tx])
    }
  }
  return (all_data)
}

#' Testa se os dados são uma distribuição normal e grava no arquivo
normalDistribution <- function(data, classifier, method) {
  crs <- c(5, 2, 3, 4, 6, 7, 8)
  all_rates <- c()
  for (i in 1:7) {
    one_rates <- c()
    for (j in 1:5) {
      normal <- data[((j - 1) * 31 + 1):(31 * j), i]
      d <- shapiro.test(normal)
      one_rates <- c(one_rates, d$p.value)
    }
    title <- paste(join(c(classifier, method, "partial", "shapiro", "test")),
                   "csv", sep = ".")
    one_rates <- matrix(one_rates, ncol = 5, byrow = TRUE)
    writeArchive(title, one_rates, row = as.numeric(crs[i]))
    normal <- data[, i]
    d <- shapiro.test(normal)
    all_rates <- c(all_rates, d$p.value)
  }
  title <- paste(join(c(classifier, method, "final", "shapiro", "test")), "csv",
                 sep = ".")
  all_rates <- matrix(all_rates, ncol = 1, byrow = TRUE)
  writeArchive(title, all_rates, row = crs)
}

#' Teste estatístico de friedman utilizado dois a dois
#' em @param data[((j - 1) * 31 + 1):(31 * j), c(1, i)] 1 é pré-fixado em 5%
#' e o parametro i varia entre 2, 3, 4, 6, 7, 8
executeFriedmanTestByTwo <- function(data, classifier, method) {
  crs <- c(5, 2, 3, 4, 6, 7, 8)
  all_rates <- c()
  for (i in 2:7) {
    one_rates <- c()
    for(j in 1:5) {
      v <- data[((j - 1) * 31 + 1):(31 * j), c(1, i)]
      d <- friedman.test(v)
      one_rates <- c(one_rates, d$p.value)
    }
    title <- paste(join(c(classifier, method, "partial", "friedman", "test")),
                   "csv", sep = ".")
    one_rates <- matrix(one_rates, ncol = 5, byrow = TRUE)
    writeArchive(title, one_rates, row = as.numeric(crs[i]))
    normal <- data[, c(1, i)]
    d <- friedman.test(normal)
    all_rates <- c(all_rates, d$p.value)
  }
  title <- paste(join(c(classifier, method, "final", "friedman", "test")), "csv",
                 sep = ".")
  all_rates <- matrix(all_rates, ncol = 1, byrow = TRUE)
  writeArchive(title, all_rates, row = crs[2:7])
}

executeWicoxonTestByTwo <- function(data, classifier, method) {
  crs <- c(5, 2, 3, 4, 6, 7, 8)
  all_rates <- c()
  for (i in 2:7) {
    one_rates <- c()
    for(j in 1:5) {
      v <- data[((j - 1) * 31 + 1):(31 * j), c(1, i)]
      vec <- v[, 1] - v[, 2]
      d <- wilcox.test(vec)
      one_rates <- c(one_rates, d$p.value)
    }
    title <- paste(join(c(classifier, method, "partial", "Wilcoxon", "test")),
                   "csv", sep = ".")
    one_rates <- matrix(one_rates, ncol = 5, byrow = TRUE)
    writeArchive(title, one_rates, row = as.numeric(crs[i]))
    normal <- data[, c(1, i)]
    vec <- normal[, 1] - normal[, 2]
    d <- wilcox.test(vec)
    all_rates <- c(all_rates, d$p.value)
  }
  title <- paste(join(c(classifier, method, "final", "Wilcoxon", "test")), "csv",
                 sep = ".")
  all_rates <- matrix(all_rates, ncol = 1, byrow = TRUE)
  writeArchive(title, all_rates, row = crs[2:7])
}

for (cl in 1:length(classifier)) {
  for (meth in 1:length(method)) {
    data <- creatingDataTTest(classifier[cl], method[meth])
    data2 <- c()
    for (i in 1:35) {
      data2 <- c(data2, getMeans(data[((i - 1) * 310 + 1):(310 * i)]))
    }
    matrix <- matrix(data2, nrow = 31)
    for(j in 1:5) {
      v <- matrix[, ((j - 1) * 7 + 1):(7 * j)]
      rownames(v) <- c("iris", "bupa", "segment", "waveform-5000",
                            "phishingData", "haberman", "mushroom", "pima",
                            "vehicle", "wilt", "kr-vs-kp",
                            "blood-transfusion-service", "cnae-9",
                            "connectionist-mines-vs-rocks", "flare",
                            "indian-liver-patient", "leukemia-haslinger",
                            "mammographic-mass", "mfeat-karhunen", "musk",
                            "ozone-onehr", "pendigits", "planning-relax",
                            "seeds", "semeion", "spectf-heart", "tic-tac-toe",
                            "twonorm", "hill-valley-with-noise",
                            "balance-scale", "car")
      colnames(v) <- rep(2:8)
      title <- paste(classifier[cl], metodos[meth], sep = "_")
      writeArchive(paste("../", title, ".csv", sep = ""), round(v, 2),
                   row = rownames(v), col = colnames(v),  sep = "&")
    }
    # cd <- matrix(h, ncol = 7, byrow = T)
    # colnames(cd) <- c(2:8)
    # if ((classifier[cl] == "naiveBayes") || (classifier[cl] == "JRip")) {
    #   plotClassifierMethod(t = round(cd, 2), nome = classificados[cl],
    #                        "FlexCon-C")
    # } else if ((classifier[cl] == "IBk") && ((meth == 1) || (meth == 2))) {
    #   plotClassifierMethod(t = round(cd, 2), nome = classificados[cl], "FlexCon-C1")
    # 
    # }
    # plotClassifierMethod(t = round(cd, 2), nome = classificados[cl],
    #                      metodo = metodos[meth])
    # # new_matrix <- converter(matrix)
    # # colnames(new_matrix) <- c(2:8)
    # # normalDistribution(new_matrix, cl, meth)
    # # executeFriedmanTestByTwo(new_matrix, cl, meth)
    # # executeWicoxonTestByTwo(new_matrix, cl, meth)
  }
}

converter <- function(matrix) {
  base <- c(1, 8, 15, 22, 29)
  indices <- c(base + 3, base, base + 1, base + 2, base + 4, base + 5, base + 6)
  new_matrix <- matrix(matrix[, indices], nrow = 155)
  return (new_matrix)
}

teste <- function(v, matrix) {
  d <- c()
  for (i in 1:ncol(v)) {
    c <- mean(v[, i])
    d <- c(d, c)
  }
  return (d)
}

# plot(c(5, 2, 3, 4, 6, 7, 8), d)

# x <- c(564, 521, 495, 564, 560, 481, 545, 478, 580, 484, 539, 467)
# 
# y <- c(557, 505, 465, 562, 545, 448, 531, 458, 562, 485, 520, 445)
# 
# diff <- x - y
# 
# wilcox.test(diff)

plotClassifierMethod <- function(t, nome, metodo) {
  png(file = paste(paste(nome, metodo, sep = "_"), "png", sep = "."))
  g_range <- range(min(t), max(t))
  plot(t[1, ], type = "o", col = 1, ylim = g_range, axes = F, ann = F)
  axis(1, at = 1:7, lab = 2:8)
  axis(2, las = 1, at = round(t, 1))
  box()
  lines(t[2, ], type = "o", pch = 22, lty = 2, col = 2)
  lines(t[3, ], type = "o", pch = 23, lty = 3, col = 3)
  lines(t[4, ], type = "o", pch = 24, lty = 4, col = 4)
  lines(t[5, ], type = "o", pch = 25, lty = 5, col = 6)
  title(main = paste("Resultados do", nome, "e", metodo), col.main = "black",
        font.main = 2)
  title(xlab = "Variável cr", col.lab = rgb(0, 0, 0))
  title(ylab = "Acurácia", col.lab = rgb(0, 0, 0))
  legend(6, g_range[2], c("5%", "10%", "15%", "20%", "25%"), cex = 0.5,
         col = c(1:4,6), pch = c(21:25), lty = c(1:5))
  dev.off()
}

