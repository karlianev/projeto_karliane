#' @description This function returns a list with k folds
#'
#' @usage crossValidation (database, all_labels, k = 10)
#'
#' @param database the database are you using without class column
#' @param all_labels the class column in the dataset
#' @param k the number of the folds to split the data
#'
#' @return a list with k sublists contains the samples
#'
#' @examples
#' data(iris)
#' data <- iris[, 1:(length(iris)-1)]
#' class <- iris$Species
#' folds <- crossValidation(data, class, k = 10)
crossValidation <- function(database, all_labels, k = 10) {
  if (k <= 0) {
    stop("K need to be higger than 0")
  } else {
    folds <- c()
    id_names <- row.names(database)
    each_class <- samplesPerClass(all_labels, k)
    for (i in 1:k) {
      selected_samples <- c()
      for (j in 1:length(levels(all_labels))) {
        range <- which(database[id_names, "class"] == levels(database$class)[j])
        selected_samples <- c(selected_samples, sample(range, each_class[j]))
      }
      folds <- c(folds, list(as.integer(id_names[selected_samples])))
      id_names <- id_names[- selected_samples]
    }
    if (length(id_names) != 0) {
      remainder <- c()
      for (lvls in levels(database$class)) {
        remainder <- c(remainder, list(sample(1:k, length(which(database$class[as.integer(id_names)] == lvls)))))
      }
      names(remainder) <- levels(database$class)
      for (label in names(remainder)) {
        if(length(remainder[[label]]) != 0) {
          for (k in remainder[[label]]) {
            content <- sample(id_names[which(database[id_names, "class"] == label)], 1)
            folds[[k]] <- c(folds[[k]], as.integer(content))
            id_names <- id_names[- which(id_names == content)]
          }
        }
      }
    }
    return (folds)
  }
}

#' @description This function counts the number of the samples per class in a
#'  vector
#'
#' @usage samplesPerClass (all_labels, k = 10)
#'
#' @param all_labels the class column in the dataset
#' @param k the number of the folds to split the data
#'
#' @return a vector with the number of the samples of each class than each fold
#'
#' @examples
#' data(iris)
#' class <- iris$Species
#' samples_each_class <- samplesPerClass(class, k = 10)
samplesPerClass <- function(all_labels, k) {
  vector <- c()
  samples_per_class <- distSamples(all_labels)
  for (i in 1:length(levels(all_labels))) {
    vector[i] <- samples_per_class[i] / k
  }
  return (vector)
}

percentageOfClasses <- function(labels) {
  percent <- c()
  y <- NROW(labels)
  for(class in levels(labels)) {
    x <- length(which(labels == class))
    percent <- c(percent, ((x / y) * 100))
  }
  return (percent)
}

#' @description This function counts the number of the samples per class in a
#' vector
#'
#' @usage distSamples (labels)
#'
#' @param labels the class column in the dataset
#'
#' @return a vector with the number of the samples of the each class
#'
#' @examples
#' data(iris)
#' class <- iris$Species
#' samples_each_class <- distSamples(class)
distSamples <- function(labels) {
  x <- c()
  percent <- c()
  y <- NROW(labels)
  for(class in levels(labels)) {
    x <- c(x, length(which(labels == class)))
  }
  return (x)
}