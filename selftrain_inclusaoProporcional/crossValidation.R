crossValidation <- function(database, all_labels, k = 10) {
  if (k <= 0) {
    stop("K need to be higger than 1")
  } else {
    aux <- c()
    folds <- c()
    id_names <- row.names(database)
    each_class <- samplesPerClass(all_labels, k)
    for (i in 1:k) {
      selected_samples <- c()
      for (j in 1:length(levels(all_labels))) {
        selected_samples <- c(selected_samples, sample(which(all_labels == levels(all_labels)[j]), each_class[j]))
      }
      folds <- c(folds, list(c(aux, as.integer(id_names[selected_samples]))))
      all_labels <- all_labels[- selected_samples]
      id_names <- id_names[- selected_samples]
    }
    return (folds)
  }
}

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

distSamples <- function(labels) {
  x <- c()
  percent <- c()
  y <- NROW(labels)
  for(class in levels(labels)) {
    x <- c(x, length(which(labels == class)))
  }
  return (x)
}