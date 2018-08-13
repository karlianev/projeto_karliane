# percent <- percentageOfClasses(labels)
# TODO Como pegar o id verdadeiro dos exemplos?

crossValidation <- function(database, all_labels, k = 10){
  if (k <= 0){
    cat("K need to be higger than 1")
  } else {
    val <- c()
    old_samples <- c()
    samples <- getLength(all_labels)
    samples_per_fold <- samples / k
    samples_per_class <- distSamples(all_labels)
    for (i in 1:length(levels(labels))) {
      val[i] <- samples_per_class[i] / k
    }
    for (i in 1:k) {
      selected_samples <- c()
      for (j in 1:length(levels(all_labels))) {
        selected_samples <- c(selected_samples, sample(which(all_labels[- old_samples] == levels(all_labels)[j]), val[j]))
      }
      old_samples <- c(old_samples, selected_samples)
    }
  }
}

percentageOfClasses <- function(labels){
  percent <- c()
  y <- NROW(labels)
  for(class in levels(labels)) {
    x <- length(which(labels == class))
    percent <- c(percent, ((x / y) * 100))
  }
  return (percent)
}


distSamples <- function(labels){
  x <- c()
  percent <- c()
  y <- NROW(labels)
  for(class in levels(labels)) {
    x <- c(x, length(which(labels == class)))
  }
  return (x)
}

percentageOfClasses(base_original$class)

distSamples(base_original$class)
distSamples(iris$Species)

all_labels <- iris$Species
database <- iris
